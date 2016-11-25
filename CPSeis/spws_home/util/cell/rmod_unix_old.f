c  unix changes are delimited by the following character strings
C<license>
C-------------------------------------------------------------------------------
C Copyright (c) 2007 ConocoPhillips Company
C
C Permission is hereby granted, free of charge, to any person obtaining a copy
C of this software and associated documentation files (the "Software"), to deal
C in the Software without restriction, including without limitation the rights
C to use, copy, modify, merge, publish, distribute, sublicense, and/or sell
C copies of the Software, and to permit persons to whom the Software is
C furnished to do so, subject to the following conditions:
C
C The above copyright notice and this permission notice shall be included in all
C copies or substantial portions of the Software.
C
C THE SOFTWARE IS PROVIDED "AS IS", WITHOUT WARRANTY OF ANY KIND, EXPRESS OR
C IMPLIED, INCLUDING BUT NOT LIMITED TO THE WARRANTIES OF MERCHANTABILITY,
C FITNESS FOR A PARTICULAR PURPOSE AND NONINFRINGEMENT. IN NO EVENT SHALL THE
C AUTHORS OR COPYRIGHT HOLDERS BE LIABLE FOR ANY CLAIM, DAMAGES OR OTHER
C LIABILITY, WHETHER IN AN ACTION OF CONTRACT, TORT OR OTHERWISE, ARISING FROM,
C OUT OF OR IN CONNECTION WITH THE SOFTWARE OR THE USE OR OTHER DEALINGS IN THE
C SOFTWARE.
C-------------------------------------------------------------------------------
C</license>
c  top of unix change
c  end of unix change
c23456789012345678901234567890123456789012345678901234567890123456789012
c     program  rmod_utility_shell
c     call rmod_utility
c     stop
c     end

c23456789012345678901234567890123456789012345678901234567890123456789012
      subroutine rmod_unix(m_work, work, mv_grd, v_grd, z_lay)
c  This is a utility program for reading, writing and manipulating
c  Conoco velocity model files

c     subroutine rmod_utility
c  This is a utility program for reading, writing and manipulating
c  Conoco velocity model files

      implicit none

c23456789012345678901234567890123456789012345678901234567890123456789012
      integer   util_len_r
      real      util_invert_1

      integer   m_work
c     parameter (m_work=6000000)
      real      work(m_work)

      integer   mv_grd
c     parameter (mv_grd=6000000)
      integer   nx_vel
      real      x0_vel,dx_vel

      integer   ny_vel
      real      y0_vel,dy_vel

      integer   nz_vel
      real      z0_vel,dz_vel

      data      nx_vel,ny_vel,nz_vel/1,1,1/
      real      v_grd(mv_grd)
      real      z_lay(mv_grd)

c      data mb,mxb,mc,mxc,ml,mxl,mbh,mvh,mch,nb,nv,ncv,nl,nbh,nvh,nch

      integer   mb,nb,nv
      parameter (mb=20000)
      integer   mxb
      parameter (mxb=20000)
      integer   imb(mb),itb(mb),ixb(mb),nxb(mb)
      integer   ib_1(mb),ib_2(mb),ib_3(mb)
      real      xb(mxb),yb(mxb),zb(mxb)

      integer   iv_1(mb),iv_2(mb),iv_3(mb)
      integer   imv(mb),itv(mb),ixv(mb),nxv(mb)
      real      xv(mxb),yv(mxb),zv(mxb),vel(mxb)

      integer   icv(mb),ncv
      real      xcv(mxb),ycv(mxb),zcv(mxb)

      integer   mc,nc
      parameter (mc=20000)
      integer   mxc
      parameter (mxc=20000)
      integer   imc(mc),ixc(mc),nxc(mc)
      real      xc(mxc),zc(mxc)

      integer   ml,nl
      parameter (ml=20000)
      integer   mxl
      parameter (mxl=20000)
      integer   ivl(ml),ixl(ml),nxl(ml)
      real      xl(mxl),zl(mxl)

      integer  mbh,nbh
      parameter (mbh=1000)
      character b_name(mbh)*16,b_color(mbh)*16
      data b_name /mbh*'                '/
      data b_color/mbh*'                '/

      integer  mvh,nvh
      parameter (mvh=1000)
      character v_name(mvh)*16,v_color(mvh)*16
      data v_name /mvh*'                '/
      data v_color/mvh*'                '/

      integer  mch,nch
      parameter (mch=1000)
      character c_name(mch)*16,c_color(mch)*16
      data c_name /mch*'                '/
      data c_color/mch*'                '/

      integer   ibid(mbh),icid(mbh),ivid(mbh)
      integer   nbseg(mbh),nvseg(mbh),ncseg(mbh)

      character h_file*64,d_file*64,z_file*64,mod_type*64
      character crd80*80

      integer   m_cord,n_cord
      parameter (m_cord=20)
      real      x_cord(2,m_cord)
      character cord(m_cord)*16

      character cxi*16,cyi*16,czi*16
      character cxo*16,cyo*16,czo*16

      integer   ibw1,ibw2,ibw3
      integer   ivw1,ivw2,ivw3
      integer   icw1,icw2,icw3

      real      x_min,x_max
      real      y_min,y_max
      real      z_min,z_max
      real      z_datum

      integer    i_option,i_stat,lu_out,i_err
c     integer    lib$spawn

c23456789012345678901234567890123456789012345678901234567890123456789012
      data nb,nv,ncv,nl,nbh,nvh,nch
     1    / 0, 0,  0, 0,  0,  0,  0/
     1,n_cord,i_option,lu_out
     1/    0,      1,-1/
     1,cxi,cyi,czi,cxo,cyo,czo
     1/' ',' ',' ',' ',' ',' '/
     1,h_file,d_file,z_file,mod_type
     1/' '  ,' '  ,' '  ,' ' /

      call getluns
      mod_type = 'NONE'
      call rmod_put_i_doc(-1)
      call rmodmdpi(1)    ! turn off modify application
      call gtol_put_lu(6) ! set gtol print unit

      print'(
     1 /,'' This program performs a number of utility services for ''
     1  ,''migration''
     1,/,'' velocity depth models.  You may read, write  and plot ''
     1  ,''models and make''
     1,/,'' various modifications to them.   You may make several ''
     1  ,''modifications''
     1,/,'' sequentialy.  However, some operations are incompatable ''
     1  ,''with the current''
     1,/,'' model  structure and may recquire an intermediate step.  ''
     1)'

    1 continue
c      if (h_file .ne. ' ') print'(/,'' model file:'',a20,'' mod_type=''
c     1,/,'' x= '',a16,'' y= '',a16,'' z= '',a16)',h_file,mod_type,cxi,c

      if (h_file .ne. ' ')
     1call rmod_reverse_negative_increment(mod_type
     1,nx_vel,x0_vel,dx_vel
     1,ny_vel,y0_vel,dy_vel
     1,nz_vel,z0_vel,dz_vel
     1,v_grd
     1)

      call rmod_get_option(i_option
     1,h_file,mod_type
     1,cxi,cyi,czi
     1,nbh,nv,ncv,nc
     1,ixv,nxv,vel
     1,z_datum
     1,x_min,x_max
     1,y_min,y_max
     1,z_min,z_max
     1,nx_vel,x0_vel,dx_vel
     1,ny_vel,y0_vel,dy_vel
     1,nz_vel,z0_vel,dz_vel
     1,v_grd)

      if (h_file .eq. ' ') h_file = 'a'

      if (i_option .eq. 0) then

        goto 2

      elseif (i_option .eq. 99) then    ! spawn a process

c  top of unix change
c  for unix comment out the lib$spawn line
c        print'(/,'' option 99 for spawning is turned off'')'
c        i_stat = lib$spawn(,,,,,,,,,,'rmod$ ',)
c  end of unix change

      elseif (i_option .eq. 1) then     ! read a model

        call rmod_read_model(0,h_file,d_file,z_file,mod_type
     1,cxi,cyi,czi,m_cord,n_cord,x_cord,cord
     1,x_min,x_max,y_min,y_max,z_min,z_max,z_datum
     1,ibw1,ibw2,ibw3,mbh,nbh,ibid,nbseg,b_name,b_color
     1,ivw1,ivw2,ivw3,mvh,nvh,ivid,nvseg,v_name,v_color
     1,icw1,icw2,icw3,mch,nch,icid,ncseg,c_name,c_color
     1,mb,mxb,nb,imb,itb,ixb,nxb,xb,yb,zb,ib_1,ib_2,ib_3
     1,nv,imv,itv,ixv,nxv,xv,yv,zv,vel,iv_1,iv_2,iv_3
     1,ncv,icv,xcv,ycv,zcv
     1,mc,mxc,nc,imc,ixc,nxc,xc,zc
     1,mv_grd
     1,nx_vel,x0_vel,dx_vel
     1,ny_vel,y0_vel,dy_vel
     1,nz_vel,z0_vel,dz_vel,v_grd
     1,m_work,work,lu_out)

      elseif (i_option .eq. 2) then    ! write file

        call rmod_write_model(0,h_file,mod_type
     1,cxi,cyi,czi,m_cord,n_cord,x_cord,cord
     1,x_min,x_max,y_min,y_max,z_min,z_max,z_datum
     1,ibw1,ibw2,ibw3,mbh,nbh,ibid,nbseg,b_name,b_color
     1,ivw1,ivw2,ivw3,mvh,nvh,ivid,nvseg,v_name,v_color
     1,icw1,icw2,icw3,mch,nch,icid,ncseg,c_name,c_color
     1,nb,imb,itb,ixb,nxb,xb,zb,ib_1,ib_2,ib_3
     1,nv,imv,itv,ixv,nxv,xv,yv,zv,vel,iv_1,iv_2,iv_3
     1,ncv,icv,xcv,ycv,zcv
     1,mc,mxc,nc,imc,ixc,nxc,xc,zc
     1,nx_vel,x0_vel,dx_vel
     1,ny_vel,y0_vel,dy_vel
     1,nz_vel,z0_vel,dz_vel
     1,v_grd,z_lay)

      elseif (i_option .eq. 3) then    ! plot model

c  top of unix change
c  for unix comment out rmod_plot_model call
c        print'(/,'' option 3 for plotting is turned off'')'
c        call rmod_plot_model(h_file,mod_type
c     1,cxi,cyi,czi,n_cord,x_cord,cord
c     1,ibw1,ibw2,ibw3,mbh,nbh,ibid,nbseg,b_name,b_color
c     1,ivw1,ivw2,ivw3,mvh,nvh,ivid,nvseg,v_name,v_color
c     1,icw1,icw2,icw3,mch,nch,icid,ncseg,c_name,c_color
c     1,x_min,x_max,z_min,z_max,nc,ixc,nxc,xc,zc,nb,imb,itb,ixb,nxb,xb,zb
c     1,nv,imv,itv,ixv,nxv,xv,zv,vel
c     1,ncv,icv,xcv,zcv,mv_grd,v_grd,z_lay
c     1,m_work,work)
c  end of unix change

      elseif (i_option .eq. 4) then   ! edit a layered model

        call rmod_write_grid_to_ascii(mod_type,h_file,cxi,cyi,czi
     1,m_cord,n_cord,x_cord,cord
     1,nx_vel,x0_vel,dx_vel
     1,ny_vel,y0_vel,dy_vel
     1,nz_vel,z0_vel,dz_vel
     1,v_grd,m_work,work)

      elseif (i_option .eq. 5) then    ! transform coordinates

        call rmod_linear_transform(mod_type,cxi,cyi,czi,n_cord,x_cord
     1,cord
     1,x_min,x_max,y_min,y_max,z_min,z_max,z_datum
     1,nb,ixb,nxb,xb,zb,nv,ixv,nxv,xv,yv,zv
     1,ncv,xcv,ycv,zcv,nc,ixc,nxc,xc,zc
     1,nx_vel,x0_vel,dx_vel
     1,ny_vel,y0_vel,dy_vel
     1,nz_vel,z0_vel,dz_vel
     1,v_grd)

      elseif (i_option .eq. 6) then    ! time to depth

        call rmod_time_to_depth(h_file,mod_type
     1,cxi,cyi,czi,m_cord,n_cord,x_cord,cord
     1,x_min,x_max,y_min,y_max,z_min,z_max,z_datum
     1,nb,imb,itb,ixb,nxb,xb,zb,nv,imv,itv,ixv,nxv,xv,yv,zv,vel
     1,ncv,icv,xcv,ycv,zcv,nc,imc,ixc,nxc,xc,zc
     1,mv_grd
     1,nx_vel,x0_vel,dx_vel
     1,ny_vel,y0_vel,dy_vel
     1,nz_vel,z0_vel,dz_vel
     1,v_grd,z_lay
     1,m_work,work,lu_out)

      elseif (i_option .eq. 47) then    ! time to depth

        call rmod_time_to_depth_new(h_file,mod_type
     1,cxi,cyi,czi,m_cord,n_cord,x_cord,cord
     1,x_min,x_max,y_min,y_max,z_min,z_max,z_datum
     1,nb,imb,itb,ixb,nxb,xb,zb,nv,imv,itv,ixv,nxv,xv,yv,zv,vel
     1,ncv,icv,xcv,ycv,zcv,nc,imc,ixc,nxc,xc,zc
     1,mv_grd
     1,nx_vel,x0_vel,dx_vel
     1,ny_vel,y0_vel,dy_vel
     1,nz_vel,z0_vel,dz_vel
     1,v_grd,z_lay
     1,m_work,work,lu_out)

      elseif (i_option .eq. 7) then     ! layer to grid

        if (mod_type(1:5) .eq. 'LAYER') then

          call rmod_layer_to_grid(h_file,mod_type,x_min,x_max,z_min
     1,z_max
     1,nb,ixb,nxb,xb,zb,nv,imv,itv,ixv,nxv,xv,zv,vel
     1,ncv,icv,xcv,zcv
     1,mc,mxc,nc,imc,ixc,nxc,xc,zc
     1,nx_vel,x0_vel,dx_vel
     1,nz_vel,z0_vel,dz_vel
     1,v_grd,m_work,work
     1,i_err)

        else    ! if (mod_type(1:5) .eq. 'LAYER') then

          call rmod_3dl_to_3dg(mod_type,z_min,z_max
     1,nv,itv,ixv,nxv,xv,yv,zv,vel
     1,ncv,icv,xcv,ycv,zcv
     1,mv_grd
     1,nx_vel,x0_vel,dx_vel
     1,ny_vel,y0_vel,dy_vel
     1,nz_vel,z0_vel,dz_vel
     1,v_grd,z_lay
     1,m_work,work)

        endif    ! if (mod_type(1:5) .eq. 'LAYER') then

      elseif (i_option .eq. 8) then     ! grid to layer

        call rmod_grid_to_layer(h_file,mod_type
     1,cxi,cyi,czi,m_cord,n_cord,x_cord,cord
     1,x_min,x_max,y_min,y_max,z_min,z_max,z_datum
     1,ibw1,ibw2,ibw3,mbh,nbh,ibid,nbseg,b_name,b_color
     1,ivw1,ivw2,ivw3,mvh,nvh,ivid,nvseg,v_name,v_color
     1,icw1,icw2,icw3,mch,nch,icid,ncseg,c_name,c_color
     1,mb,mxb,nb,imb,itb,ixb,nxb,xb,zb,nv,imv,itv,ixv,nxv,xv,yv,zv,vel
     1,ncv,icv,xcv,ycv,zcv,mc,mxc,nc,imc,ixc,nxc,xc,zc
     1,mv_grd
     1,nx_vel,x0_vel,dx_vel
     1,ny_vel,y0_vel,dy_vel
     1,nz_vel,z0_vel,dz_vel
     1,v_grd
     1,m_work,work,lu_out)

      elseif (i_option .eq. 9) then    ! layer to cell

        call rmod_layer_to_cell(mod_type,x_min,x_max,z_min,z_max
     1,nb,ixb,nxb,xb,zb
     1,nv,imv,itv,ixv,nxv,xv,zv,vel
     1,ncv,icv,xcv,zcv
     1,mc,mxc,nc,imc,ixc,nxc,xc,zc,m_work,work,i_err)

      elseif (i_option .eq. 10) then    ! cell to grid

        call rmod_cell_to_grid(h_file,mod_type
     1,nv,imv,itv,ixv,nxv,xv,zv,vel,nc,imc,ixc,nxc,xc,zc
     1,nx_vel,x0_vel,dx_vel,nz_vel,z0_vel,dz_vel,v_grd,m_work,work
     1,i_err)

      elseif (i_option .eq. 11) then   ! interpolate gridded model

        call rmod_smooth_grid(mod_type
     1,mv_grd
     1,nx_vel,x0_vel,dx_vel
     1,ny_vel,y0_vel,dy_vel
     1,nz_vel,z0_vel,dz_vel
     1,v_grd
     1,m_work,work)

      elseif (i_option .eq. 12) then   ! interpolate gridded model

        call rmod_interpolate_grid(mod_type
     1,mv_grd
     1,nx_vel,x0_vel,dx_vel
     1,ny_vel,y0_vel,dy_vel
     1,nz_vel,z0_vel,dz_vel
     1,v_grd
     1,m_work,work)

      elseif (i_option .eq. 13) then    ! set modify option

        call rmod_set_modify

      elseif (i_option .eq. 14) then    ! apply modify files

        call rmod_apply_modify(h_file,mod_type
     1,cxi,cyi,czi,m_cord,n_cord,x_cord,cord
     1,x_min,x_max,y_min,y_max,z_min,z_max,z_datum
     1,ibw1,ibw2,ibw3,mbh,nbh,ibid,nbseg,b_name,b_color
     1,ivw1,ivw2,ivw3,mvh,nvh,ivid,nvseg,v_name,v_color
     1,icw1,icw2,icw3,mch,nch,icid,ncseg,c_name,c_color
     1,mb,mxb,nb,imb,itb,ixb,nxb,xb,yb,zb,ib_1,ib_2,ib_3
     1,nv,imv,itv,ixv,nxv,xv,yv,zv,vel,iv_1,iv_2,iv_3
     1,ncv,icv,xcv,ycv,zcv
     1,mc,mxc,nc,imc,ixc,nxc,xc,zc
     1,mv_grd
     1,nx_vel,x0_vel,dx_vel
     1,ny_vel,y0_vel,dy_vel
     1,nz_vel,z0_vel,dz_vel
     1,v_grd,z_lay
     1,m_work,work,lu_out)

      elseif (i_option .eq. 15) then    ! 2d layered to 3d layered

        call rmod_2dl_to_3dl(h_file,d_file,mod_type
     1,mb,imb,itb,ixb,nxb,xb,zb,imv
     1,itv,nx_vel,x0_vel,dx_vel
     1,ny_vel,y0_vel,dy_vel
     1,nz_vel,v_grd,z_lay
     1,m_work,work)

      elseif (i_option .eq. 16) then    ! 2d gridded to 3d layered

        call rmod_23dg_to_3dl(h_file,d_file,z_file,mod_type
     1,cxi,cyi,czi,m_cord,n_cord,x_cord,cord
     1,nv,itv,ixv,nxv,xv,yv,zv,vel
     1,ncv,icv,xcv,ycv,zcv
     1,mv_grd,nx_vel,x0_vel,dx_vel
     1,ny_vel,y0_vel,dy_vel
     1,nz_vel,v_grd
     1,z_lay
     1,m_work,work)

      elseif (i_option .eq. 17) then    ! 3d layered to 2d layered

        call rmod_3dl_to_2dl(mod_type
     1,x_min,x_max
     1,y_min,y_max
     1,z_min,z_max
     1,nx_vel,x0_vel,dx_vel
     1,ny_vel,y0_vel,dy_vel
     1,nz_vel,z0_vel,dz_vel
     1,v_grd
     1,mb,mxb,nb,imb,itb,ixb,nxb,xb,zb
     1,nv,imv,itv,ixv,nxv,xv,yv,zv,vel
     1,ncv,icv,xcv,ycv,zcv
     1,m_work,work)

      elseif (i_option .eq. 18) then    ! 3d layered to 3d grid

        call rmod_3dg_to_2dg(mod_type
     1,x_min,x_max
     1,y_min,y_max
     1,mv_grd
     1,nx_vel,x0_vel,dx_vel
     1,ny_vel,y0_vel,dy_vel
     1,nz_vel,z0_vel,dz_vel
     1,v_grd
     1,m_work,work)

      elseif (i_option .eq. 19) then    ! create transform file

        call rmod_make_transform(m_cord,n_cord,x_cord,cord)

      elseif (i_option .eq. 20) then    ! create velocity horizons from

        call rmod_vhor(mod_type,x_min,x_max,z_min,z_max
     1,mb,mxb,nb,imb,itb,ixb,nxb,xb,zb,nv,imv,itv,ixv,nxv,xv,zv,vel
     1,ncv,icv,xcv,zcv,m_work,work)

      elseif (i_option .eq. 21) then    ! scale 3d velocity values

        if (mod_type(1:5) .eq. 'LAYER') then

          call rmod_scale_2d_vel(mod_type,nv,imv,itv,ixv,nxv,vel)

        else

          call rmod_scale_3d_vel(mod_type,nv,itv,ixv,nxv,vel)

        endif

      elseif (i_option .eq. 22) then    ! read a model in Shein's format

        call rmod_read_shein(h_file,d_file,mod_type
     1,cxi,cyi,czi,m_cord,n_cord,x_cord,cord
     1,x_min,x_max,y_min,y_max,z_min,z_max,z_datum
     1,ibw1,ibw2,ibw3,mbh,nbh,ibid,nbseg,b_name,b_color
     1,ivw1,ivw2,ivw3,mvh,nvh,ivid,nvseg,v_name,v_color
     1,icw1,icw2,icw3,mch,nch,icid,ncseg,c_name,c_color
     1,mb,mxb,nb,imb,itb,ixb,nxb,xb,zb,nv,imv,itv,ixv,nxv,xv,yv,zv,vel
     1,ncv,icv,xcv,ycv,zcv
     1,nx_vel,x0_vel,dx_vel
     1,ny_vel,y0_vel,dy_vel
     1,nz_vel,z0_vel,dz_vel
     1)

      elseif (i_option .eq. 23) then     ! read a model

        call rmod_convert_velocity_type(mod_type,czi
     1,nx_vel,ny_vel,nz_vel,z0_vel,dz_vel,v_grd,m_work,work)

      elseif (i_option .eq. 24) then     ! read a model

        call rmod_edit_model(mod_type
     1,cxi,cyi,czi,m_cord,n_cord,x_cord,cord
     1,ibw1,ibw2,ibw3,mbh,nbh,ibid,nbseg,b_name,b_color
     1,ivw1,ivw2,ivw3,mvh,nvh,ivid,nvseg,v_name,v_color
     1,icw1,icw2,icw3,mch,nch,icid,ncseg,c_name,c_color
     1,x_min,x_max,y_min,y_max,z_min,z_max,z_datum
     1,mxb,nb,imb,itb,ixb,nxb,xb,zb
     1,nv,imv,itv,ixv,nxv,xv,zv,vel
     1,ncv,icv,xcv,zcv
     1,mv_grd
     1,nx_vel,x0_vel,dx_vel
     1,ny_vel,y0_vel,dy_vel
     1,nz_vel,z0_vel,dz_vel
     1,v_grd
     1,m_work,work)

      elseif (i_option .eq. 25) then     ! write velocity file

        call rmod_read_cps_v(h_file,mod_type
     1,cxi,cyi,czi
     1,m_cord,n_cord,x_cord,cord
     1,x_min,x_max
     1,y_min,y_max
     1,z_min,z_max
     1,z_datum
     1,nx_vel,x0_vel,dx_vel
     1,ny_vel,y0_vel,dy_vel
     1,nz_vel,z0_vel,dz_vel
     1,mv_grd,v_grd
     1,m_work,work)

      elseif (i_option .eq. 26) then     ! time to depth convert an asci

        call rmod_ascii_time_to_depth(h_file,mod_type,cxi,cyi,czi
     1,z_datum
     1,nx_vel,x0_vel,dx_vel
     1,ny_vel,y0_vel,dy_vel
     1,nz_vel,z0_vel,dz_vel
     1,v_grd)

      elseif (i_option .eq. 27) then     ! time to depth convert an asci

        call rmod_write_velocity_cards(h_file
     1,nx_vel,x0_vel,dx_vel
     1,ny_vel,y0_vel,dy_vel
     1,nz_vel,z0_vel,dz_vel
     1)

      elseif (i_option .eq. 28) then    ! 2d gridded to 3d layered

        call rmod_replace_grid(h_file,mod_type
     1,cxi,cyi,czi,m_cord,n_cord,x_cord,cord
     1,x_min,x_max,y_min,y_max,z_min,z_max,z_datum
     1,nv,itv,ixv,nxv,xv,yv,zv,vel
     1,ncv,icv,xcv,ycv,zcv
     1,mv_grd
     1,nx_vel,x0_vel,dx_vel
     1,ny_vel,y0_vel,dy_vel
     1,nz_vel,z0_vel,dz_vel
     1,v_grd,z_lay
     1,m_work,work)

      elseif (i_option .eq. 29) then     ! make a constraint section

        call rmod_make_constraint(h_file,mod_type,nv,imv,ixv,nxv,vel)

      elseif (i_option .eq. 30) then     ! limit a gridd to a range of v

        call rmod_limit_velocity(mod_type,nx_vel*ny_vel*nz_vel,v_grd)

      elseif (i_option .eq. 31) then     ! transpose a grid

        call rmod_transpose_grid(mod_type,nz_vel,nx_vel,ny_vel,v_grd
     1,m_work
     1,work)

      elseif (i_option .eq. -31) then     ! transpose a grid

        call xrmod_transpose_grid(mod_type,nz_vel,nx_vel,ny_vel,v_grd
     1,m_work
     1,work)

      elseif (i_option .eq. 32) then     ! find and replace values

        call rmod_find_and_replace_values(mod_type,h_file
     1,nx_vel,x0_vel,dx_vel
     1,ny_vel,y0_vel,dy_vel
     1,nz_vel,z0_vel,dz_vel
     1,v_grd)

      elseif (i_option .eq. 33) then   ! layer to SIERRA

        call rmod_layer_to_sierra(mod_type,x_min,x_max,z_min,z_max
     1,nb,ixb,nxb,xb,zb,ml,mxl,nl,ivl,ixl,nxl,xl,zl
     1,nv,imv,itv,ixv,nxv,xv,zv,vel
     1,ncv,icv,xcv,zcv
     1,m_work,work,i_err)

      elseif (i_option .eq. 34) then   ! shift gridded velocity model

        call rmod_shift_model(h_file,mod_type
     1,cxi,cyi,czi
     1,m_cord,n_cord,x_cord,cord
     1,nb,ixb,nxb,xb,yb,zb
     1,nv,ixv,nxv,xv,yv,zv
     1,ncv,xcv,ycv,zcv
     1,mv_grd
     1,nx_vel,x0_vel,dx_vel
     1,ny_vel,y0_vel,dy_vel
     1,nz_vel,z0_vel,dz_vel
     1,v_grd,z_lay
     1,m_work,work)

      elseif (i_option .eq. 35) then   ! shift gridded velocity model

        call rmod_operate_on_grid(h_file,mod_type
     1,cxi,cyi,czi,m_cord,n_cord,x_cord,cord
     1,mv_grd
     1,nx_vel,x0_vel,dx_vel
     1,ny_vel,y0_vel,dy_vel
     1,nz_vel,z0_vel,dz_vel
     1,v_grd,z_lay,m_work,work)

      elseif (i_option .eq. 36) then   ! regularize 2d layered model

        call rmod_grid_2dl(mod_type
     1,nb,ixb,nxb,xb,zb
     1,nx_vel,x0_vel,dx_vel
     1,ny_vel,y0_vel,dy_vel
     1,nz_vel,z0_vel,dz_vel
     1,v_grd,m_work,work)

      elseif (i_option .eq. 37) then   ! recolumnate an ascii file

        call recolumnate

      elseif (i_option .eq. 38) then    ! 3d layered to 3d grid

        call rmod_average_vz(mod_type,nx_vel,ny_vel,nz_vel,v_grd,nx_vel
     1,ny_vel
     1,nz_vel,v_grd)

      elseif (i_option .eq. 39) then    ! condition 2D boundaries

        call rmod_condition_boundaries(mod_type
     1,x_min,x_max
     1,z_min,z_max
     1,mb,mxb,nb,imb,itb,ixb,nxb,xb,zb
     1,m_work,work
     1,i_err)

      elseif (i_option .eq. 40) then    ! decimate boundaries

        call rmod_decimate_2d_layer(mod_type
     1,x_min,x_max
     1,z_min,z_max
     1,nb,imb,itb,ixb,nxb,xb,zb
     1,nv,imv,itv,ixv,nxv,xv,yv,zv,vel)

      elseif (i_option .eq. 41) then    ! decimate boundaries

        call rmod_convert_v_stack_to_nmo_t_0(mod_type,czi
     1,nx_vel*ny_vel,nz_vel,z0_vel,dz_vel,v_grd)

      elseif (i_option .eq. 42) then     ! layer to grid

        if (mod_type(1:4) .eq. 'G3DL') then

          call rmod_3dl_to_3dg_new(mod_type,z_min,z_max
     1,nv,itv,ixv,nxv,xv,yv,zv,vel
     1,ncv,icv,xcv,ycv,zcv
     1,mv_grd
     1,nx_vel,x0_vel,dx_vel
     1,ny_vel,y0_vel,dy_vel
     1,nz_vel,z0_vel,dz_vel
     1,v_grd,z_lay
     1,m_work,work)

        endif    ! if (mod_type(1:4) .eq. 'G3DL') then

      elseif (i_option .eq. 43) then     ! condition surfaces

        call rmod_set_surfaces(mod_type
     1,nx_vel,ny_vel,nz_vel,v_grd)

      elseif (i_option .eq. 44) then    ! cell to Geodepth

c  convert from layer to grid
        if (mod_type(1:5) .eq. 'LAYER') 
     1call rmod_layer_to_cell(mod_type,x_min,x_max,z_min,z_max
     1,nb,ixb,nxb,xb,zb
     1,nv,imv,itv,ixv,nxv,xv,zv,vel
     1,ncv,icv,xcv,zcv
     1,mc,mxc,nc,imc,ixc,nxc,xc,zc
     1,m_work,work
     1,i_err)

        call rmod_cell_to_geodepth(mod_type
     1,x_min,x_max
     1,z_min,z_max
     1,nv,imv,itv,ixv,nxv,xv,zv,vel
     1,nc,imc,ixc,nxc,xc,zc
     1,nx_vel,x0_vel,dx_vel
     1,nz_vel,z0_vel,dz_vel
     1,v_grd
     1,m_work,work
     1,i_err)

      elseif (i_option .eq. 45) then   ! interpolate gridded model

        call rmod_smooth_grid_fast(mod_type
     1,mv_grd
     1,nx_vel,x0_vel,dx_vel
     1,ny_vel,y0_vel,dy_vel
     1,nz_vel,z0_vel,dz_vel
     1,v_grd
     1,m_work,work)

      elseif (i_option .eq. 46) then    ! scale cps velocity files

        call rmod_scale_cps_v(m_work,work)

      endif

      i_option = 0
      call rmod_put_i_doc(-1)
      goto 1

    2 continue
      return

      end

c23456789012345678901234567890123456789012345678901234567890123456789012
      subroutine rmod_get_i_doc(j_doc)
      data i_doc/-1/
      save i_doc
      j_doc = i_doc
      return

c23456789012345678901234567890123456789012345678901234567890123456789012
      entry rmod_put_i_doc(j_doc)
      i_doc = j_doc
      return

      end

c23456789012345678901234567890123456789012345678901234567890123456789012
      subroutine rmod_get_option(i_option
     1,h_file,mod_type,cxi,cyi,czi
     1,nbh,nv,ncv,nc
     1,ixv,nxv,vel
     1,z_datum
     1,x_min,x_max
     1,y_min,y_max
     1,z_min,z_max
     1,nx_vel,x0_vel,dx_vel
     1,ny_vel,y0_vel,dy_vel
     1,nz_vel,z0_vel,dz_vel
     1,v_grd)

      implicit none

      integer   util_len_r
      real      util_invert_1

      integer   i_option
      integer   nbh,nv,ncv,nc
      integer   ixv(1),nxv(1)
      real      vel(1)
      real      z_datum
      real      x_min,x_max
      real      y_min,y_max
      real      z_min,z_max

      integer   nx_vel
      real      x0_vel,dx_vel

      integer   ny_vel
      real      y0_vel,dy_vel

      integer   nz_vel
      real      z0_vel,dz_vel

      real      v_grd(1)

      character h_file*(*),mod_type*(*),cxi*(*),cyi*(*),czi*(*)

      character crd80*80

    1 continue

      print'(
     1   ''  0 - end                              ''
     1  ,'' 99 - spawn a process                  ''
     1,/,''  1 - read model                       ''
     1  ,''  2 - write model                      ''
     1,/,''  3 - plot model                       ''
     1  ,''  4 - write grid model to ascii file   ''
     1,/,''  5 - linear coordinate transform      ''
     1  ,''  6 - time/depth, depth/time conversion''
     1,/,''  7 - 2d / 3d layer to grid conversion ''
     1  ,''  8 - grid to layer conversion         ''
     1,/,''  9 - layer to cell conversion         ''
     1  ,'' 10 - cell to grid conversion          ''
     1)'
      print'(
     1   '' 11 - smooth gridded model             ''
     1  ,'' 12 - interpolate gridded model        ''
     1,/,'' 13 - set the modify file application  ''
     1  ,'' 14 - apply modify files               ''
     1,/,'' 15 - 2d layered to 3d layered         ''
     1  ,'' 16 - 2d/3d gridded to 3d layered      ''
     1,/,'' 17 - 3d layered to 2d layered         ''
     1  ,'' 18 - 3d gridded to crooked 2d gridded ''
     1,/,'' 19 - create transform file            ''
     1  ,'' 20 - interpolate boundaries to grid   ''
     1)'
      print'(
     1   '' 21 - scale layered velocities         ''
     1  ,'' 22 - read model in Sheins format      ''
     1,/,'' 23 - convert CPS velocity types       ''
     1  ,'' 24 - edit model parameters            ''
     1,/,'' 25 - read  a cps velocity file        ''
     1  ,'' 26 - time to depth convert ascii file ''
     1,/,'' 27 - create 3d velocity cards on grid ''
     1  ,'' 28 - replace values in grid model     ''
     1,/,'' 29 - change velocities to constraints ''
     1  ,'' 30 - limit velocity to defined range  ''
     1)'
      print'(
     1   '' 31 - transpose a gridded model        ''
     1  ,'' 32 - locate and replace values        ''
     1,/,'' 33 - layer to sierra conversion       ''
     1  ,'' 34 - shift model using by hg3dl files ''
     1,/,'' 35 - operate on gridded files         ''
     1  ,'' 36 - interpolate 2d boundaries to G3DL''
     1,/,'' 37 - recolumnate an ascii file        ''
     1  ,'' 38 - average a gridded velocity       ''
     1,/,'' 39 - condition 2D boundaries          ''
     1  ,'' 40 - decimate 2D layered models       ''
     1)'
      print'(
     1   '' 41 - convert anisotropic stacking-nmo ''
     1  ,'' 42 - new 3d layer to 3d grid          ''
     1,/,'' 43 - condition surface depths         ''
     1  ,'' 44 - create Geodepth velocities       ''
     1,/,'' 45 - smooth gridded model fast        ''
     1  ,'' 46 - scale cps velocity files         ''
     1,/,'' 47 - new time to depth transform      ''
     1)'

      if (h_file .ne. ' ')
     1call rmod_print_info(h_file,mod_type
     1,cxi,cyi,czi
     1,nbh,nv,ncv,nc
     1,ixv,nxv,vel
     1,z_datum
     1,x_min,x_max
     1,y_min,y_max
     1,z_min,z_max
     1,nx_vel,x0_vel,dx_vel
     1,ny_vel,y0_vel,dy_vel
     1,nz_vel,z0_vel,dz_vel
     1,v_grd)

      print'(
     1 /,'' Enter the option number  default= '',i5)',i_option
      read(*,'(a)',end=2)crd80
      if (crd80 .ne. ' ') read(crd80,*,err=1)i_option

    2 continue

      if (i_option .lt. 0) then
        call rmod_put_i_doc(+1)
      else
        call rmod_put_i_doc(01)
      endif
      i_option = iabs(i_option)

      return
      end

c23456789012345678901234567890123456789012345678901234567890123456789012
      subroutine rmod_set_modify
      character ans*3,cmod(2)*3
      data cmod/'Yes','No'/
      call rmodmdgi(imodify)
    1 continue
      ans = cmod(imodify+1)
      print'(/,'' This option lets you set the modify application'')'
      call rmod_ans(ans,ans,'Y','N','apply the modify file?')

      if (ans(1:1) .eq. 'Y') then
        imodify = 0
      else
        imodify = 1
      endif
      print'('' setting modify application to '',a)',cmod(imodify+1)
      call rmodmdpi(imodify)
      return
      end

c23456789012345678901234567890123456789012345678901234567890123456789012
      subroutine rmod_apply_modify(h_file,mod_type
     1,cxi,cyi,czi,m_cord,n_cord,x_cord,cord
     1,x_min,x_max,y_min,y_max,z_min,z_max,z_datum
     1,ibw1,ibw2,ibw3,mbh,nbh,ibid,nbseg,b_name,b_color
     1,ivw1,ivw2,ivw3,mvh,nvh,ivid,nvseg,v_name,v_color
     1,icw1,icw2,icw3,mch,nch,icid,ncseg,c_name,c_color
     1,mb,mxb,nb,imb,itb,ixb,nxb,xb,yb,zb,ib_1,ib_2,ib_3
     1,nv,imv,itv,ixv,nxv,xv,yv,zv,vel,iv_1,iv_2,iv_3
     1,ncv,icv,xcv,ycv,zcv
     1,mc,mxc,nc,imc,ixc,nxc,xc,zc
     1,mv_grd
     1,nx_vel,x0_vel,dx_vel
     1,ny_vel,y0_vel,dy_vel
     1,nz_vel,z0_vel,dz_vel
     1,v_grd,z_lay
     1,m_work,work,lu)
      integer   util_len_r
      integer   ib_1,ib_2,ib_3,iv_1,iv_2,iv_3
      character *(*) h_file,mod_type,cxi,cyi,czi,cord(*)
     1,b_name(*),b_color(*),v_name(*),v_color(*),c_name(*),c_color(*)
      character *16 cxo,cyo,czo,crd80*80
      parameter (m_file=20)
      character *64 d_file,word_type,g_file,a_file(m_file)
      dimension y_file(m_file)
      logical exist
      makec = 0
      cxo = ' '
      cyo = ' '
      czo = ' '

      print'(/,'' This option applys a modify file to the 2d models''
     1,'' files listed in the HMODIFY section.''
     1,/,'' This file will contain a *MODIFY section with ''
     1,''a list of modifcations''
     1,/,'' as well as a *HMODIFY section with ''
     1,''a list of 2D files to be modified''
     1,/,'' Note this overides previous setting for the modify file''
     1,/,'' application.'')'
      call rmodmdgi(imodify)
      call rmodmdpi(0)

    1 continue
      call util_copy_file_name(h_file,g_file)
      call util_get_file_name(' Enter the modify filename'
     1,g_file,'modify')
      call rmod_inquire_exist(g_file,*1)

c  add this modify file to the list of modifications
      call rmodmdpf(g_file)

c  get the list of 2D model from the HMODIFY section
      call rmod_read_file_list('*HMODIFY',g_file
     1,m_file,n_file,a_file,y_file,i_err)
      if (i_err .ne. 0) goto 999

      print'('' modifying files n_file:'',i5)',n_file
      print'('' i='',i5,'' file='',a)'
     1,(i,a_file(i)(1:util_len_r(a_file(i))),i=1,n_file)

c  create the output file name
      call rmod_par0(g_file(1:2),g_file(3:4))

      do 2 i = 1 , n_file
        h_file = a_file(i)
        call rmod_inquire_exist(h_file,*2)
c        call rmodmdpn
        call rmod_read_model(1,h_file,d_file,word_type,mod_type
     1,cxi,cyi,czi,m_cord,n_cord,x_cord,cord
     1,x_min,x_max,y_min,y_max,z_min,z_max,z_datum
     1,ibw1,ibw2,ibw3,mbh,nbh,ibid,nbseg,b_name,b_color
     1,ivw1,ivw2,ivw3,mvh,nvh,ivid,nvseg,v_name,v_color
     1,icw1,icw2,icw3,mch,nch,icid,ncseg,c_name,c_color
     1,mb,mxb,nb,imb,itb,ixb,nxb,xb,yb,zb,ib_1,ib_2,ib_3
     1,nv,imv,itv,ixv,nxv,xv,yv,zv,vel,iv_1,iv_2,iv_3
     1,ncv,icv,xcv,ycv,zcv
     1,mc,mxc,nc,imc,ixc,nxc,xc,zc
     1,mv_grd
     1,nx_vel,x0_vel,dx_vel
     1,ny_vel,y0_vel,dy_vel
     1,nz_vel,z0_vel,dz_vel
     1,v_grd
     1,m_work,work,lu)

        call rmod_apend_close             ! close the apend file
        call rmod_file_to_apend(h_file)    ! add this to the apend file
        crd80 = ' '
        write(crd80,'('' RMOD applying modify file to 2D models'')')
        call rmod_title_to_history(crd80)
        call rmodlenr(lg,g_file)
        crd80 = ' '
        write(crd80,'('' modify file name:'',a)')g_file(1:lg)
        call rmod_card_to_history(crd80)

c  create the output file name using the line number
        call rmod_par1(y_file(i),h_file)
        call rmodlenr(la,a_file(i))
        call rmodlenr(lh,h_file)
        print'(/,'' i='',i5,'' y='',f10.2,'' inp:'',a20
     1,/,'' out:'',a20)'
     1,i,y_file(i),a_file(i)(1:min(20,la)),h_file(1:min(20,lh))

        call rmod_write_model(2,h_file,mod_type
     1,cxi,cyi,czi,m_cord,n_cord,x_cord,cord
     1,x_min,x_max,y_min,y_max,z_min,z_max,z_datum
     1,ibw1,ibw2,ibw3,mbh,nbh,ibid,nbseg,b_name,b_color
     1,ivw1,ivw2,ivw3,mvh,nvh,ivid,nvseg,v_name,v_color
     1,icw1,icw2,icw3,mch,nch,icid,ncseg,c_name,c_color
     1,nb,imb,itb,ixb,nxb,xb,zb,ib_1,ib_2,ib_3
     1,nv,imv,itv,ixv,nxv,xv,yv,zv,vel,iv_1,iv_2,iv_3
     1,ncv,icv,xcv,ycv,zcv
     1,mc,mxc,nc,imc,ixc,nxc,xc,zc
     1,nx_vel,x0_vel,dx_vel
     1,ny_vel,y0_vel,dy_vel
     1,nz_vel,z0_vel,dz_vel
     1,v_grd,z_lay)

    2 continue

      call rmodmdpi(imodify)
      return
  999 continue
      call rmod_pause(' error reading file',h_file)
      mod_type = 'error'
      call rmodmdpi(imodify)
      return
      end

c23456789012345678901234567890123456789012345678901234567890123456789012
      subroutine rmod_inquire_exist(file,*)
      implicit  none

      integer   rmodf2nuf
      character file*(*)
      logical   exist

      integer   i_err,lf
      character *80 node,user,lfile,temp,msg

c....Crack the network file name i.e. node::user;;file
      node = 'NONE'
      user = 'NONE'
      lfile = file
      temp = file
      i_err = rmodf2nuf(temp,node,user,lfile,msg)

      if (i_err .ne. 0) then
        print'(/,'' error cracking this name file:'',a)',file(1:lf)
        return 1
      endif

      if (node .ne. ' ' .and. node .ne. 'NONE') then
        print'('' currently not checking for existence''
     1,/,'' node='',a10,'' user='',a10,'' file:'',a)'
     1,node,user,file
        return
      endif

      inquire (file=file,exist=exist)    ! make sure this file exists
      if( .not. exist) then
        call rmodlenr(lf,file)
        print'('' this file does not exist file:'',a)',file(1:lf)
        return 1
      endif

      return
      end

c23456789012345678901234567890123456789012345678901234567890123456789012
      subroutine rmod_read_model(i_ask_for_file
     1,h_file,d_file,word_type,mod_type
     1,cxi,cyi,czi,m_cord,n_cord,x_cord,cord
     1,x_min,x_max,y_min,y_max,z_min,z_max,z_datum
     1,ibw1,ibw2,ibw3,mbh,nbh,ibid,nbseg,b_name,b_color
     1,ivw1,ivw2,ivw3,mvh,nvh,ivid,nvseg,v_name,v_color
     1,icw1,icw2,icw3,mch,nch,icid,ncseg,c_name,c_color
     1,mb,mxb,nb,imb,itb,ixb,nxb,xb,yb,zb,ib_1,ib_2,ib_3
     1,nv,imv,itv,ixv,nxv,xv,yv,zv,vel,iv_1,iv_2,iv_3
     1,ncv,icv,xcv,ycv,zcv
     1,mc,mxc,nc,imc,ixc,nxc,xc,zc
     1,mv_grd
     1,nx_vel,x0_vel,dx_vel
     1,ny_vel,y0_vel,dy_vel
     1,nz_vel,z0_vel,dz_vel
     1,v_grd
     1,m_work,work,lu)
      implicit none
      integer i_ask_for_file,m_cord,n_cord
     1,ibw1,ibw2,ibw3,mbh,nbh,ibid,nbseg
     1,ivw1,ivw2,ivw3,mvh,nvh,ivid,nvseg
     1,icw1,icw2,icw3,mch,nch,icid,ncseg
     1,mb,mxb,nb,imb,itb,ixb,nxb,nv,imv,itv,ixv,nxv
     1,ncv,icv,mc,mxc,nc,imc,ixc,nxc
     1,mv_grd,nx_vel,ny_vel,nz_vel,m_work,lu
      integer   ib_1,ib_2,ib_3,iv_1,iv_2,iv_3

      real x_cord(2,m_cord)
     1,x_min,x_max,y_min,y_max,z_min,z_max,z_datum
     1,xb,yb,zb,xv,yv,zv,vel,xcv,ycv,zcv,xc,zc
     1,x0_vel,dx_vel,y0_vel,dy_vel,z0_vel,dz_vel,v_grd(mv_grd)
     1,work(m_work)
      character *(*)
     1 h_file,d_file,word_type,mod_type,cxi,cyi,czi,cord(*)
     1,b_name(*),b_color(*),v_name(*),v_color(*),c_name(*),c_color(*)
      character *16 cxo,cyo,czo
      character *64 m_file,t_file
      character *80 crd80
      integer makec,lh
      logical exist
      integer i_doc

      call rmod_get_i_doc(i_doc)

      makec = 0
      cxo = ' '
      cyo = ' '
      czo = ' '

      if (i_ask_for_file .eq. 0) then
        print'(/,'' This option reads in a model file'')'

        if (i_doc .gt. 0) then

        endif    ! if (i_doc .gt. 0) then


    1   continue
        call util_copy_file_name(h_file,h_file)
        call util_get_file_name(' Enter file name',h_file,'mod')
        call rmod_inquire_exist(h_file,*1)

      endif

      call rmodrall(h_file,d_file,word_type,m_file,t_file,mod_type
     1,cxi,cyi,czi,cxo,cyo,czo,m_cord,n_cord,x_cord,cord
     1,x_min,x_max,y_min,y_max,z_min,z_max,z_datum
     1,ibw1,ibw2,ibw3,mbh,nbh,ibid,nbseg,b_name,b_color
     1,ivw1,ivw2,ivw3,mvh,nvh,ivid,nvseg,v_name,v_color
     1,icw1,icw2,icw3,mch,nch,icid,ncseg,c_name,c_color
     1,mb,mxb,nb,imb,itb,ixb,nxb,xb,zb,nv,imv,itv,ixv,nxv,xv,yv,zv,vel
     1,ncv,icv,xcv,ycv,zcv,mc,mxc,nc,imc,ixc,nxc,xc,zc
     1,mv_grd
     1,nx_vel,x0_vel,dx_vel
     1,ny_vel,y0_vel,dy_vel
     1,nz_vel,z0_vel,dz_vel
     1,v_grd
     1,makec,m_work,work,lu,*999)

c  set boundary horizon info
      if (nbh .eq. 0 .and. mod_type .eq. 'LAYER')
     1call rmodshor(nb,imb,mbh,nbh,ibid,nbseg,b_name,b_color)
c  set velocity horizon info
      if (nvh .eq. 0 .and. mod_type .eq. 'LAYER')
     1call rmodshor(nv,imv,mvh,nvh,ivid,nvseg,v_name,v_color)
c  set cell horizon info
      if (nch .eq. 0 .and. mod_type .eq. 'LAYER')
     1call rmodshor(ncv,icv,mch,nch,icid,ncseg,c_name,c_color)

c  read horizon values
c      if (mod_type .eq. 'G3DL' .and. d_file .eq. 'NONE')
c      print'('' mod_type='',a16)',mod_type
c      print'('' word_type='',a16)',word_type
c      print'('' d_file='',a40)',d_file

c      print'('' mod_type='',a,/,'' word_type='',a)',mod_type,w_mod_typ

      if (mod_type .eq. 'G3DL' .and. (
     1 word_type .eq. 'ASCII'    .or.
     1 word_type .eq. 'GENERIC'  .or.
     1 word_type .eq. 'CHARISMA'     ))
     1call rmod_read_ascii_horizon(h_file,d_file,word_type
     1,nx_vel,x0_vel,dx_vel
     1,ny_vel,y0_vel,dy_vel
     1,nz_vel,v_grd)

c      if (mod_type .eq. 'G3DL')
c     1call rmod_print_z_min_max(nx_vel,ny_vel,nz_vel,v_grd)

      if (mod_type .eq. 'GRID' .and.
     1(word_type .eq. 'ASCII'))
     1call rmod_read_ascii_velocity(d_file,word_type
     1,nx_vel,x0_vel,dx_vel
     1,ny_vel,y0_vel,dy_vel
     1,nz_vel,z0_vel,dz_vel
     1,v_grd)

      call rmod_apend_close             ! close the apend file
      call rmod_file_to_apend(h_file)    ! add this to the apend file

c  add to history
c      call rmodlenr(lh,h_file)
c      crd80 = ' '
c      write(crd80,'('' RMOD reading file:'',a)')h_file(1:lh)
c      call rmod_title_to_history(crd80)

      return
  999 continue
      call rmod_pause(' error reading file',h_file)
      mod_type = 'error'
      return

      end

c23456789012345678901234567890123456789012345678901234567890123456789012
      subroutine rmod_print_info(h_file,mod_type,cxi,cyi,czi
     1,nbh,nv,ncv,nc
     1,ixv,nxv,vel
     1,z_datum
     1,x_min,x_max
     1,y_min,y_max
     1,z_min,z_max
     1,nx_vel,x0_vel,dx_vel
     1,ny_vel,y0_vel,dy_vel
     1,nz_vel,z0_vel,dz_vel
     1,v_grd)

      implicit none

      integer   util_len_r
      real      util_invert_1

      integer   nbh,nv,ncv,nc
      integer   ixv(1),nxv(1)
      real      vel(1)
      real      z_datum
      real      x_min,x_max
      real      y_min,y_max
      real      z_min,z_max

      integer   nx_vel
      real      x0_vel,dx_vel

      integer   ny_vel
      real      y0_vel,dy_vel

      integer   nz_vel
      real      z0_vel,dz_vel

      real      v_grd(1)

      character h_file*(*),mod_type*(*),cxi*(*),cyi*(*),czi*(*)

      real      x1_vel,y1_vel,z1_vel,smin,smax,v_min,v_max
      integer   iz

      x1_vel = x0_vel + (nx_vel - 1) * dx_vel
      y1_vel = y0_vel + (ny_vel - 1) * dy_vel
      z1_vel = z0_vel + (nz_vel - 1) * dz_vel

c      print'(/,'' model mod_type='',a16,'' header file='',a
c     1,/,'' # of boundaries   = '',i8,'' # of velocities   = '',i8
c     1,/,'' # of cell pointers= '',i8,'' # of cells        = '',i8
c     1,/,''  layer min   layer max''
c     1,'' grid min grid max no pts  grid increment coordinate''
c     1,/,'' x  '',f9.1,1x,f9.1,1x,f9.1,1x,f9.1,1x,i5,1x,f9.1,a16
c     1,/,'' y  '',f9.1,1x,f9.1,1x,f9.1,1x,f9.1,1x,i5,1x,f9.1,a16
c     1,/,'' z  '',f9.1,1x,f9.1,1x,f9.1,1x,f9.1,1x,i5,1x,f9.1,a16
c     1)'
c     1,mod_type(1:util_len_r(mod_type)),h_file(1:util_len_r(h_file))
c     1,nbh,nv,ncv,nc
c     1,x_min,x_max,x0_vel,x1_vel,nx_vel,dx_vel,cxi(1:util_len_r(cxi))
c     1,y_min,y_max,y0_vel,y1_vel,ny_vel,dy_vel,cyi(1:util_len_r(cyi))
c     1,z_min,z_max,z0_vel,z1_vel,nz_vel,dz_vel,czi(1:util_len_r(czi))

      print'(/,'' model mod_type='',a16,'' header file='',a
     1,/,'' # of boundaries   = '',i8,'' # of velocities   = '',i8
     1,/,'' # of cell pointers= '',i8,'' # of cells        = '',i8
     1,/,'' zero time datum depth='',g16.9
     1)'
     1,mod_type(1:util_len_r(mod_type)),h_file(1:util_len_r(h_file))
     1,nbh,nv,ncv,nc
     1,z_datum

      if (mod_type .ne. 'GRID')
     1print'(''  layered model characterisitcs''
     1,/,'' x unit='',a16,'' min='',f12.4,'' max='',f12.4
     1,/,'' y unit='',a16,'' min='',f12.4,'' max='',f12.4
     1,/,'' z unit='',a16,'' min='',f12.4,'' max='',f12.4
     1)'
     1,cxi(1:util_len_r(cxi)),x_min,x_max
     1,cyi(1:util_len_r(cyi)),y_min,y_max
     1,czi(1:util_len_r(czi)),z_min,z_max

      if (mod_type .eq. 'G3DL') then

      print'('' 3D layered model grid characteristics''
     1,/,'' Nuber of surface='',i8
     1,/,'' x      # nodes='',i8,'' min='',f12.4,'' max='',f12.4
     1,'' inc='',f12.4
     1,/,'' y      # nodes='',i8,'' min='',f12.4,'' max='',f12.4
     1,'' inc='',f12.4
     1)'
     1,nz_vel
     1,nx_vel,x0_vel,x1_vel,dx_vel
     1,ny_vel,y0_vel,y1_vel,dy_vel

      else    ! if (mod_type .eq. 'GRID') then

      print'('' gridded model characteristics''
     1,/,'' x      # nodes='',i8,'' min='',f12.4,'' max='',f12.4
     1,'' inc='',f12.4
     1,/,'' y      # nodes='',i8,'' min='',f12.4,'' max='',f12.4
     1,'' inc='',f12.4
     1,/,'' z      # nodes='',i8,'' min='',f12.4,'' max='',f12.4
     1,'' inc='',f12.4
     1)'
     1,nx_vel,x0_vel,x1_vel,dx_vel
     1,ny_vel,y0_vel,y1_vel,dy_vel
     1,nz_vel,z0_vel,z1_vel,dz_vel

      endif    ! if (mod_type .eq. 'GRID') then

      if (mod_type .eq. 'GRID') then

        call rmod_print_v_min_max(nx_vel*ny_vel*nz_vel,v_grd,0)

      elseif (mod_type .eq. 'LAYER') then

        call rmod_print_v_min_max(ixv(nv)+nxv(nv),vel,1)

      elseif (mod_type .eq. 'G3DL') then

        call rmod_print_v_min_max(nv,vel,1)
        call rmod_print_z_min_max(nx_vel,ny_vel,nz_vel,v_grd)

      endif    ! if (mod_type .eq. 'GRID') then

      return
      end

c23456789012345678901234567890123456789012345678901234567890123456789012
      subroutine rmod_print_z_min_max(nx_vel,ny_vel,nz_vel,v_grd)
      implicit none
      integer  nx_vel,ny_vel,nz_vel
c      real     v_grd(max(1,nx_vel),max(1,ny_vel),max(1,nz_vel))
      real     v_grd(nx_vel,ny_vel,nz_vel)

      real     z_min,z_max

      integer   ix_vel,iy_vel,iz_vel,jz_vel
      integer   m_above,n_above

      print'('' nx_vel='',i8,'' ny_vel='',i8,'' nz_vel='',i8)'
     1,nx_vel,ny_vel,nz_vel

      do iz_vel = 1 , nz_vel

        call util_min_max(z_min,z_max,nx_vel*ny_vel,v_grd(1,1,iz_vel))
        print'('' horizon='',i5
     1,'' min depth='',g16.6,'' max depth='',g16.6)'
     1,iz_vel,z_min,z_max

      enddo    ! do iz_vel = 1 , nz_vel

      m_above = 0

      do iz_vel = 2 , nz_vel

        jz_vel = iz_vel - 1
        n_above = 0

        do iy_vel = 1 , ny_vel

          do ix_vel = 1 , nx_vel

            if (v_grd(ix_vel,iy_vel,iz_vel) 
     1     .lt. v_grd(ix_vel,iy_vel,jz_vel)) n_above = n_above + 1

          enddo    ! do ix_vel = 1 , nx_vel

        enddo    ! do iy_vel = 1 , ny_vel

        if (n_above .ne. 0)
     1print'('' warning ... surface '',i8
     1,'' has '',i8,'' points above the previous surface'')'
     1,iz_vel,n_above

        m_above = m_above + n_above

      enddo    ! do iz_vel = 2 , nz_vel

      if (m_above .ne. 0) then

        print'(
     1 /,'' warning ... there are poorly conditioned surfaces''
     1,/,'' there are a total of m_above='',i8
     1,'' points above prior surfaces''
     1,/,'' total number of points='',i8)'
     1,m_above,nx_vel*ny_vel*nz_vel
        call rmod_pause(' ',' ')

      endif    ! if (m_above .ne. 0) then

      return
      end

c23456789012345678901234567890123456789012345678901234567890123456789012
      subroutine rmod_print_v_min_max_old(nv_grd,v_grd,i_slow)
c  print min and max velocity in array v_grd
c if i_slow=0 v_grd= slowness otherwise v_grd = velocity
      implicit none

      real     util_invert_1

      integer  nv_grd
      real     v_grd(1)
      integer  i_slow

      real     v_min,v_max

      call util_min_max(v_min,v_max,nv_grd,v_grd)

      if (i_slow .eq. 0) then

        print'('' min velocity='',g16.6,'' max velocity='',g16.6)'
     1,util_invert_1(v_max),util_invert_1(v_min)

      else    ! if (i_slow .eq. 0) then

        print'('' min velocity='',g16.6,'' max velocity='',g16.6)'
     1,v_min,v_max

      endif    ! if (i_slow .eq. 0) then

      return
      end

c23456789012345678901234567890123456789012345678901234567890123456789012
      subroutine rmod_print_v_min_max(nv_grd,v_grd,i_slow)
c  print min and max velocity in array v_grd
c if i_slow=0 v_grd= slowness otherwise v_grd = velocity
      implicit none

      real     util_invert_1

      integer  nv_grd
      real     v_grd(1)
      integer  i_slow

      real     v_min,v_max
      real     v1,v2


      if (i_slow .eq. 0) then

        call util_invert(nv_grd,v_grd)
        call util_min_max(v_min,v_max,nv_grd,v_grd)
        v1 = v_grd(1)
        v2 = v_grd(nv_grd)
        call util_invert(nv_grd,v_grd)

      else    ! if (i_slow .eq. 0) then

        call util_min_max(v_min,v_max,nv_grd,v_grd)
        v1 = v_grd(1)
        v2 = v_grd(nv_grd)

      endif    ! if (i_slow .eq. 0) then

      print'('' min   velocity='',g16.6,'' max  velocity='',g16.6)'
     1,v_min,v_max

      print'('' first velocity='',g16.6,'' last velocity='',g16.6)'
     1,v1,v2

      return
      end

c23456789012345678901234567890123456789012345678901234567890123456789012
      subroutine rmod_read_ascii_horizon(h_file,d_file,word_type
     1,nx_vel,x0_vel,dx_vel
     1,ny_vel,y0_vel,dy_vel
     1,nz_vel
     1,v_grd)
      character h_file*(*),d_file*(*),word_type*(*)

      integer   nx_vel
      real      x0_vel,dx_vel

      integer   ny_vel
      real      y0_vel,dy_vel

      integer   nz_vel
      real      v_grd(nx_vel,ny_vel,nz_vel)

      parameter (m_file=20)
      real      y2(m_file)
      character file(m_file)*64
      integer   util_len_r
      integer   ix_1,ix_2
      integer   iy_1,iy_2
      integer   iz_1,iz_2
      character ans*1

      ans = 'N'
      call rmod_ans(ans,ans,'Y','N'
     1,'read file names from the *GEN section')

      if (ans(1:1) .eq. 'Y') then

      call rmod_read_file_list('*GEN',h_file
     1,m_file,nz_vel,file,y2,i_err)
        if (i_err .ne. 0) goto 999

      else    ! if (ans .eq. 'Y') then

        do iz_vel = 1 , nz_vel
          file(iz_vel) = d_file
        enddo

      endif    ! if (ans .eq. 'Y') then

      print'(/,'' # of depth horizon files ='',i5,'' nx_vel='',i5
     1,'' ny_vel='',i5
     1,/,'' file#  iz_vel  z_min z_max z1  zn  i_err filename'')'
     1,nz_vel,nx_vel,ny_vel

c  flag the depths
      x_fill = -999.
      call util_setr(nx_vel*ny_vel*nz_vel,v_grd,x_fill)

      if (ans(1:1) .eq. 'Y') then
      do iz_vel = 1 , nz_vel

        call rmod_trupath(h_file,file(iz_vel))
        call rmodopen(i_file,file(iz_vel),'FORMATTED','OLD'
     1,'SEQUENTIAL','IEEE',0,i_err)
        if (i_err .ne. 0) goto 999

        if (word_type(1:1) .eq. 'G') then

          do ix_vel = 1 , nx_vel
            read(i_file,*,err=999,end=999)
     1(v_grd(ix_vel,iy_vel,iz_vel),iy_vel=ny_vel,1,-1)
          enddo    ! do ix_vel = 1 , nx_vel

        else    ! if (word_type(1:1) .eq. 'G') then

c        if (iz_vel .eq. 1) call rmod_get_columns(i_file
c     1,n_column,ix,iy,iz,iv)
        if (iz_vel .eq. 1) then

          ix_1 = 24
          ix_2 = 30
          iy_1 = 09
          iy_2 = 15
          iz_1 = 65
          iz_2 = 75

      print'(/,'' to read data in x,y,z order''
     1,'' enter -1,-1 next'')'

          call rmod_get_columns_2(' x coordinate',i_file,ix_1,ix_2,1)
      if (ix_1 .ge. 1 .and. ix_2 .ge. 1) then
          call rmod_get_columns_2(' y coordinate',i_file,iy_1,iy_2,0)
          call rmod_get_columns_2(' z coordinate',i_file,iz_1,iz_2,0)

      endif

        endif    ! if (iz_vel .eq. 1) then

          call rmod_read_data_3(i_file,n_card,n_fill,1,0
     1,ix_1,ix_2,nx_vel,x0_vel,dx_vel,x_min,x_max
     1,iy_1,iy_2,ny_vel,y0_vel,dy_vel,y_min,y_max
     1,iz_1,iz_2,1,v_grd(1,1,iz_vel),z_min,z_max
     1,0,0
     1,i_err)

          print'('' iz='',i5,'' n_card='',i6,'' n_fill='',i6
     1,'' nxy='',i6,'' z='',f8.0,1x,f8.0,/,'' file='',a)'
     1,iz_vel,n_card,n_fill,nx_vel*ny_vel
     1,v_grd(1,1,iz_vel),v_grd(nx_vel,nz_vel,iz_vel)
     1,file(iz_vel)(1:util_len_r(file(iz_vel)))
          print'('' x_min='',f12.4,'' x_max='',f12.4)',x_min,x_max
          print'('' y_min='',f12.4,'' y_max='',f12.4)',y_min,y_max
          print'('' z_min='',f12.4,'' z_max='',f12.4)',z_min,z_max

        endif    ! if (word_type(1:1) .eq. 'G') then

        call util_min_max(z_min,z_max,nx_vel*ny_vel,v_grd(1,1,iz_vel))
        call rmodlenr(lf,file(iz_vel))
        print'(1x,i5,1x,g10.4,1x,g10.4,1x,g10.4,1x,g10.4,/,1x,a)'
     1,iz_vel,z_min,z_max,v_grd(1,1,iz_vel),v_grd(nx_vel,ny_vel,iz_vel)
     1,file(iz_vel)(1:lf)

      enddo    ! do iz_vel = 1 , nz_vel

      else    ! if (ans(1:1) .eq. 'Y') then

        call rmodopen(i_file,d_file,'FORMATTED','OLD'
     1,'SEQUENTIAL','IEEE',0,i_err)
        if (i_err .ne. 0) goto 999

      print'(/,'' reading all horizons from a single file'')'
      print'(/,'' to read data in x,y,z,flag order''
     1,'' enter -1,-1 next'')'
      print'(/,'' to read data in x,z,flag order''
     1,'' enter -1,-2 next'')'

          call rmod_get_columns_2(' x coordinate',i_file,ix_1,ix_2,1)

      if (ix_1 .ge. 1 .and. ix_2 .ge. 1) then
          call rmod_get_columns_2(' y coordinate',i_file,iy_1,iy_2,0)
          call rmod_get_columns_2(' z coordinate',i_file,iz_1,iz_2,0)
          call rmod_get_columns_2(' horizon flag',i_file,if_1,if_2,0)
      endif    ! if (ix_1 .ge. 1 .and. ix_2 .ge. 1) then

          call rmod_read_data_3(i_file,n_card,n_fill,1,1
     1,ix_1,ix_2,nx_vel,x0_vel,dx_vel,x_min,x_max
     1,iy_1,iy_2,ny_vel,y0_vel,dy_vel,y_min,y_max
     1,iz_1,iz_2,nz_vel,v_grd,z_min,z_max
     1,if_1,if_2
     1,i_err)

      do iz_vel = 1 , nz_vel
        call util_min_max(z_min,z_max,nx_vel*ny_vel,v_grd(1,1,iz_vel))
        call rmodlenr(lf,file(1))
        print'(1x,i5,1x,g10.4,1x,g10.4,1x,g10.4,1x,g10.4,/,1x,a)'
     1,iz_vel,z_min,z_max,v_grd(1,1,iz_vel),v_grd(nx_vel,ny_vel,iz_vel)
     1,file(1)(1:lf)

      enddo    ! do iz_vel = 1 , nz_vel

      endif    ! if (ans(1:1) .eq. 'Y') then

c fill flaged values
c      print'('' nz_vel='',i10,'' nx_vel='',i10,'' ny_vel='',i10)'
c     1,nz_vel,nx_vel,ny_vel
      n_fill_x = 0
      n_fill_y = 0

      do iz = 1 , nz_vel
c23456789012345678901234567890123456789012345678901234567890123456789012
c      print'('' iz='',i10)',iz

        do iy = 1 , ny_vel
          call util_fill_vector(l_fill_y,nx_vel,  1,v_grd(
     1 1,iy,iz),x_fill)
          n_fill_x = n_fill_x + l_fill_x
        enddo    ! do iy = 1 , ny_vel

        do ix = 1 , nx_vel
          call util_fill_vector(l_fill_y,ny_vel,nx_vel
     1,v_grd(ix,1,iz),x_fill)
          n_fill_y = n_fill_y + l_fill_y

        enddo    ! do ix = 1 , nx_vel

      enddo    ! do iz = 1 , nz_vel

      print'('' number filled in the x direction='',i10)',n_fill_x
      print'('' number filled in the y direction='',i10)',n_fill_y

c      call util_min_max(z_min,z_max,nx_vel*ny_vel*nz_vel,v_grd)
c      print*,' z_min=',z_min,' z_max=',z_max

      return
  999 continue
      call rmod_pause(' error in rmod_read_generic',h_file)
      return

      end

c23456789012345678901234567890123456789012345678901234567890123456789012
      subroutine rmod_read_ascii_velocity(d_file,word_type
     1,nx_vel,x0_vel,dx_vel
     1,ny_vel,y0_vel,dy_vel
     1,nz_vel,z0_vel,dz_vel
     1,v_grd)
      implicit  none
      character d_file*(*),word_type*(*)
      integer   nx_vel
      real      x0_vel,dx_vel
      integer   ny_vel
      real      y0_vel,dy_vel
      integer   nz_vel
      real      z0_vel,dz_vel
      real      v_grd(nz_vel,nx_vel,ny_vel)

      integer   util_len_r
      integer   ix_1,ix_2
      integer   iy_1,iy_2
      integer   iz_1,iz_2
      integer   iv_1,iv_2
      integer   n_fill_x,n_fill_y,n_fill_z
      integer   l_fill_x,l_fill_y,l_fill_z
      integer   ix,iy,iz
      integer   i_file,n_card,n_fill,i_err
      real      x_fill
      real      x_min,x_max
      real      y_min,y_max
      real      z_min,z_max
      real      v_min,v_max
c  open the data file as an ascii file

      print'(/,'' reading velocity grid from ascii file''
     1,/,'' nx_vel='',i5,'' ny_vel='',i5,'' nz_vel='',i5
     1,'' word_type='',a)'
     1,nx_vel,ny_vel,nz_vel,word_type(1:util_len_r(word_type))

c  flag the depths
      x_fill = -999.
      call util_setr(nx_vel*ny_vel*nz_vel,v_grd,x_fill)
      call rmodopen(i_file,d_file,'FORMATTED','OLD'
     1,'SEQUENTIAL','IEEE',0,i_err)
      if (i_err .ne. 0) goto 999

      if (word_type .eq. 'ASCII') then

        ix_1 =  1
        ix_2 = 13
        iy_1 = 15
        iy_2 = 26
        iz_1 = 28
        iz_2 = 39
        iv_1 = 41
        iv_2 = 52

      print'(/,'' to read data in x,y,z,v order''
     1,'' enter -1,-1 next'')'
      print'(/,'' to read data in x,z,v order''
     1,'' enter -1,-2 next'')'
      call rmod_get_columns_2(' x coordinate',i_file,ix_1,ix_2,1)
      if (ix_1 .ge. 1 .and. ix_2 .ge. 1) then
        call rmod_get_columns_2(' y coordinate',i_file,iy_1,iy_2,0)
        call rmod_get_columns_2(' z coordinate',i_file,iz_1,iz_2,0)
        call rmod_get_columns_2(' v coordinate',i_file,iv_1,iv_2,0)
      endif

        call rmod_read_data_4(i_file,n_card,n_fill,1
     1,ix_1,ix_2,nx_vel,x0_vel,dx_vel,x_min,x_max
     1,iy_1,iy_2,ny_vel,y0_vel,dy_vel,y_min,y_max
     1,iz_1,iz_2,nz_vel,z0_vel,dz_vel,z_min,z_max
     1,iv_1,iv_2,v_min,v_max,v_grd,i_err)

        print'('' n_card='',i10,'' n_fill='',i10
     1,'' nxyz='',i10,/,'' file='',a)'
     1,n_card,n_fill,nx_vel*ny_vel*nz_vel
     1,d_file(1:util_len_r(d_file))
        print'('' x_min='',f12.4,'' x_max='',f12.4)',x_min,x_max
        print'('' y_min='',f12.4,'' y_max='',f12.4)',y_min,y_max
        print'('' z_min='',f12.4,'' z_max='',f12.4)',z_min,z_max
        print'('' v_min='',f12.4,'' v_max='',f12.4)',v_min,v_max

      else    ! if (word_type .eq. 'ASCII') then

        call rmod_read_data_5(i_file,n_card,1
     1,v_min,v_max,v_grd,i_err)

        print'('' n_card='',i10,'' nxyz='',i10,/,'' file='',a)'
     1,n_card,nx_vel*ny_vel*nz_vel
     1,d_file(1:util_len_r(d_file))
        print'('' v_min='',f12.4,'' v_max='',f12.4)',v_min,v_max

      endif    ! if (word_type .eq. 'ASCII') then

c fill flaged values
c      print'('' nz_vel='',i10,'' nx_vel='',i10,'' ny_vel='',i10)'
c     1,nz_vel,nx_vel,ny_vel
      n_fill_x = 0
      n_fill_y = 0
      n_fill_z = 0

c23456789012345678901234567890123456789012345678901234567890123456789012
c  fill in the z direction
      do iy = 1 , ny_vel
        do ix = 1 , nx_vel
          call util_fill_vector(l_fill_z
     1,nz_vel,            1,v_grd( 1,ix,iy),x_fill)
          n_fill_z = n_fill_z + l_fill_z
        enddo    ! do ix = 1 , nx_vel
      enddo    ! do iy = 1 , ny_vel

c  fill in the x direction
      do iy = 1 , ny_vel
        do iz = 1 , nz_vel
          call util_fill_vector(l_fill_x
     1,nx_vel,       nz_vel,v_grd(iz, 1,iy),x_fill)
          n_fill_x = n_fill_x + l_fill_x
        enddo    ! do iz = 1 , nz_vel
      enddo    ! do iy = 1 , ny_vel

c  fill in the y direction
      do ix = 1 , nx_vel
        do iz = 1 , nz_vel
          call util_fill_vector(l_fill_y
     1,ny_vel,nx_vel*nz_vel,v_grd(iz,ix, 1),x_fill)
          n_fill_y = n_fill_y + l_fill_y
        enddo    ! do iz = 1 , nz_vel
      enddo    ! do ix = 1 , nx_vel

      print'('' number filled in the x direction='',i10)',n_fill_x
      print'('' number filled in the y direction='',i10)',n_fill_y
      print'('' number filled in the z direction='',i10)',n_fill_z
      call util_invert(nx_vel*ny_vel*nz_vel,v_grd)

      return
  999 continue
      call rmod_pause(' error in rmod_read_ascii_velocity',d_file)
      return

      end

c23456789012345678901234567890123456789012345678901234567890123456789012
      subroutine rmod_write_model(i_ask_for_file,h_file,mod_type
     1,cxi,cyi,czi,m_cord,n_cord,x_cord,cord
     1,x_min,x_max,y_min,y_max,z_min,z_max,z_datum
     1,ibw1,ibw2,ibw3,mbh,nbh,ibid,nbseg,b_name,b_color
     1,ivw1,ivw2,ivw3,mvh,nvh,ivid,nvseg,v_name,v_color
     1,icw1,icw2,icw3,mch,nch,icid,ncseg,c_name,c_color
     1,nb,imb,itb,ixb,nxb,xb,zb,ib_1,ib_2,ib_3
     1,nv,imv,itv,ixv,nxv,xv,yv,zv,vel,iv_1,iv_2,iv_3
     1,ncv,icv,xcv,ycv,zcv
     1,mc,mxc,nc,imc,ixc,nxc,xc,zc
     1,nx_vel,x0_vel,dx_vel
     1,ny_vel,y0_vel,dy_vel
     1,nz_vel,z0_vel,dz_vel
     1,v_grd,z_lay)
      implicit none
      integer i_ask_for_file,m_cord,n_cord
     1,ibw1,ibw2,ibw3,mbh,nbh,ibid,nbseg
     1,ivw1,ivw2,ivw3,mvh,nvh,ivid,nvseg
     1,icw1,icw2,icw3,mch,nch,icid,ncseg
     1,nb,imb,itb,ixb,nxb,ib_1,ib_2,ib_3
     1,nv,imv,itv,ixv,nxv,iv_1,iv_2,iv_3
     1,ncv,icv,mc,mxc,nc,imc,ixc,nxc
     1,nx_vel,ny_vel,nz_vel
      real x_cord,xb,zb,xv,yv,zv,vel,xcv,ycv,zcv,xc,zc
      real x_min,x_max,y_min,y_max,z_min,z_max,z_datum
      real x0_vel,dx_vel,y0_vel,dy_vel,z0_vel,dz_vel,v_grd(1),z_lay
      character *(*) h_file,mod_type,cxi,cyi,czi,cord(*)
     1,b_name(*),b_color(*),v_name(*),v_color(*),c_name(*),c_color(*)
      integer lh,lt,ld,lw,itrans,i
      character *64 d_file,word_type,m_file,t_file
      character ext*5,stat*3
      integer i_doc

      call rmod_get_i_doc(i_doc)

c  write out file
      if (i_ask_for_file .eq. 0) then
        print'(/,'' This option writes out the current model'')'

        if (i_doc .gt. 0) then
          print'('' you will need to enter the output file name(s)''
     1,/,'' for grid models you will need both a header file name''
     1,/,'' and a binary data file name''
     1)'
        endif    ! if (i_doc .gt. 0) then

      endif

      if (mod_type(1:4) .eq. 'GRID') then
        ext = 'hgrid'
      elseif (mod_type(1:4) .eq. 'G3DL') then
        ext = 'hg3dl'
      elseif (mod_type .eq. 'CELL') then
        ext = 'cell'
      elseif (mod_type(1:1) .eq. 'S') then
        ext = 'sierra'
      else
        ext = 'mod'
      endif

      if (i_ask_for_file .eq. 0) then
        call util_copy_file_name(h_file,h_file)
        call util_get_file_name(' Enter output header file',h_file,ext)
      endif

      d_file = h_file
      t_file = h_file
      m_file = 'NONE'
      word_type = 'VMS'

      if (mod_type(1:4) .eq. 'GRID') then
        call util_copy_file_name(h_file,d_file)
        ext = 'grid'
        if (i_ask_for_file .eq. 0) call util_get_file_name(
     1' Enter output data file',d_file,ext)
      elseif (mod_type(1:4) .eq. 'G3DL') then
        call util_copy_file_name(h_file,d_file)
        ext = 'g3dl'
        if (i_ask_for_file .eq. 0) call util_get_file_name(
     1' Enter output depth grid file',d_file,ext)
      else
        ext = 'modify'
        if (i_ask_for_file .eq. 0) call util_get_file_name(
     1' Enter output modify file',m_file,ext)
      endif

c  get word mod_type
      if (mod_type(1:1) .eq. 'G' .and. i_ask_for_file .eq. 0) then
    1 continue
        word_type ='IEEE'
c        call rmod_read_string(word_type,'IEEE'
c     1,' enter grid file word mod_type IEEE, IBM, VMS and CRAY are vali
        if (word_type .ne. 'IEEE' .and. word_type .ne. 'IBM'
     1 .and. word_type .ne. 'VMS' .and. word_type .ne. 'CRAY') goto 1
      endif

c  set horizon info
      if (nbh .eq. 0 .and. mod_type .eq. 'LAYER')
     1call rmodshor(nb,imb,mbh,nbh,ibid,nbseg,b_name,b_color)
      if (nvh .eq. 0 .and. mod_type .eq. 'LAYER')
     1call rmodshor(nv,imv,mvh,nvh,ivid,nvseg,v_name,v_color)
      if (nch .eq. 0 .and. mod_type .eq. 'LAYER')
     1call rmodshor(ncv,icv,mch,nch,icid,ncseg,c_name,c_color)

      if (i_ask_for_file .eq. 2) then
        call rmodlenr(lh,h_file)
        call rmodlenr(lt,mod_type)
        call rmodlenr(ld,d_file)
        call rmodlenr(lw,word_type)

        print'(/,'' writing header file '',a
     1,/,'' with model mod_type '',a)',h_file(1:lh),mod_type(1:lt)
        if (mod_type(1:1) .eq. 'G')
     1print'(/,'' writing grid file '',a
     1,/,'' with word mod_type '',a)',d_file(1:ld),word_type(1:lw)
      endif

      itrans = 1
      stat = 'NEW'

c23456789012345678901234567890123456789012345678901234567890123456789012
      call rmodwall(h_file,d_file,word_type,m_file,t_file,mod_type
     1,stat,itrans
     1,cxi,cyi,czi,m_cord,n_cord,x_cord,cord
     1,x_min,x_max,y_min,y_max,z_min,z_max,z_datum
     1,ibw1,ibw2,ibw3,nbh,ibid,nbseg,b_name,b_color
     1,ivw1,ivw2,ivw3,nvh,ivid,nvseg,v_name,v_color
     1,icw1,icw2,icw3,nch,icid,ncseg,c_name,c_color
     1,nb,imb,itb,ixb,nxb,xb,zb,nv,imv,itv,ixv,nxv,xv,yv,zv,vel
     1,ncv,icv,xcv,ycv,zcv,mc,mxc,nc,imc,ixc,nxc,xc,zc
     1,nx_vel,x0_vel,dx_vel
     1,ny_vel,y0_vel,dy_vel
     1,nz_vel,z0_vel,dz_vel
     1,v_grd,z_lay,*999)
      call rmod_apend_to_file(h_file)   ! add apend records to file
      call rmodclof(h_file)             ! close file
      call rmodclof(d_file)             ! close file

c      call rmod_write_generic(h_file,mod_type,nbh,b_name,nx_vel,ny_vel,
c      call rmod_write_charisma(h_file,mod_type,nbh,b_name
c     1,nx_vel,x0_vel,dx_vel
c     1,ny_vel,y0_vel,dy_vel
c     1,nz_vel,z0_vel,dz_vel,v

      return
  999 continue
      call rmod_pause(' error writing file',h_file)
      return
      end

c23456789012345678901234567890123456789012345678901234567890123456789012
      subroutine rmod_pause(title,file)
c  pause
      implicit none
      character title*(*),file*(*)
      integer lt,lf
      character crd80*80
      call rmodlenr(lt,title)
      call rmodlenr(lf,file)
      print'('' '')'
      if (lt .ne. 0) print'(a)',title(1:lt)
      if (lf .ne. 0) print'(a)',file(1:lf)
      print'(/,'' enter return to continue'')'
      read(*,'(a)',err=1,end=1)crd80
    1 continue
      return
      end

c23456789012345678901234567890123456789012345678901234567890123456789012
      subroutine rmod_linear_transform(mod_type,cxi,cyi,czi,n_cord
     1,x_cord
     1,cord
     1,x_min,x_max,y_min,y_max,z_min,z_max,z_datum
     1,nb,ixb,nxb,xb,zb,nv,ixv,nxv,xv,yv,zv
     1,ncv,xcv,ycv,zcv,nc,ixc,nxc,xc,zc
     1,nx_vel,x0_vel,dx_vel
     1,ny_vel,y0_vel,dy_vel
     1,nz_vel,z0_vel,dz_vel
     1,z_lay)
c  read new coordinates
      character *(*) mod_type,cxi,cyi,czi,cord(*)
      character  *16 cx0,cy0,cz0
      character  *80 crd80

      print'(/,'' This option allows a linear transformation between''
     1,/,'' 2 pairs of coordinates.''
     1,/,'' note you cannot do a true time to depth or depth to TIME''
     1,/,'' transformation here.  for a true time to DEPTH''
     1,/,'' transformation use that option.'')'

      cx0 = cxi
      cy0 = cyi
      cz0 = czi

      print'('' rmod_linear_transform mod_type='',a16)',mod_type
c  x
      call rmod_linear_transform1(mod_type,1,cxi,n_cord,x_cord,cord
     1,x_min,x_max,nb,ixb,nxb,xb,nv,ixv,nxv,xv
     1,ncv,xcv,nc,ixc,nxc,xc,nx_vel,x0_vel,dx_vel,z_datum
     1,nx_vel*ny_vel*nz_vel,z_lay)
c  y
      call rmod_linear_transform1(mod_type,2,cyi,n_cord,x_cord,cord
     1,y_min,y_max,0,ixb,nxb,xb,nv,ixv,nxv,yv
     1,ncv,ycv,0,ixc,nxc,xc,ny_vel,y0_vel,dy_vel,z_datum
     1,nx_vel*ny_vel*nz_vel,z_lay)
c  z
      call rmod_linear_transform1(mod_type,3,czi,n_cord,x_cord,cord
     1,z_min,z_max,nb,ixb,nxb,zb,nv,ixv,nxv,zv
     1,ncv,zcv,nc,ixc,nxc,zc,nz_vel,z0_vel,dz_vel,z_datum
     1,nx_vel*ny_vel*nz_vel,z_lay)

      if (cxi .ne. cx0 .or. cyi .ne. cy0 .or. czi .ne. cz0) then
        call rmodlenr(lt,mod_type)
        crd80 = ' '
        write(crd80,'('' RMOD applying linear coordinate transform''
     1,'' - file mod_type :'',a)')mod_type(1:lt)
        call rmod_title_to_history(crd80)
        if (cxi .ne. cx0) then
          crd80 = ' '
          write(crd80,'('' input X coordinate:'',a16
     1,'' output X coordinate:'',a16)')cx0,cxi
          call rmod_card_to_history(crd80)
        endif
        if (cyi .ne. cy0) then
          crd80 = ' '
          write(crd80,'('' input Y coordinate:'',a16
     1,'' output Y coordinate:'',a16)')cy0,cyi
          call rmod_card_to_history(crd80)
        endif
        if (czi .ne. cz0) then
          crd80 = ' '
          write(crd80,'('' input Z coordinate:'',a16
     1,'' output Z coordinate:'',a16)')cz0,czi
          call rmod_card_to_history(crd80)
        endif
      endif

      return
      end

c23456789012345678901234567890123456789012345678901234567890123456789012
      subroutine rmod_linear_transform1(mod_type,icx,cxi,n_cord,x_cord
     1,cord
     1,x_min,x_max,nb,ixb,nxb,xb,nv,ixv,nxv,xv
     1,ncv,xcv,nc,ixc,nxc,xc,nx_vel,x0_vel,dx_vel,z_datum,ng,z_lay)
c  read new coordinate
      implicit none
      integer   icx,n_cord,nb,nv,ncv,nc,nx_vel,ng
      integer   ixb(1),nxb(1),ixv(1),nxv(1),ixc(1),nxc(1)
      real      xv(1),xb(1),xcv(1),xc(1)
      real      x_cord,x_min,x_max,x0_vel,dx_vel,z_datum,z_lay
      character *16 cxi,cxo,cord(*),mod_type

      integer   i_cord
      character title(3)*20
      data title/' Enter x coordinate',' Enter y coordinate'
     1,' Enter z coordinate'/

    1 continue
      cxo = cxi
      call rmodrstr(title(icx),cxo)

      if ( cxi .eq. 'TIME' .and. cxo .eq. 'DEPTH'
     1.or. cxi .eq. 'DEPTH' .and. cxo .eq. 'TIME')
     1print'('' this will only do a linear transformation.'')'

      call rmodfcor(i_cord,cxo,n_cord,cord)
      if (i_cord .le. 0) then
        print'('' could not find this coordinate in list '',a16)',cxo
        call rmodpcor(6,n_cord,x_cord,cord)
        goto 1
      endif

      call rmod_linear_transform2(mod_type,icx,cxi,cxo,n_cord,x_cord
     1,cord
     1,x_min,x_max,nb,ixb,nxb,xb,nv,ixv,nxv,xv
     1,ncv,xcv,nc,ixc,nxc,xc,nx_vel,x0_vel,dx_vel,z_datum,ng,z_lay)

      return
      end

c23456789012345678901234567890123456789012345678901234567890123456789012
      subroutine rmod_linear_transform2(mod_type,icx,cxi,cxo
     1,n_cord,x_cord,cord,x_min,x_max,nb,ixb,nxb,xb,nv,ixv,nxv,xv
     1,ncv,xcv,nc,ixc,nxc,xc,nx_vel,x0_vel,dx_vel,z_datum,ng,z_lay)
c  read new coordinate
      implicit none
      integer   icx,n_cord,nb,nv,ncv,nc,nx_vel,ng
      integer   ixb(1),nxb(1),ixv(1),nxv(1),ixc(1),nxc(1)
      real      xv(1),xb(1),xcv(1),xc(1)
      real      x_cord,x_min,x_max,x0_vel,dx_vel,z_datum,z_lay
      character *16 cxi,cxo,cord(*),mod_type

      integer   i_cord

      call rmodfcor(i_cord,cxo,n_cord,cord)
      if (i_cord .le. 0) then
        print'('' could not find this coordinate in list '',a16)',cxo
        call rmodpcor(6,n_cord,x_cord,cord)
        return
      endif

c  convert min,max
      call rmodcnmm(x_min,x_max,cxi,cxo,n_cord,x_cord,cord)

c  convert boundaries
      if (nb .gt. 0)
     1call rmodcnvn(ixb(nb)+nxb(nb),xb,cxi,cxo,n_cord,x_cord,cord)

c  convert velocities
      if (nv .gt. 0 .and. mod_type .ne. 'G3DL')
     1call rmodcnvn(ixv(nv)+nxv(nv),xv,cxi,cxo,n_cord,x_cord,cord)
      if (nv .gt. 0 .and. mod_type .eq. 'G3DL')
     1call rmodcnvn(nv,xv,cxi,cxo,n_cord,x_cord,cord)

c  convert cells
      if (nc .gt. 0)
     1call rmodcnvn(ixc(nc)+nxc(nc),xc,cxi,cxo,n_cord,x_cord,cord)

c  convert cell pointers
      call rmodcnvn(ncv,xcv,cxi,cxo,n_cord,x_cord,cord)

c  convert grid coordinates
      call rmodcnv0(nx_vel,x0_vel,dx_vel,cxi,cxo,n_cord,x_cord,cord)
      if (icx .eq. 3)
     1call rmodcnvn(1,z_datum,cxi,cxo,n_cord,x_cord,cord)

c  convert grid depths if depth and G3DL
      if (icx .eq. 3 .and. mod_type .eq. 'G3DL')
     1call rmodcnvn(ng,z_lay,cxi,cxo,n_cord,x_cord,cord)

      cxi = cxo

      return
      end

c23456789012345678901234567890123456789012345678901234567890123456789012
      subroutine rmod_linear_transform3(mod_type,cxi,cyi,czi,cxo,cyo,czo
     1,n_cord,x_cord,cord
     1,x_min,x_max,y_min,y_max,z_min,z_max,z_datum
     1,nb,ixb,nxb,xb,zb,nv,ixv,nxv,xv,yv,zv
     1,ncv,xcv,ycv,zcv,nc,ixc,nxc,xc,zc
     1,nx_vel,x0_vel,dx_vel
     1,ny_vel,y0_vel,dy_vel
     1,nz_vel,z0_vel,dz_vel
     1,z_lay)
      implicit none
      integer   n_cord,nb,nv,ncv,nc,nx_vel,ny_vel,nz_vel,ng
      integer   ixb(1),nxb(1),ixv(1),nxv(1),ixc(1),nxc(1)
      real      xb(1),xv(1),xcv(1),xc(1)
      real      yv(1),ycv(1),zv(1),zcv(1)
      real      zb(1),zc(1)
      real      x_cord,x_min,x_max,y_min,y_max,z_min,z_max
      real      x0_vel,dx_vel,y0_vel,dy_vel,z0_vel,dz_vel,z_datum,z_lay
      character *16 cxi,cyi,czi,cxo,cyo,czo,cord(*),mod_type

c  x
      call rmod_linear_transform2(mod_type,1,cxi,cxo,n_cord,x_cord,cord
     1,x_min,x_max,nb,ixb,nxb,xb,nv,ixv,nxv,xv
     1,ncv,xcv,nc,ixc,nxc,xc,nx_vel,x0_vel,dx_vel,z_datum
     1,nx_vel*ny_vel*nz_vel,z_lay)
c  y
      call rmod_linear_transform2(mod_type,2,cyi,cyo,n_cord,x_cord,cord
     1,y_min,y_max,0,ixb,nxb,xb,nv,ixv,nxv,yv
     1,ncv,ycv,0,ixc,nxc,xc,ny_vel,y0_vel,dy_vel,z_datum
     1,nx_vel*ny_vel*nz_vel,z_lay)
c  z
      call rmod_linear_transform2(mod_type,3,czi,czo,n_cord,x_cord,cord
     1,z_min,z_max,nb,ixb,nxb,zb,nv,ixv,nxv,zv
     1,ncv,zcv,nc,ixc,nxc,zc,nz_vel,z0_vel,dz_vel,z_datum
     1,nx_vel*ny_vel*nz_vel,z_lay)

      return
      end

c23456789012345678901234567890123456789012345678901234567890123456789012
      subroutine rmod_flip(a,b)
      c = a
      a = b
      b = c
      return
      end

c23456789012345678901234567890123456789012345678901234567890123456789012
      subroutine rmod_edit_model(mod_type
     1,cxi,cyi,czi,m_cord,n_cord,x_cord,cord
     1,ibw1,ibw2,ibw3,mbh,nbh,ibid,nbseg,b_name,b_color
     1,ivw1,ivw2,ivw3,mvh,nvh,ivid,nvseg,v_name,v_color
     1,icw1,icw2,icw3,mch,nch,icid,ncseg,c_name,c_color
     1,x_min,x_max,y_min,y_max,z_min,z_max,z_datum
     1,mxb,nb,imb,itb,ixb,nxb,xb,zb
     1,nv,imv,itv,ixv,nxv,xv,zv,vel
     1,ncv,icv,xcv,zcv
     1,mv_grd
     1,nx_vel,x0_vel,dx_vel
     1,ny_vel,y0_vel,dy_vel
     1,nz_vel,z0_vel,dz_vel
     1,v_grd
     1,m_work,work)
c  decimate data
      dimension imb(1),itb(1),ixb(1),nxb(1),work(1)
      dimension imv(1),itv(1),ixv(1),nxv(1),xv(1),zv(1),vel(1)
      character mod_type*(*)
     1,b_name(1),b_color(1),v_name(1),v_color(1),c_name(1),c_color(1)
      character crd80*80
      print'(/,'' This option allows you to edit various components of''
     1  ,'' the velocity models.''
     1,/,'' note - you should be carful modifying values as some are ''
     1,'' linked to others.'')'

c  coordinates
      call rmod_edit_transforms(m_cord,n_cord,x_cord,cord,cxi,cyi,czi)

c  model limits
      call rmod_elim(x_min,x_max,y_min,y_max,z_min,z_max,z_datum
     1,mv_grd
     1,nx_vel,x0_vel,dx_vel
     1,ny_vel,y0_vel,dy_vel
     1,nz_vel,z0_vel,dz_vel
     1,v_grd)

c  horizon characteristics
      call rmod_ehor0(ibw1,ibw2,ibw3,mbh,nbh,ibid,nbseg,b_name,b_color
     1            ,icw1,icw2,icw3,mch,nch,icid,ncseg,c_name,c_color
     1            ,ivw1,ivw2,ivw3,mvh,nvh,ivid,nvseg,v_name,v_color)

      if (mod_type .eq. 'LAYER') then

c  decimate
        call rmod_deci(nb,ixb,nxb,xb,zb)

c  fill
        call rmod_fill(nf,mxb,nb,ixb,nxb,xb,zb,m_work,work,*999)

c  change direction
        call rmod_dir0(' change boundary direction? (y,n) '
     1,3,nb,ixb,nxb,xb,zb,itb,vel,work,work(m_work/2))

        call rmod_dir0(' change velocity direction? (y,n) '
     1,4,nv,ixv,nxv,xv,zv,itv,vel,work,work(m_work/2))

c  renumber flag values
        call rmod_renum0(' renumber boundaries? (y,n) '
     1,2,nb,imb,itb,ixb,nxb,work,work(m_work/2))

        call rmod_renum0(' renumber cell pointers? (y,n) '
     1,1,ncv,icv,work,work,work,work,work(m_work/2))

        call rmod_renum0(' renumber velociites? (y,n) '
     1,2,nv,imv,itv,ixv,nxv,work,work(m_work/2))

c  scale numbers
        crd80 = 'N'
        call rmodrstr(' scale any values? (y,n) ',crd80)
        if (crd80 .eq. 'Y') then

          call rmod_scale_grid0(' scale boundary x values? (y,n) '
     1,ixb(nb)+nxb(nb),xb)

          call rmod_scale_grid0(' scale boundary z values? (y,n) '
     1,ixb(nb)+nxb(nb),zb)

          call rmod_scale_grid0(' scale cell pointer x values? (y,n) '
     1,ncv,xcv)

          call rmod_scale_grid0(' scale cell pointer z values? (y,n) '
     1,ncv,zcv)

          call rmod_scale_grid0(' scale velocity x values? (y,n) '
     1,ixv(nv)+nxv(nv),xv)

          call rmod_scale_grid0(' scale velocity z values? (y,n) '
     1,ixv(nv)+nxv(nv),zv)

          call rmod_scale_grid0(' scale velocity values? (y,n) '
     1,ixv(nv)+nxv(nv),vel)
        endif

      elseif (mod_type(1:1) .eq. 'G') then
        call rmod_scale_grid0(' scale velocity grid values? (y,n) '
     1,nx_vel*ny_vel*nz_vel,v_grd)
      endif

      return
  999 continue
      call rmod_pause(' error in rmod_edit_model',' ')
      return
      end

c23456789012345678901234567890123456789012345678901234567890123456789012
      subroutine rmod_edit_transforms(m_cord,n_cord,x_cord,cord
     1,cxi,cyi,czi)
c  add and delete coordinates
      dimension x_cord(2,1)
      character*(*) cxi,cyi,czi,cord(*)
      character crd16*16,crd80*80

      crd80 = 'N'
      call rmodrstr(' edit coordinate transforms? (y,n) ',crd80)
      if (crd80 .ne. 'Y') return

      print'(/,'' This option adds and deletes transform coordinates.''
     1,/,'' you currently have '',i5,'' and may have a maximum of ''
     1,i5,'' coordinates.'')',n_cord,m_cord

c  delete transforms
    2 continue
      call rmodpcor(6,n_cord,x_cord,cord)
      crd16 = 'NONE'
      print'(/,'' Enter name of the coordinate to delete default= ''
     1,a16)',crd16
      read(*,'(a)',err=2,end=21)crd16
      if (crd16 .eq. ' ' .or. crd16 .eq. 'NONE') goto 21
      if (crd16 .eq. cxi .or. crd16 .eq. cyi .or. crd16 .eq. czi) then
        print'('' cannot delete this coordinate''
     1,'' it is one of the three descriptor coordinates.'')'
        goto 2
      else
        call rmodfcor(i_cord,crd16,n_cord,cord)
        if (i_cord .eq. 0) then
          print'('' could not find this coordinate in this list.'')'
        else
          do jcord = i_cord+1 , n_cord
            cord(jcord-1) = cord(jcord)
            x_cord(1,jcord-1) = x_cord(1,jcord)
            x_cord(2,jcord-1) = x_cord(2,jcord)
          enddo    ! do jcord = i_cord+1 , n_cord
        endif
        cord(n_cord) = ' '
        print'('' corrdinate # '',i5,'' deleted'')',i_cord
        n_cord = n_cord - 1
      endif    ! if (crd16 .eq. cxi .or. crd16 .eq. cyi
      goto 2
   21 continue

c  add coordiantes
    1 continue
      if (n_cord .ge. m_cord) then
        print'(/,'' the transform arrays are filled up,''
     1,'' cannot add anymore.'')'
        goto 11
      endif
      call rmodpcor(6,n_cord,x_cord,cord)
      crd16 = 'NONE'
      print'(/,'' Enter name of the coordinate to add default= '',a16)'
     1,crd16
      read(*,'(a)',err=1,end=11)crd16
      if (crd16 .eq. ' ' .or. crd16 .eq. 'NONE') goto 11
      call rmodfcor(i_cord,crd16,n_cord,cord)
      if (i_cord .ne. 0) then
        print'('' cannot add this coordinate, it duplicates''
     1,'' coordinate # '',i5,1x,a16)',i_cord,cord(i_cord)
      else    ! if (i_cord .ne. 0) then
        n_cord = n_cord + 1
        cord(n_cord) = crd16
    3   continue
        x_cord(1,n_cord) = 0.
        x_cord(2,n_cord) = 1.
        print'(/,'' enter the 2 coordinate values default= ''
     1,f10.2,1x,f10.2)',x_cord(1,n_cord),x_cord(2,n_cord)
        crd80 = ' '
        read(*,'(a)',err=3,end=4)crd80
        if (crd80 .ne. ' ') read(crd80,*,err=3,end=4)
     1x_cord(1,n_cord),x_cord(2,n_cord)
    4   continue
      endif    ! if (i_cord .ne. 0) then
      goto 1

   11 continue

      print'('' you may change any of the coordinate values''
     1,/,'' note this will not transform any numbers'')'

      if (cxi .ne. ' ') call rmod_read_string(cxi,cxi
     1,' Enter new value for x coordinates')

      if (cyi .ne. ' ') call rmod_read_string(cyi,cyi
     1,' Enter new value for y coordinates')

      if (czi .ne. ' ') call rmod_read_string(czi,czi
     1,' Enter new value for z coordinates')


      return
      end

c23456789012345678901234567890123456789012345678901234567890123456789012
      subroutine rmod_elim(x_min,x_max,y_min,y_max,z_min,z_max,z_datum
     1,mv_grd
     1,nx_vel,x0_vel,dx_vel
     1,ny_vel,y0_vel,dy_vel
     1,nz_vel,z0_vel,dz_vel
     1,v_grd)
      character crd80*80
      crd80 = 'N'
      call rmodrstr(' edit model limits or datum? (y,n) ',crd80)
      if (crd80 .eq. 'Y') then
        call rmod_elim1('x',x_min,x_max,nx_vel,x0_vel,dx_vel)
        call rmod_elim1('y',y_min,y_max,ny_vel,y0_vel,dy_vel)
        call rmod_elim1('z',z_min,z_max,nz_vel,z0_vel,dz_vel)
    1 continue
        print'('' enter the new datum value default='',f10.2)'
     1,z_datum
        crd80 = ' '
        read(*,'(a)',err=1,end=2)crd80
        if (crd80 .ne. ' ') read(crd80,*,err=1,end=2)z_datum
    2 continue
      endif

      return
      end

c23456789012345678901234567890123456789012345678901234567890123456789012
      subroutine rmod_elim1(mod_type,x_min,x_max,nx_vel,x0_vel,dx_vel)
      character mod_type*(*),crd80*80
    1 continue

      crd80 = ' '
      print'(/,'' enter layered model '',a1
     1,'' limits min,max defaults='',f10.3,1x,f10.3)'
     1,mod_type,x_min,x_max
      read(*,'(a)',err=1,end=2)crd80
      if (crd80 .ne. ' ') read(crd80,*,err=1,end=2)x_min,x_max
    2 continue

      crd80 = ' '
      print'(/
     1,'' enter gridded model limits NUMBER, ORIGIN, INC defaults=''
     1,i5,1x,f10.3,1x,f10.5)',nx_vel,x0_vel,dx_vel
      read(*,'(a)',err=1,end=3)crd80
      if (crd80 .ne. ' ') read(crd80,*,err=1,end=3)nx_vel,x0_vel,dx_vel
    3 continue

      return
      end

c23456789012345678901234567890123456789012345678901234567890123456789012
      subroutine rmod_renum0(title,npar,n,im,it,ix,nx,ixh,nxh)
      character title*(*),crd80*80
      crd80 = 'N'
      call rmodrstr(title,crd80)
      if (crd80 .eq. 'Y') call rmod_renum(npar,n,im,it,ix,nx,ixh,nxh)
      return
      end

c23456789012345678901234567890123456789012345678901234567890123456789012
      subroutine rmod_renum(npar,n,im,it,ix,nx,ixh,nxh)
      dimension im(1),it(1),ix(1),nx(1),ixh(1),nxh(1)
      character crd80*80
      print'('' renumbering n='',i5)',n
      do 1 i = 1 , n
c  set first flag
    2   continue
        crd80 = ' '
        print'(/,'' enter new value for first flag #'',i5
     1,'' default='',i8)',i,im(i)
        read(*,'(a)',err=2,end=3)crd80
        if (crd80 .ne. ' ') read(crd80,*,err=2,end=3)im(i)
    3   continue
c  set second flag
        if (npar .ge. 2) then
          i1 = ix(i) + 1
          call gtol_vel_hor_size(nh,ixh,nxh,nx(i),it(i1))
      print'('' number of different values for second flag='',i5)',nh
          do 4 j = 1 , nh
            j1 = ixh(j) + i1
    6   continue
        crd80 = ' '
        print'(/,'' enter new value for second flag #'',i5,'','',i5
     1,'' default='',i8)',j,nxh(j),it(j1)
        read(*,'(a)',err=6,end=7)crd80
        if (crd80 .ne. ' ') read(crd80,*,err=6,end=7)it(j1)
    7   continue
            do 5 k = j1 , j1+nxh(j)
              it(k) = it(j1)
    5       continue
    4     continue
        endif
    1 continue
      return
      end

c23456789012345678901234567890123456789012345678901234567890123456789012
      subroutine rmod_scale_grid0(title,n,x)
      character title*(*),crd80*80
      crd80 = 'N'
      call rmodrstr(title,crd80)
      if (crd80 .eq. 'Y') call rmod_scale_grid(n,x)
      return
      end

c23456789012345678901234567890123456789012345678901234567890123456789012
      subroutine rmod_scale_grid(n,x)
c  scale numbers
      dimension x(1)
      character crd80*80
    1 continue
      x0 = 0
      x1 = 0
      dx = 1.
      print'(/,'' enter x0,x1,dx - new = x0 + (old - x1) * dx''
     1,/,'' value='',f10.2,'' defaults='',f10.2,1x,f10.2,1x,f10.2)'
     1,x(1),x0,x1,dx
      crd80 = ' '
      read(*,'(a)',err=1,end=2)crd80
      if (crd80 .ne. ' ') read(crd80,*,err=2,end=2)x0,x1,dx
    2 continue
      do 3 i = 1 , n
        x(i) = x0 + (x(i) - x1) * dx
    3 continue
      return
      end

c23456789012345678901234567890123456789012345678901234567890123456789012
      subroutine rmod_ehor0(
     1 ibw1,ibw2,ibw3,mbh,nbh,ibid,nbseg,b_name,b_color
     1,icw1,icw2,icw3,mch,nch,icid,ncseg,c_name,c_color
     1,ivw1,ivw2,ivw3,mvh,nvh,ivid,nvseg,v_name,v_color)
      character b_name(1),b_color(1),v_name(1),v_color(1),c_name(1)
     1,c_color(1)
      character crd80*80

      crd80 = 'N'
      call rmodrstr(' edit horizon characteristics? (y,n) ',crd80)
      if (crd80 .eq. 'Y') then

        call rmod_ehor1(' change boundary names? (y,n) '
     1,ibw1,ibw2,ibw3,mbh,nbh,ibid,nbseg,b_name,b_color)

        call rmod_ehor1(' change cell pointer names? (y,n) '
     1,icw1,icw2,icw3,mch,nch,icid,ncseg,c_name,c_color)

        call rmod_ehor1(' change velocity names? (y,n) '
     1,ivw1,ivw2,ivw3,mvh,nvh,ivid,nvseg,v_name,v_color)
      endif
      return
      end

c23456789012345678901234567890123456789012345678901234567890123456789012
      subroutine rmod_ehor1(title,iw1,iw2,iw3,mh,nh,iid,nseg,name,color)
      character title*(*),name*(*),color*(*),crd80*80
      crd80 = 'N'
      call rmodrstr(title,crd80)
      if (crd80 .eq. 'Y')
     1call rmod_ehor(iw1,iw2,iw3,mh,nh,iid,nseg,name,color)
      return
      end

c23456789012345678901234567890123456789012345678901234567890123456789012
      subroutine rmod_ehor(iw1,iw2,iw3,mh,nh,iid,nseg,name,color)
c edit horizon info
      dimension iid(1),nseg(1)
      character name(1)*16,color(1)*16,crd80*80

      print'('' renaming horizons iw1='',i5,'' iw2='',i5
     1,'' iw3='',i5)',iw1,iw2,iw3
   10 continue
      crd80 = ' '
      print'(/,'' enter number of horizons - max='',i5
     1,'' default='',i5)',mh,nh
      read(*,'(a)',err=10,end=11)crd80
      if (crd80 .ne. ' ') read(crd80,*,err=10,end=11)nh
   11 continue
      nh = min(nh,mh)

      do 1 i = 1 , nh
    2   continue

        crd80 = ' '
        print'(/,'' enter id for horizon '',i5
     1,'' default='',i5)',i,iid(i)
        read(*,'(a)',err=2,end=3)crd80
        if (crd80 .ne. ' ') read(crd80,*,err=2,end=3)iid(i)
    3   continue

        crd80 = ' '
        print'(/,'' enter number of segments default='',i5)',nseg(i)
        read(*,'(a)',err=2,end=4)crd80
        if (crd80 .ne. ' ') read(crd80,*,err=2,end=4)nseg(i)
    4   continue

        crd80 = ' '
        print'(/,'' enter horizon name default='',a16)',name(i)
        read(*,'(a)',err=2,end=5)crd80
        if (crd80 .ne. ' ') read(crd80,'(a)',err=2,end=5)name(i)
    5   continue

        crd80 = ' '
        print'(/,'' enter horizon color default='',a16)',color(i)
        read(*,'(a)',err=2,end=6)crd80
        if (crd80 .ne. ' ') read(crd80,'(a)',err=2,end=6)color(i)
    6   continue

    1 continue
      return
      end

c23456789012345678901234567890123456789012345678901234567890123456789012
      subroutine rmod_layer_to_cell(mod_type,x_min,x_max,z_min,z_max
     1,nb,ixb,nxb,xb,zb
     1,nv,imv,itv,ixv,nxv,xv,zv,vel
     1,ncv,icv,xcv,zcv
     1,mc,mxc,nc,imc,ixc,nxc,xc,zc
     1,m_work,work,i_err)
c  construct cells
      implicit  none
      integer   nb,ixb,nxb,nv,imv,itv,ixv,nxv,ncv,icv
      integer   mc,mxc,nc,imc,ixc,nxc,m_work,i_err
      real      x_min,x_max,z_min,z_max,xb,zb,xv,zv,vel,xcv,zcv,xc,zc
     1,work
      character mod_type*(*)

      integer   lpr

      print'(/,'' This option converts a layered model to''
     1,'' celled format'')'

      if (mod_type .ne. 'LAYER') then
        print'('' Model mod_type must be layer for this option''
     1,'' mod_type= '',a16)',mod_type
        call rmod_pause(' ',' ')
        return
      endif
      call cellclvp(x_min,x_max,z_min,z_max,nb,ixb,nxb,xb,zb
     1,nv,imv,itv,ixv,nxv,xv,zv,vel
     1,ncv,icv,xcv,zcv
     1,mc,mxc,nc,imc,ixc,nxc,xc,zc
     1,m_work,work,i_err)

      mod_type = 'CELL'
      if (i_err .ne. 0) then
        print'(/,'' error in rmod_layer_to_cell i_err= '',i5,'' nc='',i5
     1,'' nb='',i5,'' nv='',i5,'' ncv='',i5)',i_err,nc,nb,nv,ncv
        lpr = 6
        call cellchkp(nv,imv,itv,ixv,nxv,xv,zv,vel
     1,ncv,icv,xcv,zcv,nc,imc,ixc,nxc,xc,zc,work,lpr,i_err,*1)
    1 continue
        if (nc .eq. 0) mod_type = 'LAYER'
        return
      endif

      return
      end

c23456789012345678901234567890123456789012345678901234567890123456789012
      subroutine rmod_layer_to_grid(h_file,mod_type,x_min,x_max,z_min
     1,z_max
     1,nb,ixb,nxb,xb,zb
     1,nv,imv,itv,ixv,nxv,xv,zv,vel
     1,ncv,icv,xcv,zcv
     1,mc,mxc,nc,imc,ixc,nxc,xc,zc
     1,nx_vel,x0_vel,dx_vel,nz_vel,z0_vel,dz_vel,v_grd,m_work,work
     1,i_err)
c  convert layer model to gridded model
      character h_file*(*),mod_type*(*)

      print'(/,'' This option converts a layered model into a''
     1,'' gridded model.'')'

      if (mod_type .ne. 'LAYER') then
        print'('' Model mod_type must be layer for this option''
     1,'' mod_type= '',a16)',mod_type
        call rmod_pause(' ',' ')
        return
      endif

      call rmod_layer_to_cell(mod_type,x_min,x_max,z_min,z_max,nb,ixb
     1,nxb
     1,xb,zb
     1,nv,imv,itv,ixv,nxv,xv,zv,vel
     1,ncv,icv,xcv,zcv
     1,mc,mxc,nc,imc,ixc,nxc,xc,zc
     1,m_work,work,i_err)
      if (i_err .ne. 0) return

      call rmod_cell_to_grid(h_file,mod_type
     1,nv,imv,itv,ixv,nxv,xv,zv,vel,nc,imc,ixc,nxc,xc,zc
     1,nx_vel,x0_vel,dx_vel,nz_vel,z0_vel,dz_vel,v_grd,m_work,work
     1,i_err)

c      call rmod_apend_close    ! reset apend section

      return
      end


c23456789012345678901234567890123456789012345678901234567890123456789012
      subroutine rmod_cell_to_grid(h_file,mod_type
     1,nv,imv,itv,ixv,nxv,xv,zv,vel,nc,imc,ixc,nxc,xc,zc
     1,nx_vel,x0_vel,dx_vel,nz_vel,z0_vel,dz_vel,v_grd,m_work,work
     1,i_err)
c  convert layer model to gridded model
      implicit  none
      integer   ixv(1),nxv(1)
      integer   nv,imv,itv,nc,imc,ixc,nxc,nx_vel,nz_vel,m_work,i_err,lh
      real      xv,zv,vel,xc,zc,x0_vel,dx_vel,z0_vel,dz_vel,v_grd(1)
     1,work
      character h_file*(*),mod_type*(*)

      integer   nxinc,nzinc,nxinc0,nzinc0,i
      character ans*1,crd80*80

      print'(/,'' This option converts a celled model into a''
     1,'' gridded model.'')'

      if (mod_type .ne. 'CELL') then
        print'('' Model mod_type must be cell for this option''
     1,'' mod_type='',a16)',mod_type
        call rmod_pause(' ',' ')
        return
      endif

      call rmod_ans(ans,'V','V','C'
     1,' Is this a velocity or constraint file?')

c  get the smoothing settings for GTOL
      call gtol_get_nx_terp(nxinc0)
      call gtol_get_nz_terp(nzinc0)

      if (ans .eq. 'V') THEN
        nxinc = nxinc0
        nzinc = nzinc0
        print'('' Treating this as a velocity file, ''
     1,'' smoothing slowness.'')'
      else
        nxinc = 1
        nzinc = 1
        print'('' Treating this as a constraint file, ''
     1,'' with no smoothing.'')'
      endif

c    1 continue
c      print'(/,'' Enter the level of spatial smoothing ''
c     1,'' in the x,z directions defaults='',i8,1x,i8
c     1,/,'' 1 = no smoothing, fast, less accurate velocities.''
c     1,/,'' 5 =    smoothing, slow, more accurate velocities.''
c     1,/,'' Use 1 for tomography constraint grids.'')'
c     1,nxinc,nzinc
c      crd80 = ' '
c      read(*,'(a)',end=2)crd80
c      if (crd80 .ne. ' ') read(crd80,*,end=2,err=1)nxinc,nzinc
c    2 continue

c  reset the smoothing settings for GTOL
      call gtol_put_nx_terp(nxinc)
      call gtol_put_nz_terp(nzinc)

c  convert the layered velocity to slowness
      if (ans .eq. 'V') call util_invert(ixv(nv)+nxv(nv),vel)

c  convert from cell to grid
      call gtol(+1,nv,imv,itv,ixv,nxv,xv,zv,vel,nc,imc,ixc,nxc,xc,zc
     1,nx_vel,x0_vel,dx_vel,nz_vel,z0_vel,dz_vel,v_grd,m_work,work
     1,i_err)

c  convert the layered velocity to velocity
      if (ans .eq. 'V') then
        call util_invert(ixv(nv)+nxv(nv),vel)
      else
c        do i = 1 , nx_vel*nz_vel
c          v_grd(i) = float(nint(v_grd(i)))
c        enddo
        call util_invert(nx_vel*nz_vel,v_grd)
      endif

      if (i_err .ne. 0) then
        print'(/,'' error in rmod_cell_to_grid i_err= '',i5)',i_err
        return
      endif

c  reset the model mod_type
      mod_type = 'GRID'

c  reset the smoothing settings for GTOL back to the original vlaues
      call gtol_put_nx_terp(nxinc0)
      call gtol_put_nz_terp(nzinc0)

      crd80 = ' '
      write(crd80,'(a)')' RMOD transforming layer file to grid file '
      call rmod_title_to_history(crd80)
      crd80 = ' '
      if (ans .eq. 'V') THEN
        write(crd80,'(a)')
     1' Treating this as a velocity file, smoothing slowness.'
      else
        write(crd80,'(a)')
     1' Treating this as a constraint file, with no smoothing.'
      endif
      call rmod_card_to_history(crd80)


      return
      end

c23456789012345678901234567890123456789012345678901234567890123456789012
      subroutine rmod_grid_to_layer(h_file,mod_type
     1,cxo,cyo,czo,m_cord,n_cord,x_cord,cord
     1,x_min,x_max,y_min,y_max,z_min,z_max,z_datum
     1,ibw1,ibw2,ibw3,mbh,nbh,ibid,nbseg,b_name,b_color
     1,ivw1,ivw2,ivw3,mvh,nvh,ivid,nvseg,v_name,v_color
     1,icw1,icw2,icw3,mch,nch,icid,ncseg,c_name,c_color
     1,mb,mxb,nb,imb,itb,ixb,nxb,xb,zb,nv,imv,itv,ixv,nxv,xv,yv,zv,vel
     1,ncv,icv,xcv,ycv,zcv,mc,mxc,nc,imc,ixc,nxc,xc,zc
     1,mv_grd
     1,nx_vel,x0_vel,dx_vel
     1,ny_vel,y0_vel,dy_vel
     1,nz_vel,z0_vel,dz_vel
     1,v_grd
     1,m_work,work,lu)
c  convert gridded model to layered model
      character *(*) h_file,mod_type,cxo,cyo,czo,cord(*)
     1,b_name(*),b_color(*),v_name(*),v_color(*),c_name(*),c_color(*)
      character *64 d_file,word_type,m_file,t_file
      character *16 cxi,cyi,czi,omod_type
      character bufd*9,buft*8
      character lfile*80,crd80*80
      integer ixv(1),nxv(1)
      integer i_err

      i_err = 0

      print'(/,'' This option converts a gridded model''
     1,/,'' into a layered model.''
     1,/,'' It uses a layered overlay model to describe boundaries''
     1,/,'' and velocity structure and does a least squares fit to''
     1,/,'' determine the velocity values''
     1,/,'' Note the output model grid and layer limits will be those''
     1,'' of the layered model.'')'

c      print'('' model mod_type= '',a16)',mod_type
      if (mod_type .ne. 'GRID') then
        print'('' Model mod_type must be grid for this option''
     1,'' mod_type='',a16)',mod_type
        call rmod_pause(' ',' ')
        return
      endif

      call rmod_reverse_negative_increment(mod_type
     1,nx_vel,x0_vel,dx_vel
     1,ny_vel,y0_vel,dy_vel
     1,nz_vel,z0_vel,dz_vel
     1,vel
     1)

      call util_copy_file_name(h_file,lfile)
      call util_get_file_name(' Enter layered overlay model'
     1,lfile,'mod')

c      print'('' bef rdhd''
c     1,/,'' cxi='',a16,'' cxo='',a16
c     1,/,'' czi='',a16,'' czo='',a16)',cxi,cxo,czi,czo
c      print*,' x_min=',x_min,x_max,z_min,z_max
c      print*,' nx_vel=',nx_vel,x0_vel,dx_vel
c      print*,' nz_vel=',nz_vel,z0_vel,dz_vel
      ndim = 3
      call rmodrdhd(lfile,d_file,word_type,m_file,t_file,omod_type
     1,cxi,cyi,czi,cxo,cyo,czo,m_cord,n_cord,x_cord,cord
     1,x_min,x_max,y_min,y_max,z_min,z_max,z_datum
     1,nxo,xomin,xoinc,nyo,yomin,yoinc,nzo,zomin,zoinc,ndim,lu,*999)

      if (omod_type .ne. 'LAYER') then
        print'('' overlay file must be layer omod_type=
     1 '',a16)',omod_type
        return
      endif

      makec = 1
      call rmodrall(lfile,d_file,word_type,m_file,t_file,omod_type
     1,cxi,cyi,czi,cxo,cyo,czo,m_cord,n_cord,x_cord,cord
     1,x_min,x_max,y_min,y_max,z_min,z_max,z_datum
     1,ibw1,ibw2,ibw3,mbh,nbh,ibid,nbseg,b_name,b_color
     1,ivw1,ivw2,ivw3,mvh,nvh,ivid,nvseg,v_name,v_color
     1,icw1,icw2,icw3,mch,nch,icid,ncseg,c_name,c_color
     1,mb,mxb,nb,imb,itb,ixb,nxb,xb,zb,nv,imv,itv,ixv,nxv,xv,yv,zv,vel
     1,ncv,icv,xcv,ycv,zcv,mc,mxc,nc,imc,ixc,nxc,xc,zc
     1,mv_grd,nxo,xomin,xoinc,nyo,yomin,yoinc,nzo,zomin,zoinc,v_grd
     1,makec,m_work,work,lu,*999)

      call rmod_file_to_apend(lfile)     ! add apend
c  add to the history
      crd80 = ' '
      write(crd80,'(a)')' RMOD transforming grid file to layer file '
      call rmod_title_to_history(crd80)
      call rmodlenr(lh,h_file)
      crd80 = ' '
      write(crd80,'('' grid  file name:'',a)')h_file(1:lh)
      call rmod_card_to_history(crd80)
      call rmodlenr(lh,lfile)
      crd80 = ' '
      write(crd80,'('' layer file name:'',a)')lfile(1:lh)
      call rmod_card_to_history(crd80)

      call gtol(-1,nv,imv,itv,ixv,nxv,xv,zv,vel,nc,imc,ixc,nxc,xc,zc
     1,nx_vel,x0_vel,dx_vel,nz_vel,z0_vel,dz_vel,v_grd,m_work,work
     1,i_err)

      call util_invert(ixv(nv)+nxv(nv),vel)

      if (i_err .ne. 0) then
        print'(/,'' error in rmod_grid_to_layer i_err= '',i5)',i_err
        return
      endif

      nx_vel = nxo
      x0_vel = xomin
      dx_vel = xoinc
      ny_vel = nyo
      y0_vel = yomin
      dy_vel = yoinc
      nz_vel = nzo
      z0_vel = zomin
      dz_vel = zoinc
      mod_type = 'LAYER'

      return

  999 continue
      call rmod_pause(' error in rmod_grid_to_layer',' ')
      return

      end

c23456789012345678901234567890123456789012345678901234567890123456789012
      subroutine rmod_time_to_depth(h_file,mod_type
     1,cxi,cyi,czi,m_cord,n_cord,x_cord,cord
     1,x_min,x_max,y_min,y_max,z_min,z_max,z_datum
     1,nb,imb,itb,ixb,nxb,xb,zb,nv,imv,itv,ixv,nxv,xv,yv,zv,vel
     1,ncv,icv,xcv,ycv,zcv,nc,imc,ixc,nxc,xc,zc
     1,mv_grd
     1,nx_vel,x0_vel,dx_vel
     1,ny_vel,y0_vel,dy_vel
     1,nz_vel,z0_vel,dz_vel
     1,v_grd,z_lay
     1,m_work,work,lu)
c  convert model from time to depth or reverse
      implicit none
      integer m_cord,n_cord,nb,imb,itb,ixb,nxb,nv,imv,itv,ixv,nxv
      integer ncv,icv,xcv,ycv,zcv,nc,imc,ixc,nxc,mv_grd,nx_vel,ny_vel
     1,nz_vel
      integer m_work,lu
      real x_cord,x_min,x_max,y_min,y_max,z_min,z_max,z_datum
      real xb,zb,xv,yv,zv,vel,xc,zc
      real x0_vel,dx_vel,y0_vel,dy_vel,z0_vel,dz_vel,v_grd,z_lay
     1,work(m_work)
      character *(*) h_file,mod_type,cxi,cyi,czi,cord(*)
      character *64 d_file,word_type,m_file,t_file,h_file0
      character *16 cxv,cyv,czv,cz_lay,omod_type,cz1,czo
      character *80 crd80
      integer nxmix,nymix,nzmix,ndim,nx_vel1,ny_vel1,nz_vel1,lh,lt
      real x_min1,x_max1,y_min1,y_max1,z_min1,z_max1,z_datum1
      real x0_vel1,dx_vel1,y0_vel1,dy_vel1,z0_vel1,dz_vel1
      integer i_err

      data nxmix,nymix,nzmix/1,1,1/

      print'(/,'' This option converts from time to depth using''
     1,'' a velocity model''
     1,/,'' For gridded models it does an direct conversion.''
     1,/,'' For layered models you must use a gridded model ''
     1,''to convert with.''
     1,/,'' This may or may not be the same as the layered model''
     1,/,'' current vertical coordinate='',a)',czi

      if (mod_type .ne. 'LAYER' .and. mod_type .ne. 'GRID'
     1.and. mod_type .ne. 'G3DL') then
        print'('' Model mod_type must be layer or grid for this option''
     1,'' mod_type= '',a16)',mod_type
        call rmod_pause(' ',' ')
        return
      endif

c  set the ouput vertical coordinate
      cz_lay = 'DEPTH'
      cz1 = czi
      if (czi .eq. 'TIME') then
         czo = 'DEPTH'
      else
         czo = 'TIME'
      endif
      print'(/,'' converting from '',a16,'' to '',a16)',czi,czo

c  if the vertical coordinate is not TIME or DEPTH convert it to DEPTH
c  via a linear transform
      if (czi .ne. 'TIME' .and. czi .ne. 'DEPTH') then

        print'(/,'' converting from '',a16
     1,'' to DEPTH via linear transform'')',czi
        call rmod_linear_transform2(mod_type,3,czi,cz_lay,n_cord,x_cord
     1,cord
     1,z_min,z_max,nb,ixb,nxb,zb,nv,ixv,nxv,zv
     1,ncv,zcv,nc,ixc,nxc,zc,nz_vel,z0_vel,dz_vel,z_datum
     1,nx_vel*ny_vel*nz_vel,z_lay)

      endif

      if (mod_type .eq. 'GRID') then   ! convert gridded model

c  if the input model is a grid
c  use that same grid for the time to depth conversion
        call rmodcnmm(z_min,z_max,czi,czo,n_cord,x_cord,cord)
        call ztot_convert_velocity_n(czi,n_cord,x_cord,cord,z_datum
     1,nx_vel*ny_vel,nz_vel,z0_vel,dz_vel,v_grd,m_work,work,*999)
        h_file0 = h_file

c  if the input model is a layered model read model for conversion
      else    ! if (mod_type .eq. 'GRID') then   - 2d, 3d layered mdoels

        print'(/,'' Enter the name of the velocity model''
     1,'' to use for the conversion.''
     1,/,'' This will be converted to a gridded model''
     1,/,'' before transforming the input layered model.'')'
        call util_copy_file_name(h_file,h_file0)
        call util_get_file_name(' Enter model for conversion'
     1,h_file0,'mod')

c        ndim = 3
c        call rmodrdhd(h_file0,d_file,word_type,m_file,t_file,omod_type
c     1,cxv,cyv,czv,cxi,cyi,cz_lay,m_cord,n_cord,x_cord,cord
c     1,x_min1,x_max1,y_min1,y_max1,z_min1,z_max1,z_datum1
c     1,nx_vel1,x0_vel1,dx_vel1,ny_vel1,y0_vel1,dy_vel1,nz_vel1,z0_vel1,
c     1,ndim,lu,*999)
c     1,cxv,cyv,czv,cxv,cyv,cz_lay,m_cord,n_cord,x_cord,cord

c  read the gridded velocity - note z_lay is used for gridded vleocity
        call rmodrgrd(h_file0,omod_type
     1,cxv,cyv,czv,cxi,cyi,cz_lay,m_cord,n_cord,x_cord,cord
     1,x_min1,x_max1,y_min1,y_max1,z_min1,z_max1,z_datum1
     1,mv_grd,nx_vel1,x0_vel1,dx_vel1,ny_vel1,y0_vel1,dy_vel1,nz_vel1
     1,z0_vel1,dz_vel1
     1,z_lay,m_work,work,lu
     1,i_err)
      if (i_err .ne. 0) goto 999

        if (czv .ne. 'TIME' .and. czv .ne. 'DEPTH')
     1print'(/,'' converting conversion model from '',a16
     1,'' to DEPTH via linear transform'')',czv

c  convert a 3d layered model
        if (mod_type .eq. 'G3DL') then

          call ztot_time_to_depth_g3dl(czi,n_cord,x_cord,cord
     1,z_min,z_max,z_datum
     1,ncv,xcv,ycv,zcv,nv,xv,yv,zv
     1,nx_vel,x0_vel,dx_vel
     1,ny_vel,y0_vel,dy_vel
     1,nz_vel,z0_vel,dz_vel
     1,v_grd,v_grd
     1,nx_vel1,x0_vel1,dx_vel1,ny_vel1,y0_vel1,dy_vel1,nz_vel1,z0_vel1
     1,dz_vel1,z_lay)

c  convert a 2d layered model
        else    ! if (mod_type .eq. 'G3DL') then

          call ztot(czi,n_cord,x_cord,cord,z_min,z_max,z_datum
     1,nb,ixb,nxb,xb,zb,nv,ixv,nxv,xv,zv,ncv,xcv,zcv,nz_vel,z0_vel
     1,dz_vel
     1,nx_vel1,x0_vel1,dx_vel1,nz_vel1,z0_vel1,dz_vel1,z_lay)

        endif    ! if (mod_type .eq. 'G3DL') then

      endif    ! if (mod_type .eq. 'GRID') then   ! convert gridded mode

c      write(crd80,'('' RMOD converting from '',a16,'' to  '',a16
c     1,'' model mod_type:'',a16)')cz1,czo,mod_type
      crd80 = ' '
      write(crd80,'('' RMOD converting from '',a16,'' to  '',a16
     1)')cz1,czo
      call rmod_title_to_history(crd80)
      call rmodlenr(lt,mod_type)
      crd80 = ' '
      write(crd80,'('' model mod_type:'',a16)')mod_type(1:lt)
      call rmod_card_to_history(crd80)
      call rmodlenr(lh,h_file)
      crd80 = ' '
      write(crd80,'('' converted model file:'',a)')h_file(1:lh)
      call rmod_card_to_history(crd80)
      if (mod_type .ne. 'GRID') then
        call rmodlenr(lh,h_file0)
        crd80 = ' '
        write(crd80,'('' model file used for conversion :'',a)')
     1h_file0(1:lh)
        call rmod_card_to_history(crd80)
      endif

      return
  999 continue
      call rmod_pause(' error in rmod_time_to_depth',' ')
      return

      end

c23456789012345678901234567890123456789012345678901234567890123456789012
      subroutine rmod_time_to_depth_new(
     1 h_file,mod_type
     1,cxi,cyi,czi,m_cord,n_cord,x_cord,cord
     1,x_min,x_max,y_min,y_max,z_min,z_max,z_datum
     1,nb,imb,itb,ixb,nxb,xb,zb,nv,imv,itv,ixv,nxv,xv,yv,zv,vel
     1,ncv,icv,xcv,ycv,zcv,nc,imc,ixc,nxc,xc,zc
     1,mv_grd
     1,nx_vel,x0_vel,dx_vel
     1,ny_vel,y0_vel,dy_vel
     1,nz_vel,z0_vel,dz_vel
     1,v_grd,v_con
     1,m_work,work,lu)
c  convert model from time to depth or reverse
      implicit none
      integer m_cord,n_cord,nb,imb,itb,ixb,nxb,nv,imv,itv,ixv,nxv
      integer ncv,icv,xcv,ycv,zcv,nc,imc,ixc,nxc,mv_grd,nx_vel,ny_vel
     1,nz_vel
      integer m_work,lu
      real x_cord,x_min,x_max,y_min,y_max,z_min,z_max,z_datum
      real xb,zb,xv,yv,zv,vel,xc,zc
      real x0_vel,dx_vel,y0_vel,dy_vel,z0_vel,dz_vel,v_grd,v_con
     1,work(m_work)
      character *(*) h_file,mod_type,cxi,cyi,czi,cord(*)
      character *64 d_file,word_type,m_file,t_file,h_file0
      character *16 cxv,cyv,czv,cz_lay,omod_type,cz1,czo
      character *80 crd80
      integer nxmix,nymix,nzmix,ndim,nx_con,ny_con,nz_con,lh,lt
      real x_min1,x_max1,y_min1,y_max1,z_min1,z_max1,z_datum1
      real x0_con,dx_con,y0_con,dy_con,z0_con,dz_con
      integer i_err

      data nxmix,nymix,nzmix/1,1,1/

      print'(/,'' This option converts from time to depth using''
     1,'' a velocity model''
     1,/,'' For gridded models it does an direct conversion.''
     1,/,'' For layered models you must use a gridded model ''
     1,''to convert with.''
     1,/,'' This may or may not be the same as the layered model''
     1,/,'' current vertical coordinate='',a)',czi

      if (mod_type .ne. 'LAYER' .and. mod_type .ne. 'GRID'
     1.and. mod_type .ne. 'G3DL') then
        print'('' Model mod_type must be layer or grid for this option''
     1,'' mod_type= '',a16)',mod_type
        call rmod_pause(' ',' ')
        return
      endif

c  set the ouput vertical coordinate
      cz1 = czi
      if (czi .eq. 'TIME') then
         czo = 'DEPTH'
      else
         czo = 'TIME'
      endif
      print'(/,'' converting from '',a16,'' to '',a16)',czi,czo
      if (mod_type .eq. 'GRID') then   ! convert gridded model
      cz_lay = czi
      else    ! if (mod_type .eq. 'GRID') then   ! convert gridded model
      cz_lay = 'DEPTH'
      endif    ! if (mod_type .eq. 'GRID') then   ! convert gridded model

c  if the vertical coordinate is not TIME or DEPTH convert it to DEPTH
c  via a linear transform
      if (czi .ne. 'TIME' .and. czi .ne. 'DEPTH') then

        print'(/,'' converting from '',a16
     1,'' to DEPTH via linear transform'')',czi
        call rmod_linear_transform2(mod_type,3,czi,cz_lay,n_cord,x_cord
     1,cord
     1,z_min,z_max,nb,ixb,nxb,zb,nv,ixv,nxv,zv
     1,ncv,zcv,nc,ixc,nxc,zc,nz_vel,z0_vel,dz_vel,z_datum
     1,nx_vel*ny_vel*nz_vel,v_con)

      endif

        print'(/,'' Enter the name of the velocity model''
     1,'' to use for the conversion.''
     1,/,'' This will be converted to a gridded model''
     1,/,'' before transforming the input layered model.'')'
        call util_copy_file_name(h_file,h_file0)
        call util_get_file_name(' Enter model for conversion'
     1,h_file0,'mod')

c  read the gridded velocity - note v_con is used for gridded vleocity
        call rmodrgrd(h_file0,omod_type
     1,cxv,cyv,czv,cxi,cyi,cz_lay,m_cord,n_cord,x_cord,cord
     1,x_min1,x_max1,y_min1,y_max1,z_min1,z_max1,z_datum1
     1,mv_grd
     1,nx_con,x0_con,dx_con
     1,ny_con,y0_con,dy_con
     1,nz_con,z0_con,dz_con
     1,v_con,m_work,work,lu
     1,i_err)
      if (i_err .ne. 0) goto 999

        if (czv .ne. 'TIME' .and. czv .ne. 'DEPTH')
     1print'(/,'' converting conversion model from '',a16
     1,'' to DEPTH via linear transform'')',czv

      if (mod_type .eq. 'GRID') then   ! convert gridded model

c  if the input model is a grid
c  use that same grid for the time to depth conversion
        call rmodcnmm(z_min,z_max,czi,czo,n_cord,x_cord,cord)

        call ztot_convert_trace_n(
     1 czi,n_cord,x_cord,cord,z_datum
     1,nx_vel,x0_vel,dx_vel
     1,ny_vel,y0_vel,dy_vel
     1,nz_vel,z0_vel,dz_vel
     1,v_grd
     1,nx_con,x0_con,dx_con
     1,ny_con,y0_con,dy_con
     1,nz_con,z0_con,dz_con
     1,v_con
     1,m_work,work,*999)
        h_file0 = h_file

c  if the input model is a layered model read model for conversion
      else    ! if (mod_type .eq. 'GRID') then   - 2d, 3d layered mdoels

c  convert a 3d layered model
        if (mod_type .eq. 'G3DL') then

          call ztot_time_to_depth_g3dl(czi,n_cord,x_cord,cord
     1,z_min,z_max,z_datum
     1,ncv,xcv,ycv,zcv,nv,xv,yv,zv
     1,nx_vel,x0_vel,dx_vel
     1,ny_vel,y0_vel,dy_vel
     1,nz_vel,z0_vel,dz_vel
     1,v_grd,v_grd
     1,nx_con,x0_con,dx_con,ny_con,y0_con,dy_con,nz_con,z0_con
     1,dz_con,v_con)

c  convert a 2d layered model
        else    ! if (mod_type .eq. 'G3DL') then

          call ztot(czi,n_cord,x_cord,cord,z_min,z_max,z_datum
     1,nb,ixb,nxb,xb,zb,nv,ixv,nxv,xv,zv,ncv,xcv,zcv,nz_vel,z0_vel
     1,dz_vel
     1,nx_con,x0_con,dx_con,nz_con,z0_con,dz_con,v_con)

        endif    ! if (mod_type .eq. 'G3DL') then

      endif    ! if (mod_type .eq. 'GRID') then   ! convert gridded mode

c      write(crd80,'('' RMOD converting from '',a16,'' to  '',a16
c     1,'' model mod_type:'',a16)')cz1,czo,mod_type
      crd80 = ' '
      write(crd80,'('' RMOD converting from '',a16,'' to  '',a16
     1)')cz1,czo
      call rmod_title_to_history(crd80)
      call rmodlenr(lt,mod_type)
      crd80 = ' '
      write(crd80,'('' model mod_type:'',a16)')mod_type(1:lt)
      call rmod_card_to_history(crd80)
      call rmodlenr(lh,h_file)
      crd80 = ' '
      write(crd80,'('' converted model file:'',a)')h_file(1:lh)
      call rmod_card_to_history(crd80)
      if (mod_type .ne. 'GRID') then
        call rmodlenr(lh,h_file0)
        crd80 = ' '
        write(crd80,'('' model file used for conversion :'',a)')
     1h_file0(1:lh)
        call rmod_card_to_history(crd80)
      endif

      return
  999 continue
      call rmod_pause(' error in rmod_time_to_depth',' ')
      return

      end

c23456789012345678901234567890123456789012345678901234567890123456789012
      subroutine rmod_layer_to_sierra(mod_type,x_min,x_max,z_min,z_max
     1,nb,ixb,nxb,xb,zb,ml,mxl,nl,ivl,ixl,nx_lay,xl,zl
     1,nv,imv,itv,ixv,nxv,xv,zv,vel
     1,ncv,icv,xcv,zcv
     1,m_work,work,i_err)
c  convert gridded model to layered model
      character *(*) mod_type

      print'(/,'' This option converts a 2d layered model into a 2d''
     1,'' SIERRA model.''
     1,/,'' note the model must be complete with velocites'')'

      if (mod_type .ne. 'LAYER') then
        print'('' Model mod_type must be layer for this option''
     1,'' mod_type= '',a16)',mod_type
        call rmod_pause(' ',' ')
        return
      endif

c      subroutine layr(x_min,x_max,z_min,z_max,nb,ixb,nxb,xb,zb
c     1,ml,mxl,nl,ivl,ixl,nx_lay,xl,zl,nv,imv,itv,ixv,nxv,xv,zv,vel
c     1,ncv,icv,xcv,zcv,m_work,work,i_err)
      print*,' nb=',nb,' nv=',nv,' ncv=',ncv
     1,' ml=',ml,' mxl=',mxl,' mw=',m_work

      call layr(x_min,x_max,z_min,z_max,nb,ixb,nxb,xb,zb
     1,ml,mxl,nl,ivl,ixl,nx_lay,xl,zl,nv,imv,itv,ixv,nxv,xv,zv,vel
     1,ncv,icv,xcv,zcv,m_work,work,i_err)
      call cellpplt(nl,ixl,nx_lay,xl,zl,21,'layr xx',0)

      if (i_err .ne. 0) then
        print'(/,'' error in rmod_layer_to_sierra i_err= '',i5)',i_err
        return
      endif

      mod_type = 'SIERRA'

c        call laysra(lu,srafil,units,torz,x_min,x_max,z_min,z_max
c     1,nl,ivl,ixl,nx_lay,xl,zl,nv,itv,ixv,nxv,xv,zv,vel)
      print*,' all done with layr nl=',nl,' i_err=',i_err
      return
      end

c23456789012345678901234567890123456789012345678901234567890123456789012
      subroutine rmod_rmix(nxmix,nymix,nzmix)
      character crd80*80
      nxmix = 1
      nymix = 1
      nzmix = 1

      print'(/,'' enter the number of nodes to smooth over ''
     1,'' in x,y and z directions''
     1,/,'' defaults= '',i8,1x,i8,1x,i8)',nxmix,nymix,nzmix
      crd80 = ' '
      read(*,'(a)',end=1)crd80
      if (crd80 .ne. ' ') read(crd80,*,err=1,end=1)nxmix,nymix,nzmix
    1 continue
      return
      end

c23456789012345678901234567890123456789012345678901234567890123456789012
      subroutine rmod_movev(nv,imv,ncv,icv,xcv,zcv,icv0,xcv0,zcv0)
c  arrange velocity cards
      dimension imv(1),icv(1),xcv(1),zcv(1),icv0(1),xcv0(1),zcv0(1)
      call util_copy(ncv,icv,icv0)
      call util_copy(ncv,xcv,xcv0)
      call util_copy(ncv,zcv,zcv0)
      ncv0 = ncv
      ncv = 0
      do 1 iv = 1 , nv
        do 2 jcv = 1 , ncv0
          if (icv0(jcv) .eq. imv(iv)) then
            ncv = ncv + 1
            icv0(jcv) = -9999
            icv(ncv) = iv
            xcv(ncv) = xcv0(jcv)
            zcv(ncv) = zcv0(jcv)
          endif
    2   continue
        imv(iv) = iv
    1 continue

      do 3 jcv = 1 , ncv0
        if (icv0(jcv) .ne. -9999) then
          ncv = ncv + 1
          icv(ncv) = icv0(jcv)
          xcv(ncv) = xcv0(jcv)
          zcv(ncv) = zcv0(jcv)
        endif
    3 continue

      return
      end


c23456789012345678901234567890123456789012345678901234567890123456789012
      subroutine rmod_dir0(title,npar,n,ix,nx,x,z,it,v,ixh,nxh)
      character title*(*),crd80*80
      crd80 = 'N'
      call rmodrstr(title,crd80)
      if (crd80 .eq. 'Y') call rmod_dir(npar,n,ix,nx,x,z,it,v,ixh,nxh)
      return
      end

c23456789012345678901234567890123456789012345678901234567890123456789012
      subroutine rmod_dir(npar,n,ix,nx,x,z,it,v,ixh,nxh)
c  change direction of boundaries
      dimension it(1),ix(1),nx(1),x(1),z(1),v(1),ixh(1),nxh(1)
      do 1 i = 1 , n
        i1 = ix(i) + 1
        call gtol_vel_hor_size(nh,ixh,nxh,nx(i),it(i1))
        do 2 j = 1 , nh
          j1 = ixh(j) + i1
          if (x(j1) .gt. x(j1+1)) then
            if (npar .ge. 1) call cellrev(nxh(j),x(j1))
            if (npar .ge. 2) call cellrev(nxh(j),z(j1))
            if (npar .ge. 3) call cellrev(nxh(j),it(j1))
            if (npar .ge. 4) call cellrev(nxh(j),v(j1))
          endif
    2   continue
    1 continue
      return
      end

c23456789012345678901234567890123456789012345678901234567890123456789012
      subroutine rmod_deci(nb,ixb,nxb,xb,zb)
      character crd80*80
      ang = -1.
      crd80 = ' '
      print'(
     1/,'' Enter angle for decimating horizons - default= '',f8.4
     1,/,'' Enter angle < 0 for no decimation.  if 3 points form an ''
     1,/,'' angle less this, the center point is deleted.'')',ang
      read(*,'(a)',err=1,end=1)crd80
      if (crd80 .ne. ' ') read(crd80,*,err=1,end=1)ang
    1 continue
      ang = ang * asin(1.) / 90.
      if (ang .gt. 0.) call celldeci(ang,nb,ixb,nxb,xb,zb)
      return
      end

c23456789012345678901234567890123456789012345678901234567890123456789012
      subroutine rmod_fill(nf,mx,n,ix,nx,x,z,m_work,work,*)
      dimension ix(1),nx(1),x(1),z(1),work(1)
      character crd80*80
      integer   i_work_i,i_work_n
      ang = -1.
      crd80 = ' '
      print'(/,'' enter angle for filling - default= '',f8.4
     1,/,'' Enter angle<0 for no filling.'')',ang
      read(*,'(a)',err=2,end=2)crd80
      if (crd80 .ne. ' ') read(crd80,*,err=2,end=2)ang
    2 continue
      ang = ang * asin(1.) / 90.
      nf = 3
c  fill in bewteen points

      if (ang.le. 0. .or. nf .le. 2) return
      nx_max = nx(1)
      do 1 i = 2 , n
        nx_max = max(nx_max,nx(i))
    1 continue
      m = ix(n) + nx(n)

c  get work space
      call util_wors(i_work_i,i_work_n,m_work)
      call util_work(i_work_i,i_work_n,ix2,n)
      call util_work(i_work_i,i_work_n,nx2,n)
      call util_work(i_work_i,i_work_n,jx2,m)
      call util_work(i_work_i,i_work_n,iz2,m)
      call util_work(i_work_i,i_work_n,id,nx_max)
      call util_work(i_work_i,i_work_n,ibs,nx_max)
      call util_work(i_work_i,i_work_n,ics,nx_max)
      call util_work(i_work_i,i_work_n,ids,nx_max)
      call util_worc(i_work_i,i_work_n,i_err)
      if (i_err .ne. 0) goto 999

c  fill in arrays
      call rmod_fill1(ang,nf,mx,n,ix,nx,x,z,work(ix2),work(nx2)
     1,work(jx2),work(iz2),work(id),work(ibs),work(ics),work(ids),*999)
      return
  999 continue
      print'(/,'' error in rmod_fill'')'
      return 1
      end

c23456789012345678901234567890123456789012345678901234567890123456789012
      subroutine rmod_fill1(ang,nf,mx,n,ix1,nx1,x1,z1,ix2,nx2,x2,z2
     1,id,bs,cs,ds,*)
c  fill in n boundaries
      dimension ix1(1),nx1(1),x1(1),z1(1),ix2(1),nx2(1),x2(1),z2(1)
c  make copy of x,z points
      m = ix1(n) + nx1(n)
      call util_copy(n,ix1,ix2)
      call util_copy(n,nx1,nx2)
      call util_copy(m,x1,x2)
      call util_copy(m,z1,z2)
      call util_setr(n,ix1,0)
      call util_setr(n,nx1,0)
      call util_setr(mx,x1,0)
      call util_setr(mx,z1,0)

      do 1 i = 1 , n
        if (i .eq. 1) ix1(i) = 0
        i1 = ix1(i) + 1
        i2 = ix2(i) + 1
        lx = mx - ix1(i)
        call rmod_fil2(ang,nf
     1,lx,nx1(i),x1(i1),z1(i1),nx2(i),x2(i2),z2(i2),id,bs,cs,ds,*999)
        if (i .lt. n) ix1(i+1) = ix1(i) + nx1(i)
    1 continue
      return
  999 continue
      print'(/,'' error in r1fil1'')'
      return 1
      end

c23456789012345678901234567890123456789012345678901234567890123456789012
      subroutine rmod_fil2(ang,nf,mx,nx1,x1,z1,nx2,x2,z2,id,bs,cs,ds,*)
c  fill in 1 boundary
      dimension x1(1),z1(1),x2(1),z2(1),bs(1),cs(1),ds(1)

c  set discontinuity flag
      call util_setr(nx2,id,1)

c  compute spline coeeficients
      call cellspld(nx2,x2,z2,id,bs,cs,ds)
      n1 = 1
      i1 = 0
      do 1 i2 = 1 , nx2-1
        nf0 = nf
        if (x2(i2+1) .eq. x2(i2)) nf0 = 2
        if (i1 + nf0 .ge. mx) goto 999
        call rmod_fil3(nf0,x1(i1+1),z1(i1+1)
     1,x2(i2),z2(i2),x2(i2+1),z2(i2+1),bs(i2),cs(i2),ds(i2))
        n0 = 0
        call celldeci(ang,n1,n0,nf0,x1(i1+1),z1(i1+1))
        i1 = i1 + nf0 - 1
    1 continue
      nx1 = i1 + 1
      return
  999 continue
      print'(/,'' error in r1fil2'')'
      return 1
      end

c23456789012345678901234567890123456789012345678901234567890123456789012
      subroutine rmod_fil3(n,x,z,x1,z1,x2,z2,bs,cs,ds)
c  fill array with evenly spaced spline interpolations
      dimension x(1),z(1)
      ideriv = 0
      x(n) = x2
      z(n) = z2
      x(1) = x1
      z(1) = z1
      dx = (x2 - x1) / max(1,n-1)
      do 1 i = 2 , n-1
        x(i) = x1 + (i - 1) * dx
        call cellsplv(1,x1,z1,bs,cs,ds,x(i),ideriv,z(i))
c        xval    value of x for which interpolation is desired.
c        ideriv  0 to compute function, or 1 to compute derivative.
c        yval    computed value of function or derivative.
    1 continue
      return
      end

c23456789012345678901234567890123456789012345678901234567890123456789012
      subroutine rmod_chr(iop,inc,im,nx,it,x,z,v,nchr,xchr,ychr,chr)
      dimension x(1),z(1),v(1),xchr(1),ychr(1)
      character chr(*)*40,chr0*40
c      write(88,*)' r1chr iop=',iop,' inc=',inc,' nx=',nx
      nchr = 0
      do i = 1 , nx , inc
        chr0 = ' '
        if (iop .eq. 1) then
          write(chr0,'(i10)')it
        elseif (iop .eq. 2 .and. i .eq. nx/2+1) then
c      write(88,*)' i=',i,' im=',im,' it=',it
          write(chr0,'(i5,''-'',i5)')im,it
        elseif (iop .eq. 3 .and. i .eq. nx/2+1) then
          write(chr0,'(i10,''-'',i5,''-'',i5)')nint(v(i)),im,it
        elseif (iop .eq. 3) then
          write(chr0,'(i10)')nint(v(i))
        endif
        call rmod_comp(chr0)
        if (chr0 .ne. ' ') then
          nchr = nchr + 1
          chr(nchr) = chr0
          xchr(nchr) = x(i)
          ychr(nchr) = z(i)
c      call rmodlenr(lc,chr0)
c      write(88,'('' n='',i8,'' x='',f10.2,'' y='',f10.2,'' c='',a)')
c     1nchr,x(i),z(i),chr0(1:lc)
        endif
      enddo

      return
      end

c23456789012345678901234567890123456789012345678901234567890123456789012
      subroutine rmod_grid(i_ask_for_file,h_file,cxo,czo
     1,mv_grd,nx_vel,x0_vel,dx_vel,nz_vel,z0_vel,dz_vel,v_grd,z_lay
     1,m_work
     1,work
     1,*)
      parameter (m_cord=20)
      character *(*) h_file,cxo,czo
      character *64 rfile,d_file,word_type,m_file,t_file,mod_type
      character *16 cxi,cyi,czi,cyo,cord(m_cord)
      dimension x_cord(2,m_cord)
      data lu,ndim/-1,2/,cyo/' '/
      if (i_ask_for_file .eq. 0) then
        call rmod_efil(h_file,rfile)
        call util_get_file_name(' Enter gridded model for ray traceing'
     1,rfile,'hgrid')
      else
        rfile = h_file
      endif
      call rmodrdhd(rfile,d_file,word_type,m_file,t_file,mod_type
     1,cxi,cyi,czi,cxo,cyo,czo,m_cord,n_cord,x_cord,cord
     1,x_min,x_max,y_min,y_max,z_min,z_max,z_datum
     1,nx_vel,x0_vel,dx_vel
     1,ny_vel,y0_vel,dy_vel
     1,nz_vel,z0_vel,dz_vel
     1,ndim,lu,*999)
      if (nx_vel*ny_vel*nz_vel .gt. mv_grd) then
        print'(/,'' error in rmod_grid nx_vel= '',i8,'' nz_vel= '',i8
     1,'' mv_grd= '',i8)',nx_vel,nz_vel,mv_grd
        goto 999
      endif

      nxmix = 1
      nymix = 1
      nzmix = 1
c      print'('' cxi= '',a16,'' czi= '',a16,/,'' cxo= '',a16,'' czo= '',
c     1,cxi,czi,cxo,czo
        call rmodvgrd(d_file,word_type,mod_type
     1,cxi,cyi,czi,cxo,cyo,czo,n_cord,x_cord,cord
     1,x_min,x_max,y_min,y_max,z_min,z_max,z_datum
     1,nx_vel,x0_vel,dx_vel
     1,ny_vel,y0_vel,dy_vel
     1,nz_vel,z0_vel,dz_vel
     1,v_grd
     1,m_work,work,lu,*999)

      call rmod_reverse_negative_increment(mod_type
     1,nx_vel,x0_vel,dx_vel
     1,ny_vel,y0_vel,dy_vel
     1,nz_vel,z0_vel,dz_vel
     1,v_grd
     1)

      return
  999 continue
      call rmod_pause(' error in rmod_grid',' ')
      return 1
      end

c23456789012345678901234567890123456789012345678901234567890123456789012
      subroutine rmod_refl(
     1x_min,x_max,z_min,z_max,ib,nb,imb,itb,ixb,nxb,xb,zb
     1,nbh,ibid,nbseg,b_name,offmin,offmax,nang,angmin,anginc
     1,xb0,zb0,dx0,dz0)
      dimension imb(1),itb(1),ixb(1),nxb(1),xb(1),zb(1),ibid(1),nbseg(1)
      character b_name(1)*16,b_name0*16,crd80*80
      data init/1/
      if (init .eq. 1) then
        ib = 0
        offmin = 0
        offmax = 10000
        nang = 16
        angmin = 0.
        anginc = 5.
        init = 0
      endif

    1 continue

c  print the horizon info
      print'(/,'' # horizons = '',i8,'' # of boundaries = '',i8
     1,/,''    bnd   hor   seg  hor id  seg id  horizon name''
     1,''          x        z'')',nbh,nb
      do ibh = 1 , nbh
        is = 0
        do jb = 1 , nb
          if (ibid(ibh) .eq. imb(jb)) then
            i0 = ixb(jb) + (nxb(jb) + 1) / 2
            is = is + 1
      print'(1x,i5,1x,i5,1x,i5,2x,i5,2x,i5,5x,a16,1x,f8.2,1x,f8.2)'
     1,jb,ibh,is,ibid(ibh),itb(i0),b_name(ibh),xb(i0),zb(i0)
          endif
        enddo    ! do jb = 1 , nb
        if (is .eq. 0)
     1print'(1x,i5,1x,i5,1x,i5,2x,i5,2x,i5,2x,a16,1x,f8.2,1x,f8.2)'
     1,jb,ibh,is,ibid(ibh),0,b_name(ibh)
      enddo    ! do ibh = 1 , nbh

c  get boundary number
      crd80 = ' '
      print'(/,'' enter the boundary # to trace from default= '',i5
     1,/,'' Enter -1 to select a location by x,z,dip or ''
     1,'' 0 by picking'')',ib
      read(*,'(a)',err=1,end=2)crd80
      if (crd80 .ne. ' ')read(crd80,*,err=1)ib
    2 continue
      ib = max(-1,min(nb,ib))
      if (ib .eq. 0) then
        print'('' when plot comes up you must''
     1,'' pick two points for ray tracing.''
     1,/,'' mod_type any character to end picking'')'
        goto 6
      elseif (ib .lt. 0) then
  101   continue
        xb0 = (x_min + x_max) / 2.
        zb0 = (z_min + z_max) / 2.
        dip = 0
        print'(/,'' enter the x,z location and dip ''
     1,'' defaults='',f10.2,1x,f10.2,1x,f10.4)',xb0,zb0,dip
        crd80 = ' '
        read(*,'(a)',err=101)crd80
        if (crd80 .ne. ' ') read(crd80,*,end=102)xb0,zb0,dip
  102   continue
        dx0 = cos(dip)
        dz0 = -sin(dip)
c      print*,' dip=',dip,' dx0=',dx0,' dz0=',dz0
        goto 6
      endif

c  get x location
      call util_min_max(x1,x2,nxb(ib),xb(ixb(ib)+1))
      xb0 = (x1 + x2) / 2.
    4 continue
      crd80 = ' '
      print'(/,'' enter the x location default= '',f10.2)',xb0
      read(*,'(a)',err=4,end=5)crd80
      if (crd80 .ne.' ')read(crd80,*)xb0
    5 continue
      if (xb0 .lt. x_min .or. xb0 .gt. x_max) goto 4

c  determine the z location and dip
      ib1 = ixb(ib) + 1
      call rmod_fndx(i0,xb0,zb0,dx0,dz0,nxb(ib),xb(ib1),zb(ib1))
      if (i0 .eq. 0) goto 4

c  get offset range
    6 continue

      print'(/,'' enter minimum and maximum offsets default= ''
     1,f10.2,1x,f10.2
     1,/,'' enter 0.,0. for zero offset ray tracing''
     1)',offmin,offmax
      crd80 = ' '
      read(*,'(a)',err=6,end=7)crd80
      if (crd80 .ne. ' ')read(crd80,*,end=7)offmin,offmax
    7 continue

      print'(/,'' enter number of angles, min and increment defaults=''
     1,1x,i8,f10.2,1x,f10.2
     1,/,'' enter 1,0.,1. for zero offset ray tracing''
     1)',nang,angmin,anginc
      crd80 = ' '
      read(*,'(a)',err=6,end=8)crd80
      if (crd80 .ne. ' ')read(crd80,*,end=8)nang,angmin,anginc
    8 continue

      return

      entry rmod_refl_init
      init = 1
      return

      end

c23456789012345678901234567890123456789012345678901234567890123456789012
      subroutine rmod_ray_trace(
     1mt_ray,nt_ray,dt_ray,a_ray,x_ray,z_ray,t_ray
     1,x_min,x_max,z_min,z_max,is
     1,nx_vel,x0_vel,dx_vel,nz_vel,z0_vel,dz_vel,vel)
c  trace rays nt_ray time steps through a gridded slowness model
c  a_ray = initial starting angle in radians 0 = + x axis pi/2 = + zaxis
c  start at x_ray(1),z_ray(1)
c
c   pi/2
c ^  |
c |  |<-
c z  |   \
c    |    |                px = cos(angle) pz = sin(angle)
c    ------------ 0
c       x -->
      implicit  none
      integer   is,mt_ray,nt_ray
      real      dt_ray,a_ray
c      real      x_ray(mt_ray),z_ray(mt_ray),t_ray(mt_ray)
      real      x_ray(mt_ray),z_ray(mt_ray),t_ray

      real      x_min,x_max,z_min,z_max

      integer   nx_vel
      real      x0_vel,dx_vel
      integer   nz_vel
      real      z0_vel,dz_vel
      real      vel(nz_vel,nx_vel)

      integer   ix_min,ix_max,iz_min,iz_max,it
      real      px,pz,xf1,xf2,zf1,zf2,v0,dvdx,dvdz
      real      x1,z1,r1,r2,rx,rz,dx,dz,dt
      real      scale
      integer   i_call
      data      i_call/0/
      i_call = i_call + 1

c      if (i_call .le. -1) then
c      write(88,*)' mt_ray=',mt_ray,' dt_ray=',dt_ray
c     1,' a_ray=',a_ray*90./asin(1.),' x=',x_ray(1),' z=',z_ray(1)
c      write(88,*)' x_min=',x_min,x_max,z_min,z_max
c      write(88,*)' nx_vel=',nx_vel,x0_vel,dx_vel
c      write(88,*)' nz_vel=',nz_vel,z0_vel,dz_vel
c      write(88,*)' vel=',1./vel(1,1)
c      endif

      call rmod_dvdx(v0,dvdx,dvdz,x_ray(1),z_ray(1)
     1,nx_vel,x0_vel,dx_vel,nz_vel,z0_vel,dz_vel,vel)
      px = cos(a_ray) * v0
      pz = sin(a_ray) * v0
      is = 0

      do it = 2 , mt_ray

c  compute velocity and gradient_ray at this x,z location
c  compute new x,z coords and new ray parameter
        nt_ray = it
        x_ray(it) = x_ray(it-1) + px * dt_ray / v0**2
        z_ray(it) = z_ray(it-1) + pz * dt_ray / v0**2
c        t_ray(it) = (it - 1) * dt_ray

c      write(88,*)' it=',it,' v0=',1./v0,' dvdx=',dvdx,' dvdz=',dvdz
c     1,' dt=',sqrt((x_ray(it)-x_ray(it-1))**2+(z_ray(it)-z_ray(it-1))**

        call rmod_dvdx(v0,dvdx,dvdz,x_ray(it),z_ray(it)
     1,nx_vel,x0_vel,dx_vel,nz_vel,z0_vel,dz_vel,vel)

        px = px + dvdx * dt_ray / v0
        pz = pz + dvdz * dt_ray / v0

            if (x_ray(it) .lt. x_min) then
          is = 1
        elseif (z_ray(it) .lt. z_min) then
          is = 2
        elseif (x_ray(it) .gt. x_max) then
          is = 3
        elseif (z_ray(it) .gt. z_max) then
          is = 4
        endif
        if (is .ne. 0) goto 1

      enddo    ! do it = 2 , mt_ray

    1 continue

      dx = x_ray(nt_ray) - x_ray(nt_ray-1)
      dz = z_ray(nt_ray) - z_ray(nt_ray-1)
c      dt = t_ray(nt_ray) - t_ray(nt_ray-1)

      r2 = sqrt(dx**2+dz**2)
      x1 = max(x_min,min(x_max,x_ray(nt_ray)))
      z1 = max(z_min,min(z_max,z_ray(nt_ray)))

      if (dx .ne. 0) then
        rx = r2 * (x1 - x_ray(nt_ray-1)) / dx
      else
        rx = r2
      endif

      if (dz .ne. 0) then
        rz = r2 * (z1 - z_ray(nt_ray-1)) / dz
      else
        rz = r2
      endif

      r1 = min(rx,rz)
      if (r2 .eq. 0.) then
        scale = 1.
      else
        scale = r1 / r2
      endif

c      write(88,'(1x,i4,1x,i8
c     1,1x,f8.2,1x,f8.2
c     1,1x,f8.2,1x,f8.2
c     1,1x,f8.5,1x,f8.5,1x,f8.6)')
c     1i_call,nt_ray
c     1,x_ray(nt_ray-1),x_ray(nt_ray)
c     1,z_ray(nt_ray-1),z_ray(nt_ray)
c     1,t_ray(nt_ray-1),t_ray(nt_ray),scale
      x_ray(nt_ray) = x_ray(nt_ray-1) + dx * scale
      z_ray(nt_ray) = z_ray(nt_ray-1) + dz * scale
c      t_ray(nt_ray) = t_ray(nt_ray-1) + dt * scale
      t_ray = (nt_ray - 2) * dt_ray + scale * dt_ray
c      write(88,'(1x,i4,1x,i8
c     1,1x,f8.2,1x,f8.2
c     1,1x,f8.2,1x,f8.2
c     1,1x,f8.5,1x,f8.5)')
c     1i_call,nt_ray
c     1,x_ray(nt_ray-1),x_ray(nt_ray)
c     1,z_ray(nt_ray-1),z_ray(nt_ray)
c     1,t_ray(nt_ray-1),t_ray(nt_ray)

      return
      end

c23456789012345678901234567890123456789012345678901234567890123456789012
      subroutine rmod_dvdx(v0,dvdx,dvdz,x,z
     1,nx_vel,x0_vel,dx_vel,nz_vel,z0_vel,dz_vel,vel)
c  determine velocity and gradient at point x,z
      implicit  none
      real      v0,dvdx,dvdz,x,z

      integer   nx_vel
      real      x0_vel,dx_vel
      integer   nz_vel
      real      z0_vel,dz_vel
      real      vel(nz_vel,nx_vel)
      integer   ix1,ix2,iz1,iz2
      real      xf1,xf2,zf1,zf2

      ix1 = int((x-x0_vel)/dx_vel) + 1
      ix2 = max(1,min(nx_vel,ix1+1))
      ix1 = max(1,min(nx_vel,ix1))

      xf2 = max(0.,min(1.,(x-((ix1-1)*dx_vel+x0_vel))/dx_vel))
      xf1 = 1. - xf2

      iz1 = int((z-z0_vel)/dz_vel) + 1
      iz2 = max(1,min(nz_vel,iz1+1))
      iz1 = max(1,min(nz_vel,iz1))

      zf2 = max(0.,min(1.,(z-((iz1-1)*dz_vel+z0_vel))/dz_vel))
      zf1 = 1. - zf2

      v0 = xf1 * zf1 * vel(iz1,ix1)
     1   + xf2 * zf1 * vel(iz1,ix2)
     1   + xf1 * zf2 * vel(iz2,ix1)
     1   + xf2 * zf2 * vel(iz2,ix2)

      dvdx = (zf1*(vel(iz1,ix2)-vel(iz1,ix1))
     1       +zf2*(vel(iz2,ix2)-vel(iz2,ix1))) / dx_vel

      dvdz = (xf1*(vel(iz2,ix1)-vel(iz1,ix1))
     1       +xf2*(vel(iz2,ix2)-vel(iz1,ix2))) / dz_vel

      return
      end

c23456789012345678901234567890123456789012345678901234567890123456789012
      subroutine rmod_fndx(i0,x0,z0,dx0,dz0,nx_vel,x,z)
c  determine z location and dip at x0
c  dip is measured from + z axis i clockwise direction
      dimension x(1),z(1)
      i0 = 0
      dx0 = 0.
      dz0 = 0.
      do 1 i = 1 , nx_vel
        if (x0 .ge. min(x(i),x(i+1))
     1.and. x0 .le. max(x(i),x(i+1))) then
          i0 = i
          if (x(i) .ne. x(i+1)) then
            z0 = z(i) + (x0 - x(i)) * (z(i+1) - z(i)) / (x(i+1) - x(i))
          else
            z0 = (z(i) + z(i+1)) / 2.
          endif
c          dip0 = atan2(z(i+1)-z(i),x(i+1)-x(i))
          dx0 = x(i+1) - x(i)
          dz0 = z(i+1) - z(i)
c      write(88,*)' i0=',i0,' x0=',x0,' z0=',z0,' dx0=',dx0,' dz0=',dz0
          goto 2
        endif
    1 continue
    2 continue
      return
      end

c23456789012345678901234567890123456789012345678901234567890123456789012
      subroutine rmod_vhor(mod_type,x_min,x_max,z_min,z_max
     1,mb,mxb,nb,imb,itb,ixb,nxb,xb,zb,nv,imv,itv,ixv,nxv,xv,zv,vel
     1,ncv,icv,xcv,zcv,m_work,velb)
      dimension imb(1),itb(1),ixb(1),nxb(1),xb(1),zb(1)
     1,imv(1),itv(1),ixv(1),nxv(1),xv(1),zv(1),vel(1)
     1,icv(1),xcv(1),zcv(1),x0(2),z0(2),velb(2,1)
      character mod_type*(*),crd80*80
c  make sure this is the proper model mod_type
      print'(/,'' This option copies boundaries to velocity horizons''
     1,'' on a regular grid''
     1,/,'' the velocitiy horizons will extend from x_min to x_max.'')'
      if (mod_type .ne. 'LAYER') then
        print'('' Model mod_type must be layer''
     1,'' mod_type='',a16)',mod_type
        call rmod_pause(' ',' ')
        return
      endif
    1 continue
      crd80 = ' '
      nx = 11
      xinc = (x_max - x_min) / (nx - 1)
      print'('' x_min='',f10.2,'' x_max='',f10.2
     1,/,'' Enter xinc default='',f10.3)',x_min,x_max,xinc
      read(*,'(a)',err=1,end=2)crd80
      if (crd80 .ne. ' ') read(crd80,*,err=1,end=2)xinc
    2 continue
      nx = (x_max-x_min) / xinc + 1
      xinc = (x_max - x_min) / max(1,nx-1)
      if (nb * 2 * nx + nxv(1) . gt. mxb) then
        print'('' nx is too large for memory''
     1,/,'' nb='',i5,'' nxv='',i5,'' nx='',i5,'' mxb='',i8)'
     1,nb,nxv(1),nx,mxb
        goto 1
      endif

c      print*,' bef nx_max=',ixv(nv)+nxv(nv)
c     1,' v_min=',v_min,' v_inc=',v_inc
      call rmod_ibv(nb+1,velb,nv,itv,ixv,nxv,vel)
      n2 = 2
      x0(1) = x_min
      x0(2) = x_max

      do 3 i = 1 , nb+1
        i1 = max(1,min(nb,i-1))
        i2 = max(1,min(nb,i))
        ib1 = ixb(i1) + 1
        ib2 = ixb(i2) + 1
        if (i .eq. 1) then
          ixv(i) = 0
        else
          ixv(i) = ixv(i-1) + nxv(i-1)
        endif
        iv1 = ixv(i) + 1
c      print*,' i=',i,' velb=',velb(1,i),velb(2,i)
        if (i .eq. 1 ) then
          call util_setr(2,z0,z_min)
          call rmod_ib1(nx,x_min,xinc
     1,n2,x0,z0,nxb(i2),xb(ib2),zb(ib2)
     1,velb(1,i),nxv(i),itv(iv1),xv(iv1),zv(iv1),vel(iv1),xcv(i),zcv(i))
        elseif (i .eq. nb+1) then
          call util_setr(2,z0,z_max)
          call rmod_ib1(nx,x_min,xinc
     1,nxb(i1),xb(ib1),zb(ib1),n2,x0,z0
     1,velb(1,i),nxv(i),itv(iv1),xv(iv1),zv(iv1),vel(iv1),xcv(i),zcv(i))
        else
          call rmod_ib1(nx,x_min,xinc
     1,nxb(i1),xb(ib1),zb(ib1),nxb(i2),xb(ib2),zb(ib2)
     1,velb(1,i),nxv(i),itv(iv1),xv(iv1),zv(iv1),vel(iv1),xcv(i),zcv(i))
        endif
c        icv(i) = imv(i)
    3 continue
      nv = nb + 1
c      ncv = nv
c      print*,' aft nx_max=',ixv(nv)+nxv(nv)
      return
      end

c23456789012345678901234567890123456789012345678901234567890123456789012
      subroutine rmod_ibv(nb,velb,nv,itv,ixv,nxv,vel)
      dimension velb(2,1),itv(1),ixv(1),nxv(1),vel(1)
      do 1 i = 1 , nv
        i1 = ixv(i) + 1
        i2 = ixv(i) + nxv(i)
        velb(1,i) = vel(i1)
        do 2 j = i1 , i2
          if (itv(j) .ne. itv(i1)) then
            velb(2,i) = vel(j)
            goto 1
          endif
    2   continue
    1 continue
      do 3 i = nv+1 , nb
        velb(1,i) = velb(1,nv)
        velb(2,i) = velb(2,nv)
    3 continue
      return
      end

c23456789012345678901234567890123456789012345678901234567890123456789012
      subroutine rmod_ib1(nx,x_min,xinc,nxb1,xb1,zb1,nxb2,xb2,zb2,velb
     1,nxv,itv,xv,zv,vel,xcv,zcv)
      dimension xb1(1),zb1(1),xb2(1),zb2(1),itv(1),xv(1),zv(1),vel(1)
     1,velb(2)
      data icall/0/
      icall = icall + 1
      x_max = (nx - 1) * xinc + x_min
      call util_min_max(x_min2,x_max2,nxb2,xb2)
      x1 = max(x_min,x_min2)
      x2 = min(x_max,x_max2)
      ix1 = int((x1-x_min)/xinc) + 1
      if ((ix1-1)*xinc+x_min .gt. x1) ix1 = ix1 - 1
      ix2 = int((x2-x_min)/xinc) + 1
      if ((ix2-1)*xinc+x_min .lt. x2) ix2 = ix2 + 1
      n = int(x2 - x1) / xinc + 1
      if ((n-1)*xinc+x1 .lt. x2) n = n + 1
      nxv = n * 2
      do 1 i = 1 , n
        xv(i) = (i- 1) * xinc + x1
        xv(i+n) = xv(i)
    1 continue
c      print*,' nxb1=',nxb1,' nxb2=',nxb2,' nx=',nx,' n=',n
c     1,' velb=',velb(1),velb(2)
c      write(88,'(1x,f10.2,1x,f10.2,1x,i5)')(xb1(i),zb1(i),1,i=1,nxb1)
c      write(88,'(1x,f10.2,1x,f10.2,1x,i5)')(xb2(i),zb2(i),2,i=1,nxb2)
c      write(88,*)' bef icall=',icall,' nxb1=',nxb1,' n=',n
      call rmodfitc(nxb1,xb1,zb1,n,xv,zv)
c      write(88,*)' bef nxb2=',nxb2
      call rmodfitc(nxb2,xb2,zb2,n,xv(n+1),zv(n+1))
c      write(88,*)' aft nxb2=',nxb2
c      write(88,'(1x,f10.2,1x,f10.2,1x,i5)')(xv(i),zv(i),-1,i=1,n)
c      write(88,'(1x,f10.2,1x,f10.2,1x,i5)')(xv(i),zv(i),-2,i=n+1,n*2)
      call util_setr(n,vel,velb(1))
      call util_setr(n,vel(n+1),velb(2))
      call util_setr(n,itv,1)
      call util_setr(n,itv(n+1),2)
      xcv = xv(n/2+1)
      zcv = (zv(n/2+1) + zv(n+n/2+1)) / 2.
      return
      end

c23456789012345678901234567890123456789012345678901234567890123456789012
      subroutine rmod_2dl_to_3dl(h_file,d_file,mod_type
     1,mx,ix,nx,x,y,z,v,xg,yg
     1,nx_vel,x0_vel,dx_vel
     1,ny_vel,y0_vel,dy_vel
     1,nz_vel,v_grd,z_lay
     1,m_work
     1,work)
      character *(*) h_file,d_file,mod_type
      dimension ix(mx),nx(mx),x(mx),y(mx),z(mx),v(mx)
     1,xg(mx),yg(mx),v_grd(nx_vel,ny_vel,nz_vel*2),z_lay(nx_vel,ny_vel
     1,nz_vel),work(m_work)
      parameter (m_file=20)
      dimension y_file(m_file),ihor(m_file*2),iseg(m_file*2)
      character a_file(m_file)*64,b_file(m_file)*64,crd80*80

      print'(/,'' This option builds a 3d layered model using''
     1,/,'' 2d layered models for the velocity structure,''
     1,/,'' and 3d generic files for the horizon structure.''
     1,/,'' note it does not apply modify files,''
     1,'' and does not write out the files.'')'

      if (h_file .eq. ' ') then
        print'('' you must first read in the model file'')'
        return
      endif

      if (mod_type .ne. 'G3DL') then
        print'('' Model mod_type must be G3DL for this option''
     1,'' mod_type='',a16)',mod_type
        call rmod_pause(' ',' ')
        return
      endif

      crd80 = ' '
      write(crd80,'(a)')
     1' RMOD creating 3D layers from 2D layers and Generic'
      call rmod_title_to_history(crd80)
      call rmodlenr(lh,h_file)
      crd80 = ' '
      write(crd80,'('' Input file:'',a)')h_file(1:lh)
      call rmod_card_to_history(crd80)

      call rmod_read_file_list('*HMODIFY',h_file
     1,m_file,na_file,a_file,y_file,i_err)
      if (i_err .ne. 0) goto 999

      call rmod_read_file_list('*GEN',h_file
     1,m_file,nb_file,b_file,y_file
     1,i_err)
      if (i_err .ne. 0) goto 999

      print'('' grid characteristics'',/,'' nz_vel='',i5
     1,/,'' nx_vel='',i5,'' x0_vel='',f10.2,'' dx_vel='',f10.4
     1,/,'' ny_vel='',i5,'' y0_vel='',f10.2,'' dy_vel='',f10.4)'
     1,nz_vel
     1,nx_vel,x0_vel,dx_vel
     1,ny_vel,y0_vel,dy_vel
      print'(/,'' # of velocity horizon files ='',i5
     1,/,'' file#    line   filename'')',na_file
      do i = 1 , na_file
        call rmodlenr(la,a_file(i))
        print'(1x,i5,1x,f10.2,1x,a)',i,y_file(i),a_file(i)(1:la)
        crd80 = ' '
        write(crd80,'('' 2D file number:'',i8,'' y:'',f10.2
     1,'' file:'',a)')i,y_file(i),a_file(i)(1:la)
        call rmod_card_to_history(crd80)
      enddo
      print'(/,'' # of depth horizon files ='',i5
     1,/,'' file#  hor1  seg1  hor2  seg2  filename'')',nb_file
      do i = 1 , nb_file
        call rmodlenr(lb,b_file(i))
        print'(1x,i5,1x,i5,1x,i5,1x,i5,1x,i5,1x,a)'
     1,i,ihor(2*i-1),iseg(2*i-1),ihor(2*i),iseg(2*i),b_file(i)(1:lb)
        crd80 = ' '
        write(crd80,'('' Generic file number:'',i8,'' horizon:'',i8
     1,'' seg:'',i8,'' seg2:'',i8,'' file:'',a)')
     1i,ihor(2*i-1),iseg(2*i-1),ihor(2*i),iseg(2*i),b_file(i)(1:lb)
        call rmod_card_to_history(crd80)
      enddo
      nz_vel = nb_file

c  read horizon values
      if (d_file .ne. 'NONE') then
        print'('' not creating depth horizon grid''
     1,'' because it already exists.'')'
      else
        print'('' creating depth horizon grid'')'
        do 4 ib = 1 , nb_file
          call rmod_trupath(h_file,b_file(ib))
          call rmodopen(ib_file,b_file(ib),'FORMATTED','OLD'
     1,'SEQUENTIAL','IEEE',0,i_err)
          do 5 ixg = 1 , nx_vel
            read(ib_file,*,err=998,end=998)(z_lay(ixg,iyg,ib),iyg=ny_vel
     1,1
     1,-1)
    5     continue
    4   continue
      endif

      call rmod_apend_close             ! close the apend file
      call rmod_file_to_apend(h_file)    ! add this to the apend file

      return
  998 continue
      call rmod_pause(' error during read',b_file(ib))
      return
  999 continue
      call rmod_pause(' error during read',h_file)
      return
      end

c23456789012345678901234567890123456789012345678901234567890123456789012
      subroutine rmod_read_file_list(head
     1,file,m_file,n_file,a_file,y_file,i_err)
      integer util_len_r
      dimension y_file(1)
      character head*(*),file*(*),a_file(m_file)*(*)
c      character head*(*),file*(*),a_file(m_file)*64
      character crd132*132
      call rmodopen(i_file,file,'FORMATTED','OLD','SEQUENTIAL'
     1,'IEEE',n0,i_err)
c      print*,' aft open i_err=',i_err,' n_file=',n_file
c      print'('' head='',a)',head

      if (i_err .ne. 0) return

      call rmodfsts(i_file,1,head,*999)
c      print*,' aft fsts'
      i = 0

      do 1 j = 1 , m_file
    3   continue
        read(i_file,'(a)',end=2,err=999) crd132
        call rmodfchr(i_star,'*',1,len(crd132),crd132)
        call rmodfchr(i_pound,'#',1,len(crd132),crd132)
c      print'('' crd132='',a60)',crd132(1:60)
c      print*,' i_star=',i_star,' i_pound=',i_pound
c      print*,' len(a_file)=',len(a_file(1))

        if (i_star .ne. 0) goto 2
        if (i_pound .ne. 0) goto 3
        i = i + 1
        a_file(i) = ' '
        call headgvcc(crd132,a_file(i),nchar,'file')
        if (head .eq. '*HMODIFY') call headgvrc(crd132,y_file(i),'line')
        n_file = i

        if (head .eq. '*HMODIFY') then
c      print'('' n_file='',i5,'' y_file='',f10.2,'' a_file='',a40)'
c     1,n_file,y_file(i)
c     1,a_file(i)(1:util_len_r(a_file(i)))
      else
c      print'('' n_file='',i5,'' a_file='',a40)',n_file
c     1,a_file(i)(1:util_len_r(a_file(i)))
      endif

    1 continue
    2 continue
      call rmodclof(file)
c      print*,' before return n_file=',n_file

      return
  999 continue
      i_err = 1
      call rmod_pause(' error during in rmod_read_file_list',' ')
      return
      end

c23456789012345678901234567890123456789012345678901234567890123456789012
      subroutine rmod_rdvs(file,ihor,iseg,y0,mx,nx,x,y,z,v,i_err)
      dimension x(1),y(1),z(1),v(1)
      character *(*) file
      character crd132*132
      call rmodlenr(lf,file)
c      print'('' rdv2 ihor='',i5,'' iseg='',i5,'' file:'',a)'
c     1,file(1:lf)
      nx = 0
      call rmodopen(i_file,file,'FORMATTED','OLD','SEQUENTIAL'
     1,'IEEE',n0,i_err)
      if (i_err .ne. 0) goto 999
      ldof = 5
c  find start
      call rmodfstr(i_file,1,ncrd,'*VELOCITY',crd132,*2)
      do 1 ix = 1 , mx
        call rmodrcrd(i_file,ldof,x0,z0,i1,i2,v0,*2)
        if (ihor .eq. i1 .and. iseg .eq. i2) then
          nx = nx + 1
          x(nx) = x0
          y(nx) = y0
          z(nx) = z0
          v(nx) = v0
        endif
    1 continue
    2 continue
      call rmodclof(file)
c      print*,' ihor=',ihor,' iseg=',iseg,' nx=',nx
      return
  999 continue
      call rmodlenr(lf,file)
      print'(/,'' error during open file:'',a)',file(1:lf)
      return
      end

      subroutine rmod_par0(name3,name4)
      character *64 name0,name1,name2
      character *(*) name3,name4
      save name0,ln0,ln1,ln2

      name1 = name3
      name2 = name4

      print'(/,'' Enter output file name start and end.''
     1,/,'' files will have the first 2 digits of the line number ''
     1,''in the center of the name'')'

    1 continue
      call rmodlenr(ln1,name1)
      print'(/,'' enter file name start default='',a)',name1(1:ln1)
      read(*,'(a)',err=1,end=2)name1

    2 continue
      call rmodlenr(ln2,name2)
      print'(/,'' enter file name end   default='',a)',name2(1:ln2)
      read(*,'(a)',err=2,end=3)name2
      call rmod_ext(name2,'mod')

    3 continue

      call rmodlenr(ln1,name1)
      call rmodlenr(ln2,name2)

      name0 = name1(1:ln1)//'xx'//name2(1:ln2)
      call rmodlenr(ln0,name0)
      print'('' filenames will be '',a)',name0(1:ln0)

      return

      entry rmod_par1(y,name3)
      name3 = name0(1:ln0)
      name3(ln1+1:ln1+1) = '00'
      if (int(y/100) .lt. 10) then
        write(name3(ln1+2:ln1+2),'(i1)')max(0,min(9,int(y/100.)))
      else
        write(name3(ln1+1:ln1+2),'(i2)')max(0,min(99,int(y/100.)))
      endif
c      print'('' y='',f10.2,'' name3='',a)',y,name3(1:ln0)
      return
      end

c23456789012345678901234567890123456789012345678901234567890123456789012
      subroutine rmod_23dg_to_3dl(h_file,d_file,word_type,mod_type
     1,cxi,cyi,czi,m_cord,n_cord,x_cord,cord
     1,nxv,iv1,iv2,iv3,xv,yv,zv,vel
     1,ncv,icv,xcv,ycv,zcv
     1,mv_grd,nx_lay,x0_lay,dx_lay
     1,ny_lay,y0_lay,dy_lay
     1,nz_lay,zl,v2,m_work,work)
      character *(*) h_file,d_file,word_type,mod_type,cxi,cyi,czi
      dimension xv(1),yv(1),zv(1),vel(1),iv1(1),iv2(1),iv3(1)
     1,icv(1),xcv(1),ycv(1),zcv(1)
     1,zl(nx_lay,ny_lay,nz_lay),v2(mv_grd),work(m_work)
      parameter (m_file=20)
      dimension y2(m_file)
      character fy2(m_file)*64,crd80*80,ans*2,h_file0*80,omod_type*16
      character *16 cxv,cyv,czv
      character cord(*)*(*)


      print'(/,'' This option builds a 3d layered model using''
     1,/,'' 2d gridded models for the velocity structure,''
     1,/,'' and 3d generic files for the horizon structure.''
     1,/,'' note it does not apply modify files,''
     1,'' and does not write out the files.'')'

      if (h_file .eq. ' ') then
        print'('' you must first read in the 3d layered model file'')'
        return
      endif

      if (mod_type .ne. 'G3DL') then
        print'('' Model mod_type must be G3DL for this option''
     1,'' mod_type='',a16)',mod_type
        call rmod_pause(' ',' ')
        return
      endif

      ans = '2D'
      call rmod_ans(ans,ans,'2D','3D'
     1,' Fit 2D or 3D grids to 3d layered?')

      if (ans .eq. '2D') then

c  read 2d velocity model names
      call rmod_read_file_list('*HMODIFY'
     1,h_file,m_file,ny2,fy2,y2,i_err)
      iv = 1
      print'('' number of 2d velocity models='',i5)',ny2
      crd80 = ' '
      write(crd80,'(a)')
     1' RMOD creating 3D layer velocities from 2D gridded models'
      call rmod_title_to_history(crd80)
      call rmodlenr(lh,h_file)
      crd80 = ' '
      write(crd80,'('' Number of files:'',i8,'' HMODIFY file:'',a)')
     1ny2,h_file(1:lh)
      call rmod_card_to_history(crd80)

      do i = 1 , ny2
        call rmodlenr(lf,fy2(i))
        print'('' i='',i5,'' y='',f10.2,'' file:'',a)'
     1,i,y2(i),fy2(i)(1:lf)
        crd80 = ' '
      write(crd80,'('' File number :'',i8,'' y:'',f10.2,'' file:'',a)')
     1i,y2(i),fy2(i)(1:lf)
        call rmod_card_to_history(crd80)
        call rmod_grid(1,fy2(i),cxi,czi
     1,mv_grd-i+1,nx2,x2min,x2inc,nz2,z2min,z2inc,v2(iv),z,m_work,work
     1,*999)
        iv = iv + nx2 * nz2
      enddo    ! do i = 1 , ny2
      ndim = 2
      y2inc = 1.
      if (i_err .ne. 0) goto 999
      else    ! if (ans .eq. '2D') then

    2 continue
        ndim = 3
        call util_copy_file_name(h_file,h_file0)
        call util_get_file_name(' Enter 3d gridded model'
     1,h_file0,'hgrid')
        crd80 = ' '
        write(crd80,'(a)')
     1' RMOD creating 3D layer velocities from 3D gridded model'
        call rmod_title_to_history(crd80)
        call rmodlenr(lh,h_file0)
        crd80 = ' '
        write(crd80,'('' file:'',a)')h_file0(1:lf)
        call rmod_card_to_history(crd80)
c  read the gridded velocity - note z_lay is used for gridded vleocity
        call rmodrgrd(h_file0,omod_type
     1,cxv,cyv,czv,cxi,cyi,czi,m_cord,n_cord,x_cord,cord
     1,x_min1,x_max1,y_min1,y_max1,z_min1,z_max1,z_datum1
     1,mv_grd,nx2,x2min,x2inc,ny2,y2min,y2inc,nz2,z2min,z2inc
     1,v2,m_work,work,lu
     1,i_err)
      if (i_err .ne. 0) goto 999

        if (omod_type .ne. 'GRID') then
          print'('' this model must be a grid
     1 omod_type='',a16)',omod_type
          return
        endif


      endif    ! if (ans .eq. '2D') then

      print'('' ndim='',i5,'' ncv='',i5,'' nxv='',i10)',ndim,ncv,nxv
      print'('' nx_vel='',i10,'' x0_vel='',f10.2,'' dx_vel='',f10.4)'
     1,nx2,x2min,x2inc
      print'('' ny_vel='',i10,'' y0_vel='',f10.2,'' dy_vel='',f10.4)'
     1,ny2,y2min,y2inc
      print'('' nz_vel='',i10,'' z0_vel='',f10.2,'' dz_vel='',f10.4)'
     1,nz2,z2min,z2inc

      call gtol_3dg_to_3dl(
     1nxv,iv1,iv2,iv3,xv,yv,zv,vel
     1,ncv,icv,xcv,ycv,zcv
     1,mzl,nx_lay,x0_lay,dx_lay
     1,ny_lay,y0_lay,dy_lay
     1,nz_lay,zl
     1,ndim,nx2,x2min,x2inc,ny2,y2min,y2inc,nz2,z2min,z2inc,v2,y2
     1,m_work,work,i_err)

      call util_invert(nxv,vel)

      call rmod_apend_close             ! close the apend file
      call rmod_file_to_apend(h_file)    ! add this to the apend file

      return
  999 continue
      print'(/,'' error reading file h_file= '',a40)',h_file
      return
      end

c23456789012345678901234567890123456789012345678901234567890123456789012
      subroutine rmod_3dl_to_3dg(mod_type,zlmin,zlmax
     1,nxv,iv1,iv2,iv3,xv,yv,zv,vel
     1,ncv,icv,xcv,ycv,zcv
     1,mv_grd
     1,nx_vel,x0_vel,dx_vel
     1,ny_vel,y0_vel,dy_vel
     1,nz_vel,z0_vel,dz_vel
     1,v_grd,z_lay
     1,m_work,work)
      character mod_type*(*),crd80*80
      dimension work(1)
      real zlmin,zlmax

      if (mod_type .ne. 'G3DL') then
        print'('' Model mod_type must be G3DL mod_type='',a16)',mod_type
        call rmod_pause(' ',' ')
        return
      endif

      print'(/,'' This option converts a 3d layered model into a ''
     1,''3d gridded model.'',/,'' Enter new grid limits'')'
      nz_lay = nz_vel
      nz_vel = 101
      z0_vel = zlmin
      dz_vel = (zlmax - z0_vel) / (nz_vel - 1)
      call rmod_new_grid_size('x'
     1,nx_lay,x0_lay,dx_lay
     1,nx_vel,x0_vel,dx_vel)
      call rmod_new_grid_size('y'
     1,ny_lay,y0_lay,dy_lay
     1,ny_vel,y0_vel,dy_vel)
      call rmod_new_grid_size('z'
     1,nz1,z1min,z1inc
     1,nz_vel,z0_vel,dz_vel)
      if (nx_vel*ny_vel*nz_vel .gt. mv_grd) then
        print'('' insufficient memory for new grid''
     1,/,'' mv_grd='',i8,''
     1 nx_vel*ny_vel*nz_vel='',i8)',mv_grd,nx_vel*ny_vel*nz_vel
        nz_vel = nz_lay
        nx_vel = nx_lay
        x0_vel = x0_lay
        dx_vel = dx_lay
        ny_vel = ny_lay
        y0_vel = y0_lay
        dy_vel = dy_lay
        return
      endif
      print'(/,'' layered grid characteristics''
     1,/,'' nz_lay='',i5,'' zlmin='',f10.2,'' zlmax='',f10.4
     1,/,'' nx_lay='',i5,'' x0_lay='',f10.2,'' dx_lay='',f10.4
     1,/,'' ny_lay='',i5,'' y0_lay='',f10.2,'' dy_lay='',f10.4)'
     1,nz_lay,z_min,z_max
     1,nx_lay,x0_lay,dx_lay
     1,ny_lay,y0_lay,dy_lay
      print'(/,'' 3d grid characteristics''
     1,/,'' nx_vel='',i5,'' x0_vel='',f10.2,'' dx_vel='',f10.4
     1,/,'' ny_vel='',i5,'' y0_vel='',f10.2,'' dy_vel='',f10.4
     1,/,'' nz_vel='',i5,'' z0_vel='',f10.2,'' dz_vel='',f10.4)'
     1,nx_vel,x0_vel,dx_vel
     1,ny_vel,y0_vel,dy_vel
     1,nz_vel,z0_vel,dz_vel
      crd80 = ' '
      write(crd80,'(a)')' RMOD creating 3D grid from 3D layer'
      call rmod_title_to_history(crd80)
      crd80 = ' '
      write(crd80,'('' nz_lay:'',i5)')nz_lay
      call rmod_card_to_history(crd80)
      crd80 = ' '
      write(crd80,'('' nx_lay:'',i5,'' x0_lay:'',f10.2
     1,'' dx_lay:'',f10.4)')
     1nx_lay,x0_lay,dx_lay
      call rmod_card_to_history(crd80)
      crd80 = ' '
      write(crd80,'('' ny_lay:'',i5,'' y0_lay:'',f10.2
     1,'' dy_lay:'',f10.4)')
     1ny_lay,y0_lay,dy_lay
      call rmod_card_to_history(crd80)
      crd80 = ' '
      write(crd80,'('' nx_vel:'',i5,'' x0_vel:'',f10.2,''
     1 dx_vel:'',f10.4)')
     1nx_vel,x0_vel,dx_vel
      call rmod_card_to_history(crd80)
      crd80 = ' '
      write(crd80,'('' ny_vel:'',i5,'' y0_vel:'',f10.2,''
     1 dy_vel:'',f10.4)')
     1ny_vel,y0_vel,dy_vel
      call rmod_card_to_history(crd80)
      crd80 = ' '
      write(crd80,'('' nz_vel:'',i5,'' z0_vel:'',f10.2,''
     1 dz_vel:'',f10.4)')
     1nz_vel,z0_vel,dz_vel
      call rmod_card_to_history(crd80)

      call util_invert(nxv,vel)

      call util_copy(nx_lay*ny_lay*nz_lay,v_grd,z_lay)
      call gtol_3dl_to_3dg(
     1 nx_lay,x0_lay,dx_lay
     1,ny_lay,y0_lay,dy_lay
     1,nz_lay,z_lay
     1,nxv,iv1,iv2,iv3,xv,yv,zv,vel
     1,ncv,icv,xcv,ycv,zcv
     1,nx_vel,x0_vel,dx_vel
     1,ny_vel,y0_vel,dy_vel
     1,nz_vel,z0_velc,dz_vel
     1,v_grd,m_work,work
     1,i_err)
      if (i_err .ne. 0) goto 999

      call util_invert(nxv,vel)

      mod_type ='GRID'

      return
  999 continue
      print'(''errror in rmod_3dl_to_3dg'')'
      return
      end

c23456789012345678901234567890123456789012345678901234567890123456789012
      subroutine rmod_new_grid_size(cx,nx,x_min,xinc,nx_vel,x0_vel
     1,dx_vel)
      character cx*(*),crd80*80
      nx = nx_vel
      x_min = x0_vel
      xinc = dx_vel

    1 continue
      print'(/,'' enter '',a1,'' grid values defaults - ''
     1,/,'' n'',a1,''='',i8,'' '',a1,''min='',f10.2,'' '',a1,''inc=''
     1,f10.4)',cx,cx,nx_vel,cx,x0_vel,cx,dx_vel
      crd80 = ' '
      read(*,'(a)',err=1,end=2)crd80
      if (crd80 .ne. ' ') read(crd80,*,err=1,end=2)nx_vel,x0_vel,dx_vel
    2 continue
c      print*,' nx=',nx,x_min,xinc
c      print*,' nx_vel=',nx_vel,x0_vel,dx_vel

      return
      end

c23456789012345678901234567890123456789012345678901234567890123456789012
      subroutine rmod_set_grid_size(cx,nx,x_min,x_inc,nx_vel,x0_vel
     1,dx_vel)
      character cx*(*),crd80*80
      nx_vel = nx
      x0_vel = x_min
      dx_vel = x_inc

    1 continue
      print'(/,'' enter '',a1,'' grid values defaults - ''
     1,/,'' n'',a1,''='',i8,'' '',a1,''min='',f10.2,'' '',a1,''inc=''
     1,f10.4)',cx,cx,nx_vel,cx,x0_vel,cx,dx_vel
      crd80 = ' '
      read(*,'(a)',err=1,end=2)crd80
      if (crd80 .ne. ' ') read(crd80,*,err=1,end=2)nx_vel,x0_vel,dx_vel
    2 continue
c      print*,' nx=',nx,x_min,x_inc
c      print*,' nx_vel=',nx_vel,x0_vel,dx_vel

      return
      end

c23456789012345678901234567890123456789012345678901234567890123456789012
      subroutine rmod_smooth_grid_fast(mod_type
     1,mv_grd
     1,nx_vel,x0_vel,dx_vel
     1,ny_vel,y0_vel,dy_vel
     1,nz_vel,z0_vel,dz_vel
     1,v_grd
     1,m_work,work)

      implicit  none
      integer   mv_grd

      integer   nx_vel
      real      x0_vel,dx_vel

      integer   ny_vel
      real      y0_vel,dy_vel

      integer   nz_vel
      real      z0_vel,dz_vel

      real      v_grd(nz_vel,nx_vel,ny_vel)

      integer   m_work
      real      work(nz_vel,nx_vel,ny_vel)

      integer   n1,n2,n3
      integer   nx_smooth,ny_smooth,nz_smooth
      integer   i_work_i,i_work_n
      integer   ix_vel,jx_vel,ix_vel_1,ix_vel_2
      integer   iy_vel,jy_vel,iy_vel_1,iy_vel_2
      integer   iz_vel,jz_vel,iz_vel_1,iz_vel_2
      real      w_sum,v_sum
      integer   i_err

      character mod_type*(*),crd80*80

      print'('' This program creates a smoothed gridded model.''
     1,/,'' On the same grid as the input.''
     1,/,'' You will be asked to define the X,Y and Z smoothing ''
     1,/,'' distances in terms of grid nodes in each direction''
     1)'

      if (mod_type(1:1) .ne. 'G') then
        print'('' Model mod_type must be grid or G3DL''
     1,'' mod_type='',a16)',mod_type
        call rmod_pause(' ',' ')
        return
      endif

    1 continue

      nx_smooth = 0
      ny_smooth = 0
      nz_smooth = 0

      print'(
     1'' enter the smothing distances in each direction defaults=''
     1,i8,1x,i8,1x,i8)',nx_smooth,ny_smooth,nz_smooth

      read(*,'(a)',err=1,end=2)crd80
      if (crd80(1:1) .ne. ' ') read(crd80,*,err=1,end=2)
     1nx_smooth,ny_smooth,nz_smooth
    2 continue

      print'(
     1'' the smothing distances in each direction are=''
     1,i8,1x,i8,1x,i8)',nx_smooth,ny_smooth,nz_smooth

      if (mod_type(1:4) .eq. 'G3DL') then
      call util_stop(' this is ready yet',0)
        n1 = nx_smooth
        n2 = ny_smooth
        n3 = nz_smooth
        nz_smooth = n1
        nx_smooth = n2
        ny_smooth = n3
      endif    ! if (mod_type(1:4) .eq. 'G3DL') then

c      nv_grd = nx_vel * ny_vel * nz_vel
c      nwk = 4 
c      call util_wors(i_work_i,i_work_n,m_work)
c      call util_work(i_work_i,i_work_n,iv_grd,nv_grd)
c      call util_work(i_work_i,i_work_n,iwk,nwk)
c      call util_worc(i_work_i,i_work_n,i_err)

      if (nx_vel*ny_vel*nz_vel .gt. mv_grd .or. i_err .ne. 0) then
        print'('' insufficient memory for new grid''
     1,/,'' mv_grd='',i8,'' nx_vel*ny_vel*nz_vel='',i8,'' i_err='',i8)'
     1,mv_grd,nx_vel*ny_vel*nz_vel,i_err
        return
      endif


      call util_copy(nx_vel*ny_vel*nz_vel,v_grd,work)

      do iy_vel = 1 , ny_vel
        iy_vel_1 = max(     1,iy_vel-ny_smooth)
        iy_vel_2 = min(ny_vel,iy_vel+ny_smooth)

        do ix_vel = 1 , nx_vel
          ix_vel_1 = max(     1,ix_vel-nx_smooth)
          ix_vel_2 = min(nx_vel,ix_vel+nx_smooth)

          do iz_vel = 1 , nz_vel
            iz_vel_1 = max(     1,iz_vel-nz_smooth)
            iz_vel_2 = min(nz_vel,iz_vel+nz_smooth)

            v_sum = 0
            w_sum = 0.

            do jy_vel = iy_vel_1 , iy_vel_2
            do jx_vel = ix_vel_1 , ix_vel_2
            do jz_vel = iz_vel_1 , iz_vel_2

              w_sum = w_sum + 1
              v_sum = v_sum + work(jz_vel,jx_vel,jy_vel)

            enddo    ! do jz_vel = iz_vel_1 , iz_vel_2
            enddo    ! do jx_vel = ix_vel_1 , ix_vel_2
            enddo    ! do jy_vel = iy_vel_1 , iy_vel_2

      if (w_sum .ne. 0.) v_grd(iz_vel,ix_vel,iy_vel) = v_sum / w_sum

      enddo    ! do iz_vel = 1 , nz_vel
      enddo    ! do ix_vel = 1 , nx_vel
      enddo    ! do iy_vel = 1 , ny_vel

      crd80 = ' '
      write(crd80,'('' RMOD smoothing gridded model fast'')')
      call rmod_title_to_history(crd80)

      crd80 = ' '
      write(crd80,'(''  smothing x='',i8,'' y='',i8,'' z='',i8)')
     1 nx_smooth,ny_smooth,nz_smooth
      call rmod_card_to_history(crd80)

      return
  999 continue
      call rmod_pause(' error during grid interpolation',' ')
      print'(/,'' error in rmod_interpolate_grid'')'
      return
      end

c23456789012345678901234567890123456789012345678901234567890123456789012
      subroutine rmod_smooth_grid(mod_type
     1,mv_grd
     1,nx_vel,x0_vel,dx_vel
     1,ny_vel,y0_vel,dy_vel
     1,nz_vel,z0_vel,dz_vel
     1,v_grd
     1,m_work,work)

      integer  n1,n2,n3

      real     r1_top,r2_top,r3_top
      real     d1_top,d2_top,d3_top
      real     r1_bot,r2_bot,r3_bot
      real     d1_bot,d2_bot,d3_bot

      integer  na1
      real     oa1,da1
      integer  na2
      real     oa2,da2
      integer  na3
      real     oa3,da3

      integer  nb1
      real     ob1,db1
      integer  nb2
      real     ob2,db2
      integer  nb3
      real     ob3,db3
      integer   i_work_i,i_work_n

      character mod_type*(*),crd80*80
      dimension work(1)

      print'('' This program creates a smoothed gridded model.''
     1,/,'' You will be asked to define the output grid,''
     1,/,'' then you will be asked for the one sided smoothing ''
     1,/,'' operator size, x_smooth,''
     1  ,'' at the top and bottom of the model''
     1,/,'' and the smoothing step, dx_smooth, in the smoothing sum''
     1,/,'' in each of the X,Y and Z directions.''
     1,/,'' The velocity at each output grid node will be the average''
     1,/,'' of the input velocity measured at 2*nx_smooth+1 locations''
     1,/,'' Those locations are at''
     1,/,'' x_smooth_sum = x_output + (ix_smooth - 1) * dx_smooth ''
     1,/,'' ix_smooth = -nx_smooth , nx_smooth''
     1,/,'' nx_smooth = x_smooth / dx_smooth''
     1)'

      if (mod_type(1:1) .ne. 'G') then
        print'('' Model mod_type must be grid or G3DL''
     1,'' mod_type='',a16)',mod_type
        call rmod_pause(' ',' ')
        return
      endif

      print'(/,'' enter the output grid definition'')'
      call rmod_new_grid_size('x',nx,x_min,xinc,nx_vel,x0_vel,dx_vel)
      call rmod_new_grid_size('y',ny,y_min,yinc,ny_vel,y0_vel,dy_vel)
      if (mod_type .ne. 'G3DL')
     1call rmod_new_grid_size('z',nz,z_min,zinc,nz_vel,z0_vel,dz_vel)

      if (mod_type .eq. 'G3DL') then
        nz = nz_vel
        z_min = z0_vel
        zinc = dz_vel

        na1 = nx
        oa1 = x_min
        da1 = xinc

        na2 = ny
        oa2 = y_min
        da2 = yinc

        na3 = nz
        oa3 = z_min
        da3 = dz_vel

        nb1 = nx_vel
        ob1 = x0_vel
        db1 = dx_vel

        nb2 = ny_vel
        ob2 = y0_vel
        db2 = dy_vel

        nb3 = nz_vel
        ob3 = z0_vel
        db3 = dz_vel

      else

        na2 = nx
        oa2 = x_min
        da2 = xinc

        na3 = ny
        oa3 = y_min
        da3 = yinc

        na1 = nz
        oa1 = z_min
        da1 = zinc

        nb2 = nx_vel
        ob2 = x0_vel
        db2 = dx_vel

        nb3 = ny_vel
        ob3 = y0_vel
        db3 = dy_vel

        nb1 = nz_vel
        ob1 = z0_vel
        db1 = dz_vel

      endif
      nv_grd = na1 * na2 * na3
      nwk = 4 * (nb1 + nb2 + nb3)
      call util_wors(i_work_i,i_work_n,m_work)
      call util_work(i_work_i,i_work_n,iv_grd,nv_grd)
      call util_work(i_work_i,i_work_n,iwk,nwk)
      call util_worc(i_work_i,i_work_n,i_err)

      if (nx_vel*ny_vel*nz_vel .gt. mv_grd .or. i_err .ne. 0) then
        print'('' insufficient memory for new grid''
     1,/,'' mv_grd='',i8,'' nx_vel*ny_vel*nz_vel='',i8,'' i_err='',i8)'
     1,mv_grd,nx_vel*ny_vel*nz_vel,i_err
        return
      endif


c      print*,' na1=',na1,na2,na3,' nb1=',nb1,nb2,nb3
c      print*,' oa1=',oa1,oa2,oa3,' ob1=',ob1,ob2,ob3
c      print*,' da1=',da1,da2,da3,' db1=',db1,db2,db3

      call util_copy(nv_grd,v_grd,work(iv_grd))

      print'(
     1   '' na1='',i10,'' oa1='',f10.2,'' da1='',f10.2
     1,/,'' na2='',i10,'' oa2='',f10.2,'' da2='',f10.2
     1,/,'' na3='',i10,'' oa3='',f10.2,'' da3='',f10.2
     1,/,'' nb1='',i10,'' ob1='',f10.2,'' db1='',f10.2
     1,/,'' nb2='',i10,'' ob2='',f10.2,'' db2='',f10.2
     1,/,'' nb3='',i10,'' ob3='',f10.2,'' db3='',f10.2)'
     1,na1,oa1,da1
     1,na2,oa2,da2
     1,na3,oa3,da3
     1,nb1,ob1,db1
     1,nb2,ob2,db2
     1,nb3,ob3,db3

      n1 = max(0,nint(db1/da1)/2)
      n2 = max(0,nint(db2/da2)/2)
      n3 = max(0,nint(db3/da3)/2)

      n1 = 1
      n2 = 1
      n3 = 1

    3 continue
      r1_top = n1 * da1
      r2_top = n2 * da2
      r3_top = n3 * da3

      d1_top = da1
      d2_top = da2
      d3_top = da3

      print'(/,'' enter the size to one side of the smoothing operator''
     1,/,'' in each of the X,Y,Z directions at the top of the model''
     1,/,'' the average will be computed at dx_smooth increments ''
     1,/,''over this distance to either side.'')'

      if (mod_type .eq. 'G3DL') then

        print'(/,'' enter the top    one sided smoothing operator size''
     1,/,'' defaults dx='',f10.4,''  dy='',f10.4,''  dz='',f10.4)'
     1,r1_top,r2_top,r3_top
      crd80 = ' '
      read(*,'(a)',err=3,end=4)crd80
      if (crd80 .ne. ' ') read(crd80,*,err=3,end=4)r1_top,r2_top,r3_top
    4 continue

      r1_bot = r1_top
      r2_bot = r2_top
      r3_bot = r3_top

        print'(/,'' enter the bottom one sided smoothing operator size''
     1,/,'' defaults dx='',f10.4,''  dy='',f10.4,''  dz='',f10.4)'
     1,r1_bot,r2_bot,r3_bot
        crd80 = ' '
        read(*,'(a)',err=3,end=41)crd80
        if (crd80 .ne. ' ') 
     1read(crd80,*,err=3,end=41)r1_bot,r2_bot,r3_bot
   41 continue

        print'(/,'' enter the top    smoothing sum increment''
     1,/,'' defaults dx='',f10.4,''  dy='',f10.4,''  dz='',f10.4)'
     1,d1_top,d2_top,d3_top
      crd80 = ' '
      read(*,'(a)',err=3,end=2)crd80
      if (crd80 .ne. ' ') 
     1read(crd80,*,err=3,end=2)d1_top,d2_bot,d3_top
    2 continue

      d1_bot = d1_top
      d2_bot = d2_top
      d3_bot = d3_top

        print'(/,'' enter the bottom smoothing sum increment''
     1,/,'' defaults dx='',f10.4,''  dy='',f10.4,''  dz='',f10.4)'
     1,d1_bot,d2_bot,d3_bot
      crd80 = ' '
      read(*,'(a)',err=3,end=21)crd80
      if (crd80 .ne. ' ') 
     1read(crd80,*,err=3,end=21)d1_bot,d2_bot,d3_bot
   21 continue

      else    ! if (mod_type .eq. 'G3DL') then

        print'(/,'' enter the top    one sided smoothing operator size''
     1,/,'' defaults dx='',f10.4,''  dy='',f10.4,'' dz='',f10.4)'
     1,r2_top,r3_top,r1_top
      crd80 = ' '
      read(*,'(a)',err=3,end=40)crd80
      if (crd80 .ne. ' ') read(crd80,*,err=3,end=40)r2_top,r3_top,r1_top
   40 continue

      r1_bot = r1_top
      r2_bot = r2_top
      r3_bot = r3_top

        print'(/,'' enter the bottom one sided smoothing operator size''
     1,/,'' defaults dx='',f10.4,''  dy='',f10.4,''  dz='',f10.4)'
     1,r2_top,r3_top,r1_top
        crd80 = ' '
        read(*,'(a)',err=3,end=42)crd80
        if (crd80 .ne. ' ') 
     1read(crd80,*,err=3,end=42)r2_bot,r3_bot,r1_bot
   42 continue

        print'(/,'' enter the top    smoothing sum increment''
     1,/,'' defaults dx='',f10.4,''  dy='',f10.4,'' dz='',f10.4)'
     1,d2_top,d3_top,d1_top
      crd80 = ' '
      read(*,'(a)',err=3,end=20)crd80
      if (crd80 .ne. ' ') 
     1read(crd80,*,err=3,end=20)d2_top,d3_top,d1_top
   20 continue

      d1_bot = d1_top
      d2_bot = d2_top
      d3_bot = d3_top
        print'(/,'' enter the bottom smoothing sum increment''
     1,/,'' defaults dx='',f10.4,'' dy='',f10.4,''  dz='',f10.4)'
     1,d2_bot,d3_bot,d1_bot
      crd80 = ' '
      read(*,'(a)',err=3,end=22)crd80
      if (crd80 .ne. ' ') 
     1read(crd80,*,err=3,end=22)d1_bot,d2_bot,d3_bot
   22 continue
      endif    ! if (mod_type .eq. 'G3DL') then

      call rmod_smooth_grid_0(
     1 r1_top,r2_top,r3_top
     1,d1_top,d2_top,d3_top
     1,r1_bot,r2_bot,r3_bot
     1,d1_bot,d2_bot,d3_bot
     1,na1,oa1,da1,na2,oa2,da2,na3,oa3,da3,work(iv_grd)
     1,nb1,ob1,db1,nb2,ob2,db2,nb3,ob3,db3,v_grd,nwk,work(iwk),i_err)
      if (i_err .ne. 0) goto 999

      crd80 = ' '
      write(crd80,'('' RMOD smoothing gridded model'')')
      call rmod_title_to_history(crd80)

      crd80 = ' '
      write(crd80,'(''  input NX:'',i8,'' XMIN:'',f10.2
     1,'' XINC:'',f10.2)')nx,x_min,xinc
      call rmod_card_to_history(crd80)
      crd80 = ' '
      write(crd80,'('' output NX:'',i8,'' XMIN:'',f10.2
     1,'' XINC:'',f10.2)')nx_vel,x0_vel,dx_vel
      call rmod_card_to_history(crd80)

      crd80 = ' '
      write(crd80,'(''  input NY:'',i8,'' YMIN:'',f10.2
     1,'' YINC:'',f10.2)')ny,y_min,yinc
      call rmod_card_to_history(crd80)
      crd80 = ' '
      write(crd80,'('' output NY:'',i8,'' YMIN:'',f10.2
     1,'' YINC:'',f10.2)')ny_vel,y0_vel,dy_vel
      call rmod_card_to_history(crd80)

      crd80 = ' '
      write(crd80,'(''  input NZ:'',i8,'' ZMIN:'',f10.2
     1,'' ZINC:'',f10.2)')nz,z_min,zinc
      call rmod_card_to_history(crd80)
      crd80 = ' '
      write(crd80,'('' output NZ:'',i8,'' ZMIN:'',f10.2
     1,'' ZINC:'',f10.2)')nz_vel,z0_vel,dz_vel
      call rmod_card_to_history(crd80)

      return
  999 continue
      call rmod_pause(' error during grid interpolation',' ')
      print'(/,'' error in rmod_interpolate_grid'')'
      return
      end

c23456789012345678901234567890123456789012345678901234567890123456789012
      subroutine rmod_smooth_grid_0(
     1 r1_top,r2_top,r3_top
     1,d1_top,d2_top,d3_top
     1,r1_bot,r2_bot,r3_bot
     1,d1_bot,d2_bot,d3_bot
     1,na1,oa1,da1,na2,oa2,da2,na3,oa3,da3,a
     1,nb1,ob1,db1,nb2,ob2,db2,nb3,ob3,db3,b)
      implicit none

      real     r1_top,r2_top,r3_top
      real     d1_top,d2_top,d3_top
      real     r1_bot,r2_bot,r3_bot
      real     d1_bot,d2_bot,d3_bot

      integer  na1
      real     oa1,da1
      integer  na2
      real     oa2,da2
      integer  na3
      real     oa3,da3
      real     a(na1,na2,na3)

      integer  nb1
      real     ob1,db1
      integer  nb2
      real     ob2,db2
      integer  nb3
      real     ob3,db3
      real     b(nb1,nb2,nb3)

      integer  ia1,ia2,ia3
      integer  ib1,ib2,ib3
      real     weight,w_sum,b_sum,b0,r0
      real     a1,a2,a3
      real     b1,b2,b3

      integer  n1,n2,n3
      real     r1,r2,r3
      real     d1,d2,d3
      real     q1,f_top,f_bot

c  weight by a gaussian function
      q1 = (nb1 - 1) * db1
      if (q1 .eq. 0.) q1 = 1.

      do ib3 = 1 , nb3
        b3 = (ib3 - 1) * db3 + ob3

        do ib2 = 1 , nb2
          b2 = (ib2 - 1) * db2 + ob2

          do ib1 = 1 , nb1
            b1 = (ib1 - 1) * db1 + ob1

      f_bot = max(0.,min(1.,(b1-ob1)/q1))
      f_top = 1. - f_bot

      r1 = r1_top * f_top + r1_bot * f_bot
      r2 = r2_top * f_top + r2_bot * f_bot
      r3 = r3_top * f_top + r3_bot * f_bot

      d1 = d1_top * f_top + d1_bot * f_bot
      d2 = d2_top * f_top + d2_bot * f_bot
      d3 = d3_top * f_top + d3_bot * f_bot

      n1 = max(0,int(r1/d1))
      n2 = max(0,int(r2/d2))
      n2 = max(0,int(r2/d2))

      if (na1 .le. 1) n1 = 1
      if (na2 .le. 1) n2 = 1
      if (na3 .le. 1) n3 = 1

      r0 = (n1*r1)**2 + (n2*r2)**2 + (n3*r3)**2
      if (r0 .ne. 0.) r0 = 1. / r0

            w_sum = 0.
            b_sum = 0.

            do ia3 = -n3 , n3
              a3 = b3 + ia3 * d3

              do ia2 = -n2 , n2
                a2 = b2 + ia2 * d2

                do ia1 = -n1 , n1
                  a1 = b1 + ia1 * d1

                call util_interpolate(
     1 na1,oa1,da1,na2,oa2,da2,na3,oa3,da3,a
     1,  1, a1,db1,  1, a2,db2,  1, a3,db3,b0)
c                  weight = exp(-(ia1**2+ia2**2+ia3**2)*r0)
                  weight = 1.
                  w_sum = w_sum + weight
                  b_sum = b_sum + weight * b0

                enddo    ! do ia1 = -n1 , n1

              enddo    ! do ia1 = -n1 , n1

            enddo    ! do ia1 = -n1 , n1

            b(ib1,ib2,ib3) = b_sum / w_sum

          enddo    ! do ib1 = 1 , nb1

        enddo    ! do ib2 = 1 , nb2

      enddo    ! do ib3 = 1 , nb3

      return
      end

c23456789012345678901234567890123456789012345678901234567890123456789012
      subroutine rmod_interpolate_grid(mod_type
     1,mv_grd
     1,nx_vel,x0_vel,dx_vel
     1,ny_vel,y0_vel,dy_vel
     1,nz_vel,z0_vel,dz_vel
     1,v_grd
     1,m_work,work)
      character mod_type*(*),crd80*80
      integer   i_work_i,i_work_n
      dimension work(1)
      if (mod_type(1:1) .ne. 'G') then
        print'('' Model mod_type must be grid or G3DL''
     1,'' mod_type='',a16)',mod_type
        call rmod_pause(' ',' ')
        return
      endif
      call rmod_new_grid_size('x',nx,x_min,xinc,nx_vel,x0_vel,dx_vel)
      call rmod_new_grid_size('Y',ny,y_min,yinc,ny_vel,y0_vel,dy_vel)
      if (mod_type .ne. 'G3DL')
     1call rmod_new_grid_size('z',nz,z_min,zinc,nz_vel,z0_vel,dz_vel)

      if (mod_type .eq. 'G3DL') then
        nz = nz_vel
        z_min = z0_vel
        zinc = dz_vel

        na1 = nx
        oa1 = x_min
        da1 = xinc

        na2 = ny
        oa2 = y_min
        da2 = yinc

        na3 = nz
        oa3 = z_min
        da3 = dz_vel

        nb1 = nx_vel
        ob1 = x0_vel
        db1 = dx_vel

        nb2 = ny_vel
        ob2 = y0_vel
        db2 = dy_vel

        nb3 = nz_vel
        ob3 = z0_vel
        db3 = dz_vel

      else

        na2 = nx
        oa2 = x_min
        da2 = xinc

        na3 = ny
        oa3 = y_min
        da3 = yinc

        na1 = nz
        oa1 = z_min
        da1 = zinc

        nb2 = nx_vel
        ob2 = x0_vel
        db2 = dx_vel

        nb3 = ny_vel
        ob3 = y0_vel
        db3 = dy_vel

        nb1 = nz_vel
        ob1 = z0_vel
        db1 = dz_vel

      endif
      nv_grd = na1 * na2 * na3
      nwk = 4 * (nb1 + nb2 + nb3)
      call util_wors(i_work_i,i_work_n,m_work)
      call util_work(i_work_i,i_work_n,iv_grd,nv_grd)
      call util_work(i_work_i,i_work_n,iwk,nwk)
      call util_worc(i_work_i,i_work_n,i_err)

      if (nx_vel*ny_vel*nz_vel .gt. mv_grd .or. i_err .ne. 0) then
        print'('' insufficient memory for new grid''
     1,/,'' mv_grd='',i8,'' nx_vel*ny_vel*nz_vel='',i8,'' i_err='',i8)'
     1,mv_grd,nx_vel*ny_vel*nz_vel,i_err
        return
      endif


c      print*,' na1=',na1,na2,na3,' nb1=',nb1,nb2,nb3
c      print*,' oa1=',oa1,oa2,oa3,' ob1=',ob1,ob2,ob3
c      print*,' da1=',da1,da2,da3,' db1=',db1,db2,db3

      call util_copy(nv_grd,v_grd,work(iv_grd))

      call util_interpolate_1(
     1 na1,oa1,da1
     1,na2,oa2,da2
     1,na3,oa3,da3
     1,work(iv_grd)
     1,nb1,ob1,db1
     1,nb2,ob2,db2
     1,nb3,ob3,db3
     1,v_grd
     1,nwk,work(iwk)
     1,i_err)
      if (i_err .ne. 0) goto 999

      crd80 = ' '
      write(crd80,'('' RMOD interpolating gridded model'')')
      call rmod_title_to_history(crd80)

      crd80 = ' '
      write(crd80,'(''  input NX:'',i8,'' XMIN:'',f10.2
     1,'' XINC:'',f10.2)')nx,x_min,xinc
      call rmod_card_to_history(crd80)
      crd80 = ' '
      write(crd80,'('' output NX:'',i8,'' XMIN:'',f10.2
     1,'' XINC:'',f10.2)')nx_vel,x0_vel,dx_vel
      call rmod_card_to_history(crd80)

      crd80 = ' '
      write(crd80,'(''  input NY:'',i8,'' YMIN:'',f10.2
     1,'' YINC:'',f10.2)')ny,y_min,yinc
      call rmod_card_to_history(crd80)
      crd80 = ' '
      write(crd80,'('' output NY:'',i8,'' YMIN:'',f10.2
     1,'' YINC:'',f10.2)')ny_vel,y0_vel,dy_vel
      call rmod_card_to_history(crd80)

      crd80 = ' '
      write(crd80,'(''  input NZ:'',i8,'' ZMIN:'',f10.2
     1,'' ZINC:'',f10.2)')nz,z_min,zinc
      call rmod_card_to_history(crd80)
      crd80 = ' '
      write(crd80,'('' output NZ:'',i8,'' ZMIN:'',f10.2
     1,'' ZINC:'',f10.2)')nz_vel,z0_vel,dz_vel
      call rmod_card_to_history(crd80)

      return
  999 continue
      call rmod_pause(' error during grid interpolation',' ')
      print'(/,'' error in rmod_interpolate_grid'')'
      return
      end

c23456789012345678901234567890123456789012345678901234567890123456789012
      subroutine rmod_write_generic(h_file,mod_type,nbh,b_name
     1,nx_vel,ny_vel,nz_vel,v_grd)
      implicit none
      integer nbh,nx_vel,ny_vel,nz_vel
      real v_grd(nx_vel,ny_vel,nz_vel)
      character h_file*(*),mod_type*(*),b_name(1)*16

      integer i_file,n0,i_err,ix,iy,iz,lf
      character crd80*80,file*64,ans*1
      if (mod_type(1:4) .ne. 'G3DL') return
      call rmod_ans(ans,'N','Y','N',' create generic horizon files?')
      if (ans .eq. 'N') return

      print'('' number of gridded horizons='',i8
     1,'' number of horizon names='',i8)',nz_vel,nbh
      crd80 = ' '
      write(crd80,'(a)')' RMOD writing generic files'
      call rmod_title_to_history(crd80)
      crd80 = ' '
      write(crd80,'('' Number of gridded horizons:'',i8)')nz_vel
      call rmod_card_to_history(crd80)

      do iz = 1 , nz_vel
        if (iz .le. nbh) then
          call util_copy_file_name(b_name(iz),file)
        else
          call util_copy_file_name(h_file,file)
        endif
        print'('' horizon number ='',i8,'' horizon name ='',a16)'
     1,iz,b_name(min(iz,nbh))
        print'('' creating generic file for horizon '',i8)',iz
        call util_get_file_name(' Enter output generic file',file,'Gen')
        call rmodopen(i_file,file,'FORMATTED','NEW','SEQUENTIAL'
     1,'IEEE',n0,i_err)
        call rmodlenr(lf,file)
        crd80 = ' '
        write(crd80,'('' horizon number :'',i8,'' horizon name :'',a16
     1,'' file name:'',a)')iz,b_name(min(iz,nbh)),file(1:lf)
        call rmod_card_to_history(crd80)

        if (i_err .eq. 0) then
          do ix = 1 , nx_vel
            write(i_file,'(8f10.4)')(v_grd(ix,iy,iz),iy=ny_vel,1,-1)
          enddo    ! do ix = 1 , nx_vel
          call rmodclof(file)
        else    ! if (i_err .eq. 0) then
          print'('' could not open file for horizon='',i8)',iz
        endif    ! if (i_err .eq. 0) then
      enddo    ! do iz = 1 , nz_vel
      return
      end

c23456789012345678901234567890123456789012345678901234567890123456789012
      subroutine rmod_make_transform(m_cord,n_cord,x_cord,cord)
c  this creates a transform file from an mgd file
c  and adds defaults for others

      implicit none

      integer   m_cord,n_cord
      character cord(m_cord)*(*)
      real      x_cord(m_cord,2)

c mgd parameters
      character file*80,ans*1,crd80*80
      integer nroff,numch,nptb
      real rin,sp1,ac,spin,sin

c other stuff
      real xg1,xg2,xb1,xb2,xa1,xa2
      integer i_file,lf,i_err,imet

      print'(/,'' This option creates a transform file''
     1,/,'' you can read information from an mgd file''
     1,'' or edit the transforms directly.'')'

      call rmod_ans(ans,'Y','Y','N',' use metric measurements?')
      if (ans .eq. 'Y') then
        imet = -1    ! metric units
      else
        imet = +1    ! english units
      endif
      call rmodpmet(imet)    ! set metric flag to metric

      n_cord = 0

c get mgd file name
      file = 'NONE'
      call util_get_file_name(
     1' Enter mgd cinf file name for transform info',file,'cinf')
      call rmodlenr(lf,file)

      if (file(1:4) .ne. 'NONE') then
        call rmodopen(i_file,file,'FORMATTED','OLD','SEQUENTIAL'
     1,'IEEE',0,i_err)
        if (i_err .ne. 0) goto 991
        crd80 = ' '
        write(crd80,'('' RMOD creating transforms from MGD file:''
     1,a)')file(1:lf)
        call rmod_title_to_history(crd80)

c find the start of the mgd data card section
        call rmodfsts(i_file,1,' mgd ',*992)

c  read mgd file parameters
        call rmodgvi(nroff,'NROFF',i_file)
        call rmodgvi(nptb ,'NPTB' ,i_file)
        call rmodgvi(numch,'NUMCH',i_file)
        call rmodgvr(rin  ,'RIN'  ,i_file)
        call rmodgvr(sin  ,'SIN'  ,i_file)
        call rmodgvr(sp1  ,'SP1'  ,i_file)
        call rmodgvr(spin ,'SPIN' ,i_file)
        call rmodgvr(ac   ,'AC'   ,i_file)

c        write(6,*) 'NROFF=',nroff
c        write(6,*) 'NPTB=',nptb
c        write(6,*) 'NUMCH=',numch
c        write(6,*) 'RIN=',rin
c        write(6,*) 'SIN=',sin
c        write(6,*) 'SP1=',sp1
c        write(6,*) 'SPIN=',spin
c        write(6,*) 'AC=',ac

c prepare output
        xg1 = 1.
        xg2 = float(numch) + float(nptb-1)*2.*abs(sin/rin)
        call rmodgvr(xg1  ,'XGRID1',i_file)
        call rmodgvr(xg2  ,'XGRID2',i_file)

        xb1 = (float(nroff)+rin*float(numch-1))/2. + (xg1-1.)*rin/2.
        xb2 = (float(nroff)+rin*float(numch-1))/2. + (xg2-1.)*rin/2.
        xa1 = sp1
     1- (2.*ac + float(nroff) + rin*float(numch-1))/(2.*spin)
     1   + (xg1-1.)*rin/(2.*spin)
        xa2 = sp1
     1- (2.*ac + float(nroff) + rin*float(numch-1))/(2.*spin)
     1   + (xg2-1.)*rin/(2.*spin)
c write comments
c        write(6,*) '       first grid value=',xg1
c        write(6,*) '        last grid value=',xg2
c        write(6,*) '   first basement value=',xb1
c        write(6,*) '    last basement value=',xb2
c        write(6,*) ' first annotation value=',xa1
c        write(6,*) '  last annotation value=',xa2
        call rmodclof(file)    ! close mgd file
        call rmodacor('XANNOTATION',xa1,xa2,m_cord,n_cord,x_cord,cord)
        call rmodacor('XBASEMENT'  ,xb1,xb2,m_cord,n_cord,x_cord,cord)
        call rmodacor('XGRID'      ,xg1,xg2,m_cord,n_cord,x_cord,cord)
      endif    ! if (file .ne. 'NONE') then

c  add defaults
      call rmoddtrn(m_cord,n_cord,x_cord,cord)

c  edit transforms
      call rmod_edit_transforms(m_cord,n_cord,x_cord,cord,' ',' ',' ')

c get output file name
      call rmod_efil(file,file)
      call util_get_file_name(' Enter transform file name',file,'trans')
      call rmodlenr(lf,file)
      call rmodwtrn(file,m_cord,n_cord,x_cord,cord,*993)

      return

  991 continue
      print'(/,'' error during open file:'',a)',file(1:lf)
      return

  992 continue
      print'(/,'' error finding mgd section in file:'',a)',file(1:lf)
      return

  993 continue
      print'(/,'' error during open file:'',a)',file(1:lf)
      return

      end

c23456789012345678901234567890123456789012345678901234567890123456789012
      subroutine rmod_ans(ans,default,ans1,ans2,title)
      implicit none
      character *(*) ans,ans1,ans2,default,title
      character title0*80
      title0 = title//' ('//ans1//','//ans2//')'
    1 continue
      call rmod_read_string(ans,default,title0)
      if (ans .ne. ans1 .and. ans .ne. ans2) goto 1
      return
      end

c23456789012345678901234567890123456789012345678901234567890123456789012
      subroutine rmod_read_string(str,default,title)
      implicit none
      character *(*) str,default,title
      character default0*80,crd80*80
      integer ls,lt,lc
      call rmodlenr(lt,title)
      default0 = default
    1 continue
      str = default0
      call rmodlenr(ls,str)
      crd80 = title(1:lt)//' default='//str(1:ls)
      call rmodlenr(lc,crd80)
      print'(/,a)',crd80(1:lc)
      call rmodlenr(lc,crd80)
      read(*,'(a)',err=1,end=2)str
    2 continue
      if (str .eq. ' ') str = default0
      call rmodcaps(str,str)
      return
      end

c23456789012345678901234567890123456789012345678901234567890123456789012
      subroutine rmod_read_integer(data,default,title)
      implicit none
      character title*(*)
      integer   data,default
      character crd80*80
      integer   lt,lc

      call rmodlenr(lt,title)
    1 continue
      data = default
      crd80 = title(1:lt)//' default='
      write(crd80(lt+10:lt+17),'(i8)')data
      call rmodlenr(lc,crd80)
      print'(/,a)',crd80(1:lc)
      crd80 = ' '
      read(*,'(a)',err=1,end=2)crd80
      if (crd80 .ne. ' ') read(crd80,*,err=1)data
    2 continue

      return
      end

c23456789012345678901234567890123456789012345678901234567890123456789012
      subroutine rmod_read_float(data,default,title)
      implicit none
      character title*(*)
      real      data,default
      character crd80*80
      integer   lt,lc

      call rmodlenr(lt,title)
    1 continue
      data = default
      crd80 = title(1:lt)//' default='
      write(crd80(lt+10:lt+25),'(g16.9)')data
      call rmodlenr(lc,crd80)
      print'(/,a)',crd80(1:lc)
      crd80 = ' '
      read(*,'(a)',err=1,end=2)crd80
      if (crd80 .ne. ' ') read(crd80,*,err=1)data
    2 continue

      return
      end

c23456789012345678901234567890123456789012345678901234567890123456789012
      subroutine rmod_scale_2d_vel(mod_type,nv,imv,itv,ixv,nxv,vel)
      implicit  none
      integer   nv,imv(1),itv(1),ixv(1),nxv(1)
      real      vel(1)
      character mod_type*(*)

      integer   lv,jmv,jtv,i,lt,iv,n_change
      real      scale,v1,v2,c1,s1,e1
      character crd80*80

      call rmodlenr(lt,mod_type)

      print'(/,'' This option operates on a 2D layer velocity file''
     1,/,'' It will allow you to modify the layer velocity value via''
     1,'' an equation of the form''
     1,/,'' new value = c1 + s1 * old value**e1''
     1,/,'' velocity file mod_type='',a)',mod_type(1:lt)

      if (mod_type .ne. 'LAYER') then
        print'('' Model mod_type must be layer''
     1,'' mod_type='',a16)',mod_type
        call rmod_pause(' ',' ')
        return
      endif

      lv = ixv(nv)+nxv(nv)
      print'(
     1 /,'' number of velocity horizons='',i8
     1,/,'' number of velocity nodes   ='',i8
     1)',nv,lv
      crd80 = ' '
      write(crd80,'(a)')' RMOD scaling 2D layered velocities'
      call rmod_title_to_history(crd80)
      print'(''      layer  vel-id-1 vel-id-2 velocity'')'

      do iv = 1 , nv
        i = ixv(iv)+1
      print'(1x,i8,1x,i8,1x,i8,1x,f12.4)',iv,imv(iv),itv(i),vel(i)
        do i = ixv(iv)+2 , ixv(iv)+nxv(iv)
          if (itv(i) .ne. itv(i-1))
     1print'(1x,i8,1x,i8,1x,i8,1x,f12.4)',iv,imv(iv),itv(i),vel(i)
        enddo    ! do i = ixv(iv)+2 , ixv(iv)+nxv(iv)
      enddo    ! do i = ixv(iv)+2 , ixv(iv)+nxv(iv)

    1 continue

      jmv = -1
      jtv = -1
      c1 = 0.
      s1 = 1.
      e1 = 1.

      print'(/,'' enter the velocity boundary flags and coefficients''
     1,'' enter -1,-1 to end''
     1,/,'' enter 0 for flag 1 or 2 to get all values of that flag''
     1,/,'' defaults flag1='',i8,'' flag2='',i8
     1,'' c1='',f10.4,'' s1='',f10.4,'' e1='',f10.4)'
     1,jmv,jtv,c1,s1,e1
      crd80 = ' '
      read(*,'(a)')crd80
      if (crd80 .ne. ' ') read(crd80,*,err=1,end=2)jmv,jtv,c1,s1,e1

    2 continue

      if (jmv .ne. -1 .and. jtv .ne. -1) then

        print'('' flag1='',i8,'' flag2='',i8
     1,'' c1='',f10.4,'' s1='',f10.4,'' e1='',f10.4)'
     1,jmv,jtv,c1,s1,e1

        n_change = 0

        do iv = 1 , nv
          do i = ixv(iv)+1 , ixv(iv)+nxv(iv)
            if (
     1      (jmv .eq. 0 .or. jmv .eq. imv(iv))
     1.and. (jtv .eq. 0 .or. jtv .eq. itv(iv))
     1) then
              v1 = vel(i)
              vel(i) = c1 + s1 * vel(i) ** e1
              v2 = vel(i)
              n_change = n_change + 1
      if (iv .le. 100 .or. mod(iv,100) .eq. 0
     1.or. iv.eq. nv)
     1print'('' i='',i8,'' n='',i8
     1,'' old value='',f12.4,'' new value='',f12.4)'
     1,i,n_change,v1,v2
            endif    ! if (jtv .eq. itv(i)) then
          enddo    ! do i = ixv(iv)+1 , ixv(iv)+nxv(iv)
        enddo    ! do iv = 1 , nv


        write(6,'('' n='',i6,'' f1:'',i5,'' f2:'',i5
     1,'' c1:'',f10.4,'' s1:'',f10.4,'' e1:'',f10.4)')
     1n_change,jmv,jtv,c1,s1,e1
        if (n_change .ne. 0) then
        crd80 = ' '
        write(crd80,'('' n='',i6,'' f1:'',i5,'' f2:'',i5
     1,'' c1:'',f10.4,'' s1:'',f10.4,'' e1:'',f10.4)')
     1n_change,jmv,jtv,c1,s1,e1
        call rmod_card_to_history(crd80)

        endif
        goto 1
      endif    ! if (jtv .ne. -1) then

      return
      end

c23456789012345678901234567890123456789012345678901234567890123456789012
      subroutine rmod_scale_3d_vel(mod_type,nv,itv,ixv,nxv,vel)
      implicit  none
      integer   nv,itv(1),ixv(1),nxv(1)
      real      vel(1)
      character mod_type*(*)

      integer   jitv,jixv,jnxv,iv,jv,i,lt,n_change
      real      c1,s1,e1,scale,v1,v2
      character crd80*80

      call rmodlenr(lt,mod_type)
      print'(/,'' This option scales 3d layered velocities''
     1,'' new velocity = old velocity * scale''
     1,/,'' velocity file mod_type='',a)',mod_type(1:lt)

      if (mod_type .ne. 'G3DL') then
        print'('' Model mod_type must be G3DL mod_type='',a16)',mod_type
        call rmod_pause(' ',' ')
        return
      endif

      print'('' number of velocity horizons='',i8)',nv

      jv = -999
      do iv = 1 , nv

        if (iv .eq. nv .or. itv(iv) .ne. jv) then

c          print'(/,'' jv='',i6,'' f1='',i5,'' f2='',i5,'' f3='',i5
c     1,'' vel='',f10.2)'
c     1,jv,itv(jv),ixv(jv),nxv(jv),vel(jv)

          print'('' i='',i6,'' f1='',i5,'' f2='',i5,'' f3='',i5
     1,'' vel='',f10.2)'
     1,iv,itv(iv),ixv(iv),nxv(iv),vel(iv)

        endif    ! if (iv .eq. 1 .or. itv(iv) .ne. itv(jv) then

          jv = itv(iv)

      enddo    ! do iv = 1 , nv
      print'(/)'

      crd80 = ' '
      write(crd80,'(a)')' RMOD scaling 3D layered velocities'
      call rmod_title_to_history(crd80)

    1 continue

      jitv = -1
      jixv = -1
      jnxv = -1
      c1 = 0.
      s1 = 1.
      e1 = 1.

      print'(/,'' enter the velocity boundary flags and coefficients''
     1,'' enter -1,-1 to end''
     1,/,'' enter 0 for flag 1,2 or 3 to get all values of that flag''
     1,/,'' defaults f1='',i5,'' f2='',i5,'' f3='',i5
     1,'' c1='',f10.4,'' s1='',f10.4,'' e1='',f10.4)'
     1,jitv,jixv,jnxv,c1,s1,e1
      crd80 = ' '
      read(*,'(a)')crd80
      if (crd80 .ne. ' ')
     1read(crd80,*,err=1,end=2)jitv,jixv,jnxv,c1,s1,e1

    2 continue

      if (jitv .ne. -1 .and. jixv .ne. -1 .and. jnxv .ne. -1) then

        print'('' f1='',i5,'' f2='',i5,'' f3='',i5
     1,'' c1='',f10.4,'' s1='',f10.4,'' e1='',f10.4)'
     1,jitv,jixv,jnxv,c1,s1,e1

        n_change = 0

        do iv = 1 , nv

c          print'('' iv='',i5,'' f1='',i5,'' f2='',i5,'' f3='',i5
c     1,'' vel='',f10.2)'
c     1,iv,itv(iv),ixv(iv),nxv(iv),vel(iv)

            if (
     1      (jitv .eq. 0 .or. jitv .eq. itv(iv))
     1.and. (jixv .eq. 0 .or. jixv .eq. ixv(iv))
     1.and. (jnxv .eq. 0 .or. jnxv .eq. nxv(iv))
     1) then

              v1 = vel(iv)
              vel(iv) = c1 + s1 * vel(iv) ** e1
              v2 = vel(iv)
              n_change = n_change + 1

      if (iv .le. 100 .or. mod(iv,100) .eq. 0
     1.or. iv.eq. nv)
     1print'('' i='',i8,'' n='',i8
     1,'' old value='',f12.4,'' new value='',f12.4)'
     1,iv,n_change,v1,v2

            endif    ! if (jixv .eq. ixv(iv)) then

        enddo    ! do iv = 1 , nv

          write(6,'('' n='',i6
     1,'' f1:'',i5,'' f2:'',i5,'' f3:'',i5
     1,'' c1:'',f10.4,'' s1:'',f10.4,'' e1:'',f10.4)')
     1n_change,jitv,jixv,jnxv,c1,s1,e1
        if (n_change .ne. 0) then
          crd80 = ' '
          write(crd80,'('' n='',i6
     1,'' f1:'',i5,'' f2:'',i5,'' f3:'',i5
     1,'' c1:'',f10.4,'' s1:'',f10.4,'' e1:'',f10.4)')
     1n_change,jitv,jixv,jnxv,c1,s1,e1
          call rmod_card_to_history(crd80)
         endif

        goto 1

      endif    ! if (jixv .ne. -1) then


      return
      end

c23456789012345678901234567890123456789012345678901234567890123456789012
      subroutine rmod_read_shein(h_file,d_file,mod_type
     1,cxi,cyi,czi,m_cord,n_cord,x_cord,cord
     1,x_min,x_max,y_min,y_max,z_min,z_max,z_datum
     1,ibw1,ibw2,ibw3,mbh,nbh,ibid,nbseg,b_name,b_color
     1,ivw1,ivw2,ivw3,mvh,nvh,ivid,nvseg,v_name,v_color
     1,icw1,icw2,icw3,mch,nch,icid,ncseg,c_name,c_color
     1,mb,mxb,nb,imb,itb,ixb,nxb,xb,zb,nv,imv,itv,ixv,nxv,xv,yv,zv,vel
     1,ncv,icv,xcv,ycv,zcv
     1,nx_vel,x0_vel,dx_vel
     1,ny_vel,y0_vel,dy_vel
     1,nz_vel,z0_vel,dz_vel
     1)
c  read a header file
      implicit  none
      integer   m_cord,n_cord,nx_vel,ny_vel,nz_vel
      integer   mb,mxb,nb,imb(1),itb(1),ixb(1),nxb(1)
      integer   nv,imv(1),itv(1),ixv(1),nxv(1)
      integer   ncv,icv(1)
      integer   ibw1,ibw2,ibw3,mbh,nbh,ibid,nbseg
      integer   ivw1,ivw2,ivw3,mvh,nvh,ivid,nvseg
      integer   icw1,icw2,icw3,mch,nch,icid,ncseg

      real      x_cord(2,1)
      real      x_min,x_max,y_min,y_max,z_min,z_max,z_datum
      real      x0_vel,dx_vel,y0_vel,dy_vel,z0_vel,dz_vel
      real      xb(1),zb(1),xv(1),yv(1),zv(1),vel(1)
      real      xcv(1),ycv(1),zcv(1)
      character *(*) h_file,d_file,mod_type,cxi,cyi,czi,cord(*)
     1,b_name(*),b_color(*),v_name(*),v_color(*),c_name(*),c_color(*)

      integer   i_file,n0,i_err,nhmod,ib,idb,j,nxb1,lh
      real      shot,base,baseinc,xtop(2),ztop(2)
      character crd80*80

c  read model in sheins format
c  add defaults to transform list

      print'(/,'' This option reads in a model file in Sheins format'')'
    1 continue
      call util_copy_file_name(h_file,h_file)
      call util_get_file_name(' Enter file name',h_file,'mod')
      call rmod_inquire_exist(h_file,*1)

      crd80 = ' '
      call rmodlenr(lh,h_file)
      crd80 = ' '
      write(crd80,'(a)')' RMOD reading file in SHIEN format'
      call rmod_title_to_history(crd80)
      write(crd80,'('' Input file:'',a)')h_file(1:lh)
      call rmod_card_to_history(crd80)

      call rmod_shein(h_file,d_file,mod_type
     1,cxi,cyi,czi,m_cord,n_cord,x_cord,cord
     1,x_min,x_max,y_min,y_max,z_min,z_max,z_datum
     1,ibw1,ibw2,ibw3,mbh,nbh,ibid,nbseg,b_name,b_color
     1,ivw1,ivw2,ivw3,mvh,nvh,ivid,nvseg,v_name,v_color
     1,icw1,icw2,icw3,mch,nch,icid,ncseg,c_name,c_color
     1,mb,mxb,nb,imb,itb,ixb,nxb,xb,zb,nv,imv,itv,ixv,nxv,xv,yv,zv,vel
     1,ncv,icv,xcv,ycv,zcv
     1,nx_vel,x0_vel,dx_vel
     1,ny_vel,y0_vel,dy_vel
     1,nz_vel,z0_vel,dz_vel
     1)

      return

      end

c23456789012345678901234567890123456789012345678901234567890123456789012
      subroutine rmod_read_ascii_0(
     1i_ask_for_file,h_file,d_file,word_type,mod_type
     1,cxi,cyi,czi,m_cord,n_cord,x_cord,cord
     1,x_min,x_max,y_min,y_max,z_min,z_max,z_datum
     1,ibw1,ibw2,ibw3,mbh,nbh,ibid,nbseg,b_name,b_color
     1,ivw1,ivw2,ivw3,mvh,nvh,ivid,nvseg,v_name,v_color
     1,icw1,icw2,icw3,mch,nch,icid,ncseg,c_name,c_color
     1,mb,mxb,nb,imb,itb,ixb,nxb,xb,zb,nv,imv,itv,ixv,nxv,xv,yv,zv,vel
     1,ncv,icv,xcv,ycv,zcv,mc,mxc,nc,imc,ixc,nxc,xc,zc
     1,mv_grd
     1,nx_vel,x0_vel,dx_vel
     1,ny_vel,y0_vel,dy_vel
     1,nz_vel,z0_vel,dz_vel
     1,v_grd
     1,m_work,work,lu)
c  rewrite an ascii column file to a grid file
      implicit  none
      integer   i_ask_for_file,m_cord,n_cord
     1,ibw1,ibw2,ibw3,mbh,nbh,ibid,nbseg
     1,ivw1,ivw2,ivw3,mvh,nvh,ivid,nvseg
     1,icw1,icw2,icw3,mch,nch,icid,ncseg
     1,mb,mxb,nb,imb,itb,ixb,nxb,nv,imv,itv,ixv,nxv
     1,ncv,icv,mc,mxc,nc,imc,ixc,nxc
     1,mv_grd,nx_vel,ny_vel,nz_vel,m_work,lu
      real      x_cord(2,m_cord)
     1,x_min,x_max,y_min,y_max,z_min,z_max,z_datum
     1,xb,zb,xv,yv,zv,vel,xcv,ycv,zcv,xc,zc
     1,x0_vel,dx_vel,y0_vel,dy_vel,z0_vel,dz_vel,v_grd(mv_grd),z_lay(1)
     1,work(m_work)
      character *(*)
     1 h_file,d_file,word_type,mod_type,cxi,cyi,czi,cord(*)
     1,b_name(*),b_color(*),v_name(*),v_color(*),c_name(*),c_color(*)
      logical exist

      integer   iz,i_file,i_err,nx,ny,nz,lc
      real      eps,x,y,z,x0,y0,z0,x1,y1,z1
      character crd80*80,t_file*80
      data      eps/1e-5/

      print'(
     1 /,'' This option reads a gridded model in ascii format''
     1,/,'' Note you may need to edit the transform coordinates ''
     1,''as they will take default values'')'

    1 continue
      call util_copy_file_name(h_file,h_file)
      call util_get_file_name(' Enter ascii file name',h_file,'xtvy')
      call rmod_inquire_exist(h_file,*1)
      call rmodopen(i_file,h_file,'FORMATTED','OLD','SEQUENTIAL'
     1,'IEEE',0,i_err)
      if (i_err .ne. 0) goto 999

      iz = 0
      dx_vel = -999.
      dy_vel = -999.
      dz_vel = -999.
      nx_vel = 0
      ny_vel = 0
      nz_vel = 0
      nx = 1
      ny = 1
      nz = 1

    2 continue
      read(i_file,*,err=999,end=3)x,z,v_grd(iz+1),y
      iz = min(iz+1,mv_grd)

c  initialize some values
      if (iz .eq. 1) then
        x0 = x
        y0 = y
        z0 = z
        x1 = x
        y1 = y
        z1 = z
        x_min = x
        x_max = x
        y_min = y
        y_max = y
        z_min = z
        z_max = z
        x0_vel = x
        y0_vel = y
        z0_vel = z
        print'('' Found the first X, x_min='',f10.2)',x_min
        print'('' Found the first Y, y_min='',f10.2)',y_min
        print'('' Found the first Z, z_min='',f10.2)',z_min
      endif

c  set spacings if they have not already been set
      if (dx_vel .eq. -999. .and. abs(x-x0) .gt. eps)
     1print'('' Found X spacing dx_vel='',f10.2)',x-x0
      if (dy_vel .eq. -999. .and. abs(y-y0) .gt. eps)
     1print'('' Found Y spacing dy_vel='',f10.2)',y-y0
      if (dz_vel .eq. -999. .and. abs(z-z0) .gt. eps)
     1print'('' Found Z spacing dz_vel='',f10.2)',z-z0

      if (dx_vel .eq. -999. .and. abs(x-x0) .gt. eps) dx_vel = x - x0
      if (dy_vel .eq. -999. .and. abs(y-y0) .gt. eps) dy_vel = y - y0
      if (dz_vel .eq. -999. .and. abs(z-z0) .gt. eps) dz_vel = z - z0

c  set number of grid points if they have not already been set
      if (nz_vel .eq. 0 .and. abs(x-x0) .gt. eps)
     1print'('' Number of Z values nz_vel='',i8)',iz-1
      if (nz_vel .eq. 0 .and. abs(x-x0) .gt. eps) nz_vel = iz - 1

c  if this is a new line make sure number of depths has been set
      if (nx_vel .eq. 0 .and. abs(y-y0) .gt. eps .and. nz_vel .eq. 0)
     1 then
        print'(/,'' error reading file found line change before''
     1,'' depth change y='',f10.2,'' y0='',f10.2)',y,y0
        goto 999
      endif

      if (nx_vel .eq. 0 .and. abs(y-y0) .gt. eps)
     1print'('' Number of X values nx_vel='',i8)',(iz-1)/nz_vel
      if (nx_vel .eq. 0 .and. abs(y-y0) .gt. eps) nx_vel = (iz - 1) /
     1 nz_vel

c  set min, max values
      x_min = min(x,x_min)
      x_max = max(x,x_max)
      y_min = min(y,y_min)
      y_max = max(y,y_max)
      z_min = min(z,z_min)
      z_max = max(z,z_max)

c  if the x value changes
      if (abs(x-x0) .gt. eps) then

        if (nz .ne. nz_vel)
     1print'('' Found a different number of z values nx='',i8
     1,'' ny='',i8,'' nz='',i8,'' nz_vel='',i8)',nx,ny,nz,nz_vel

c  if the y value changes
        if (abs(y-y0) .gt. eps) then

          ny = ny + 1

      if (ny .le. 20 .or. mod(ny,10) .eq. 1)
     1print'('' Found the start of new a Y NY='',i8,'' Y='',f10.2
     1,'' NX='',i8)',ny,y,nx

          if (nx .ne. nx_vel)
     1print'('' Found a different number of X values nx='',i8
     1,'' ny='',i8,'' nz='',i8,'' nx_vel='',i8)',nx,ny,nz,nx_vel

          if (abs((y-y0)-dy_vel) .gt. eps .and. abs(y-y1) .gt. eps)
     1print'('' Found a difference in this Y spacing='',f10.4
     1,'' and the first spacing='',f10.4)',y-y0,dy_vel

          if (abs(x-x1) .gt. eps)
     1print'('' Found a difference in this X value='',f10.2
     1,'' and the first X value='',f10.2)',x,x1

          nx = 0
          y0 = y

        else    ! if (abs(y-y0) .gt. eps) then

          if (abs((x-x0)-dx_vel) .gt. eps .and. abs(x-x1) .gt. eps)
     1print'('' Found a difference in this X spacing='',f10.4
     1,'' and the first spacing='',f10.4)',x-x0,dx_vel

        endif    ! if (abs(y-y0) .gt. eps) then

        nx = nx + 1

        if (ny .eq. 1 .and. (nx .le. 20 .or. mod(nx,10) .eq. 1))
     1print'('' Found the start of new a X NX='',i8,'' X='',f10.2
     1,'' NZ='',i8)',nx,x,nz


        if (abs(z-z1) .gt. eps)
     1print'('' Found a difference in this Z value='',f10.2
     1,'' and the first Z value='',f10.2)',z,z1

        nz = 0
        x0 = x

      else    ! if (abs(x-x0) .gt. eps) then

        if (abs((z-z0)-dz_vel) .gt. eps .and. abs(z-z1) .gt. eps)
     1print'('' Found a difference in this Z spacing='',f10.4
     1,'' and the first spacing='',f10.4)',z-z0,dz_vel

        if (abs((z-z0)-dz_vel) .gt. eps .and. abs(z-z1) .gt. eps)
     1print*,' z=',z,' z0=',z0,' dz_vel=',dz_vel,' eps=',eps
        if (abs((z-z0)-dz_vel) .gt. eps .and. abs(z-z1) .gt. eps)
     1stop

      endif    ! if (abs(x-x0) .gt. eps) then

      nz = nz + 1
      z0 = z

      goto 2

    3 continue

      if (nz_vel .eq. 0) nz_vel = nz
      if (nx_vel .eq. 0) nx_vel = nx
      ny_vel = iz / (nx_vel * nz_vel)

      call rmodclos(i_file)
      mod_type = 'GRID'

      print'(
     1 /,'' number of X values found in file     ='',i8
     1,/,'' number of Y values found in file     ='',i8
     1,/,'' number of Z values found in file     ='',i8
     1,/,'' total number of X,Y,Z points in model='',i8
     1,/,'' total number of points read from file:'',i8)'
     1,nx_vel,ny_vel,nz_vel,nx_vel*ny_vel*nz_vel,iz

      if (nx_vel*ny_vel*nz_vel .ne. iz)
     1print'('' Note there is a discrepency between the number''
     1,'' of points read from this file''
     1,/,'' and the number of points in the velocity functions.''
     1,/,'' There should be the same number of x values for each Y''
     1,'' and the same number of Z values for each X.'')'

      cxi = 'XANNOTATION'
      cyi = 'YANNOTATION'
      czi = 'TIME'

      call rmod_transform_defaults(cxi,cyi,czi
     1,m_cord,n_cord,x_cord,cord)

      call rmodlenr(lc,h_file)
      crd80 = ' '
      write(crd80,'(
     1'' RMOD reading gridded model from file '',a)')h_file(1:lc)
      call rmod_title_to_history(crd80)

      call util_invert(nx_vel*ny_vel*nz_vel,v_grd)

      return

  999 continue
      call rmod_pause(' error during ascii read read',h_file)
      return

      end

c23456789012345678901234567890123456789012345678901234567890123456789012
      subroutine rmod_transform_defaults(cxi,cyi,czi
     1,m_cord,n_cord,x_cord,cord)

      implicit none
      integer   m_cord,n_cord
      real      x_cord(2,m_cord)
      character cxi*(*),cyi*(*),czi*(*),cord*(*)

      character *80 t_file

    1 continue
      t_file = 'NONE'
      call util_get_file_name(
     1' Enter the transform file name (NONE means enter by hand'
     1,t_file,'TRANS')

      if (t_file(1:4) .ne. 'NONE') then
        call rmod_inquire_exist(t_file,*1)
        call rmodrtrn(t_file,m_cord,n_cord,x_cord,cord,*1)
      else
c  add defaults
        call rmoddtrn(m_cord,n_cord,x_cord,cord)

c  edit transforms
        call rmod_edit_transforms(m_cord,n_cord,x_cord,cord,cxi,cyi,czi)

      endif
      return
      end

c23456789012345678901234567890123456789012345678901234567890123456789012
      subroutine rmod_make_constraint(file,mod_type,nv,imv,ixv,nxv,vel)
      implicit  none
      integer   nv,imv(1),ixv(1),nxv(1)
      real      vel(1)
      character *(*) file,mod_type

      integer   jv,iv,nv_changed,nxv_changed,nvel,i_file,i_err,lt,lf
      real      v0
      character crd80*80

      nvel = ixv(nv) + nxv(nv)

      print'(
     1 /,'' This option replaces layered velocities with ''
     1,''constraint values'')'

      if (mod_type(1:4) .ne. 'G3DL' .and. mod_type(1:1) .ne. 'L') then
        call rmodlenr(lt,mod_type)
        print'('' The model mod_type must be LAYER or G3DL ''
     1,'' for this option mod_type='',a)',mod_type(1:lt)
        call rmod_pause(' ',' ')
        return
      endif

      print'(
     1'' You may either read velocity flag, constraint value pairs''
     1,/,'' from a file or enter them by hand.''
     1,/,'' To enter by hand enter NONE for the file name''
     1,/,'' You have a total of '',i8,'' velocity flags and ''
     1,i8,'' velocity values'')',nv,nvel
      print'(''   velocity  velocity  number   first    last''
     1,/,''    number     flag  of values  value    value'')'
      print'(1x,i8,1x,i8,1x,i8,1x,f14.4,f14.4)',
     1(jv,imv(jv),nxv(jv),vel(ixv(jv)+1),vel(ixv(jv)+nxv(jv)),jv=1,nv)

      call util_copy_file_name(file,file)
    1 continue
      call util_get_file_name(' Enter velocity constraint file name'
     1,file,'dat')

      if (file(1:4) .eq. 'none') file = 'NONE'

      if (file(1:4) .ne. 'NONE') then
        call rmod_inquire_exist(file,*1)
        call rmodopen(i_file,file,'FORMATTED','OLD','SEQUENTIAL'
     1,'IEEE',0,i_err)
        if (i_err .ne. 0) goto 1

      else
        i_file = 6
      endif

      nv_changed = 0
      nxv_changed = 0
    2 continue
      if (file(1:4) .eq. 'NONE') then
        print'(/,'' Number of velocity flags in file:'',i8
     1,'' number changed='',i8)',nv,nv_changed
        print'(/,'' enter the velocity flag, constraint values''
     1,'' enter -999,0. to end'')'
        if (nv_changed .lt. nv) then
          iv = imv(nv_changed+1)
          v0 = 1.
        else
          iv = -999
          v0 = 0.
        endif
        crd80 = ' '
        read(*,'(a)',err=2,end=3)crd80
        if (crd80 .ne. ' ') read(crd80,*,end=3)iv,v0
    3  continue

      else

        read(i_file,*,err=999,end=4)iv,v0

      endif

      if (iv .eq. -999) goto 4

      do jv = 1 , nv
        if (imv(jv) .eq. iv) then
          nv_changed = nv_changed + 1
          nxv_changed = nxv_changed + nxv(jv)
          print'('' found velocity flag match''
     1,'' velocity flag='',i8,'' constraint value='',f14.4
     1,'' velocity number='',i8,'' number of points='',i8)'
     1,iv,v0,jv,nxv(jv)
          call util_setr(nxv(jv),vel(ixv(jv)+1),v0)
        endif
      enddo
      goto 2

    4 continue

      print'(
     1 /,'' number of velocity flags ='',i8,'' number changed='',i8
     1,/,'' number of velocity points='',i8,'' number changed='',i8)'
     1,nv,nv_changed,nvel,nxv_changed
      print'(''   velocity  velocity  number   first    last''
     1,/,''    number     flag  of values  value    value'')'
      print'(1x,i8,1x,i8,1x,i8,1x,f14.4,f14.4)',
     1(jv,imv(jv),nxv(jv),vel(ixv(jv)+1),vel(ixv(jv)+nxv(jv)),jv=1,nv)

      if (nv .ne. nv_changed .or. nvel .ne. nxv_changed)
     1print'(/,'' Note: there is a discrepency between ''
     1,'' the number flags or points in the file''
     1,/,'' and the number changed.'')'

      crd80 = ' '
      write(crd80
     1,'('' rmod changing velocity values to constraint values.'')')
      call rmod_card_to_history(crd80)
      if (file(1:4) .ne. 'NONE') then
        call rmodlenr(lf,file)
        crd80 = ' '
        write(crd80,'('' Using input file '',a)')file(1:lf)
        call rmod_card_to_history(crd80)
      endif

      return

  999 continue
      call rmod_pause(' error during velocity - constraint read',' ')
      return

      end

c23456789012345678901234567890123456789012345678901234567890123456789012
      subroutine rmod_gaussian_grid(mod_type,nx_vel,ny_vel,nz_vel,v_grd
     1,m_work
     1,work)
      implicit  none
      integer   nx_vel,ny_vel,nz_vel,m_work
      real      v_grd(nz_vel,nx_vel,ny_vel),work(m_work)
      character mod_type*(*)

      integer   nxs,nys,nzs
      character crd80*80

      print'(
     1 /,'' This option smooths a gridded velocity file''
     1,/,'' nx_vel='',i8,''  ny_vel='',i8,''
     1 nz_vel='',i8)',nx_vel,ny_vel,nz_vel

      if (mod_type .ne. 'GRID') then
        print'('' Model mod_type must be GRID for this option''
     1,'' mod_type='',a16)',mod_type
        call rmod_pause(' ',' ')
        return
      endif

      if (m_work .lt. nx_vel*ny_vel*nz_vel) then
        print'('' insufficient work space to copy velocity grid''
     1,/,'' m_work='',i8,''
     1 nx_vel*ny_vel*nz_vel='',i8)',m_work,nx_vel*ny_vel*nz_vel
        return
      endif

      call rmod_read_integer(nxs,1
     1,' enter the number of X nodes to smooth over')

      call rmod_read_integer(nys,1
     1,' enter the number of Y nodes to smooth over')

      call rmod_read_integer(nzs,1
     1,' enter the number of Z nodes to smooth over')

      nxs = min(nx_vel,max(1,nxs))
      nys = min(ny_vel,max(1,nys))
      nzs = min(nz_vel,max(1,nzs))

      call rmod_gaussian_grid_0(nxs,nys,nzs,nx_vel,ny_vel,nz_vel
     1,v_grd,work,m_work-nx_vel*ny_vel*nz_vel
     1,work(nx_vel*ny_vel*nz_vel+1))

      crd80 = ' '
      write(crd80,'('' smoothing velocity grid x:'',i8
     1,'' y:'',i8,'' z:'',i8)')nxs,nys,nzs
      call rmod_card_to_history(crd80)

      return
      end

c23456789012345678901234567890123456789012345678901234567890123456789012
      subroutine rmod_gaussian_grid_0(nxs,nys,nzs,nx_vel,ny_vel,nz_vel
     1,v_grd,v_grd0,mweight,weight)
      implicit  none
      integer   nxs,nys,nzs,nx_vel,ny_vel,nz_vel,mweight
      real      v_grd(nz_vel,nx_vel,ny_vel),v_grd0(nz_vel,nx_vel,ny_vel)
      real      weight(nxs/2+1,nys/2+1,nzs/2+1)

      integer   nxs2,ix,jx,kx,nys2,iy,jy,ky,nzs2,iz,jz,kz
      real      wsum,vsum,e0

      nxs2 = nxs / 2
      nys2 = nys / 2
      nzs2 = nzs / 2

      if (mweight .lt. (nxs2+1)*(nys2+1)*(nzs2+1)) then
        print'('' insufficient work space to copy velocity grid''
     1,/,'' mweight='',i8,'' (nxs2+1)*(nys2+1)*(nzs2+1)='',i8)'
     1,mweight,(nxs2+1)*(nys2+1)*(nzs2+1)
        return
      endif

      e0 = -((nxs2+1)**2 + (nys2+1)**2 + (nzs2+1)**2)
      do ix = 1 , nxs2+1
        do iy = 1 , nys2+1
          do iz = 1 , nzs2+1
            weight(ix,iy,iz) = exp(((ix-1)**2+(iy-1)**2+(iz-1)**2)/e0)
          enddo    ! do iz = 1 , nzs2+1
        enddo    ! do iy = 1 , nys2+1
      enddo    ! do ix = 1 , nxs2+1

c  make a copy of the velocity grid
      call util_copy(nx_vel*ny_vel*nz_vel,v_grd,v_grd0)

      do ix = 1 , nx_vel
        do iy = 1 , ny_vel
          do iz = 1 , nz_vel

            vsum = 0.
            wsum = 0.
            do jx = max(ix-nxs2,1) , min(ix+nxs2,nx_vel)
              kx = iabs(jx-ix) + 1
              do jy = max(iy-nys2,1) , min(iy+nys2,ny_vel)
                ky = iabs(jy-iy) + 1
                do jz = max(iz-nzs2,1) , min(iz+nzs2,nz_vel)
c  weight by a gaussian function
                  kz = iabs(jz-iz) + 1
                  wsum = wsum + weight(kx,ky,kz)
                  vsum = vsum + weight(kx,ky,kz) * v_grd0(jz,jx,jy)
                enddo    ! do jz = max(iz-nzs2,1) , min(iz+nzs2,nz_vel)
              enddo    ! do jy = max(iy-nys2,1) , min(iy+nys2,ny_vel)
            enddo    ! do jx = max(ix-nxs2,1) , min(ix+nxs2,nx_vel)

            if (wsum .ne. 0.) then
              v_grd(iz,ix,iy) = vsum / wsum
            else    !             if (wsum .ne. 0.) then
              v_grd(iz,ix,iy) = v_grd0(iz,ix,iy)
      print*,' iz=',iz,' ix=',ix,' iy=',iy,' v_grd=',v_grd(iz,ix,iy)
            endif    ! if (wsum .ne. 0.) then

          enddo    ! do iz = 1 , nz_vel
        enddo    ! do iy = 1 , ny_vel
      enddo    ! do ix = 1 , nx_vel

      return
      end

c23456789012345678901234567890123456789012345678901234567890123456789012
      subroutine rmod_read_cps_v(h_file,mod_type
     1,cxi,cyi,czi
     1,m_cord,n_cord,x_cord,cord
     1,x_min,x_max
     1,y_min,y_max
     1,z_min,z_max
     1,z_datum
     1,nx_vel,x0_vel,dx_vel
     1,ny_vel,y0_vel,dy_vel
     1,nz_vel,z0_vel,dz_vel
     1,m_vel,vel
     1,m_work,work)

      implicit  none

      character h_file*(*)
      character mod_type*(*)
      character cxi*(*)
      character cyi*(*)
      character czi*(*)
      integer   m_cord,n_cord
      real      x_cord(2,m_cord)
      character cord(m_cord)*16

      real      x_min,x_max,y_min,y_max,z_min,z_max,z_datum

      integer   nx_vel
      real      x0_vel,dx_vel

      integer   ny_vel
      real      y0_vel,dy_vel

      integer   nz_vel
      real      z0_vel,dz_vel

      integer   m_vel
      real      vel(m_vel)

      integer   m_work
      real      work(m_work)

      integer   i_err

      integer   nt_vel
      real      t0_vel,dt_vel

      integer   l_vel,n_vel,nv_vel,ix_vel,iy_vel,iz_vel
      integer   iv_work,it_work,iw_work
      integer   ix_work,iy_work,jx_work,jy_work,n_work
      integer   lu,ihx,ihy,i,i_pass,i_xyz_vel
      real      z1_vel
      real      v_min,v_max
      logical   i_exist
      character project*10,line_name*10,rec_date*5,proj_date*5
      character userid*3,remark*15,v_name*8,v_type*4,vel_file*80
      integer   util_len_r

      print'(
     1 /,'' This option reads a gridded model from a CPS ''
     1,''velocity file''
     1,/,'' Note you may need to edit the transform coordinates ''
     1,/,''as they will take default values''
     1,/,'' Note also, these velocity functions should already''
     1,/,'' be on a uniform x,y,z or x,y,t grid.''
     1)'

      i_err    = 0

    1 continue
      call util_copy_file_name(h_file,vel_file)
      call util_get_file_name(' Enter CPS velocity file name'
     1,vel_file,'vel')
      call rmod_inquire_exist(vel_file,*1)

      call getlun(lu,*1099)
      goto 1098
 1099 continue
      print'(/,'' error in getlun'')'
      goto 999
 1098 continue

c  make 2 passes through the file
c  the first will read the x,y locations and determine the x,y grid size
c  the second will actualy read the velociites
      nt_vel = 1000

      do i_pass = 1 , 3

        if (i_pass .eq. 1) then

          print'('' reading the first function to get ''
     1,''the number of points in each function'')'

        elseif (i_pass .eq. 2) then

          print'('' reading through velocities to get x,y range'')'

        else    ! if (i_pass .eq. 1) then

          print'('' reading in velocities'')'

        endif    ! if (i_pass .eq. 1) then

c  open the velocity function file
        call open_cps_velfile2(lu,vel_file,'READ',l_vel,n_vel
     1,*999,*999,ihx,ihy)

        ix_work = 1
        iy_work = ix_work + n_vel
        it_work = iy_work + n_vel
        iv_work = it_work + nt_vel
        iw_work = iv_work + nt_vel
        n_work  = iw_work + nt_vel

        if (n_work .gt. m_work) then
      print'('' need more memory in rmod_read_velfile have='',i10
     1,'' and need='',i10)',m_work,n_work
          i_err = -1
          goto 999
        endif

c  read the first function to get the number of points in each function
        if (i_pass .eq. 1) then

          call read_cps_velfile(lu,l_vel,v_name
     1,nv_vel,work(ix_work),work(iy_work)
     1,v_type,work(it_work),work(iv_work),*999,*999
     1,project,line_name,rec_date,proj_date,userid,remark,work(iw_work))

          nt_vel = nv_vel

        else    ! if (i_pass .eq. 1) then

c read each velocity function in the file
          do i = 1 , n_vel

            jx_work = ix_work + i - 1
            jy_work = iy_work + i - 1

            call read_cps_velfile(lu,l_vel,v_name
     1,nv_vel,work(jx_work),work(jy_work)
     1,v_type,work(it_work),work(iv_work),*999,*999
     1,project,line_name,rec_date,proj_date,userid,remark,work(iw_work))

c      print*,' x=',work(jx_work),work(jx_work+nv_vel-1)
c     1,' y=',work(jy_work),work(jx_work+nv_vel-1)
c     1,' t=',work(it_work),work(it_work+nv_vel-1)
c     1,' v=',work(iv_work),work(iv_work+nv_vel-1)

            if (i_pass .eq. 2 .and. i .eq. 1) then
              print'('' number of functions='',i10
     1,'' number of columns='',i10)',n_vel,l_vel
              dz_vel = work(it_work+1) - work(it_work)
              z0_vel = work(it_work)
              z1_vel = work(it_work+nv_vel-1)
              nz_vel = int((z1_vel-z0_vel)/dz_vel) + 1
              if ((nz_vel-1)*dz_vel+z0_vel .lt. z1_vel)
     1nz_vel = nz_vel + 1

              if (ihx .eq. 7) then
                cxi = 'XGRID'
              elseif (ihx .eq. 17) then
                cxi = 'XBASEMENT'
              else
                cxi = 'XANNOTATION'
              endif

              if (ihy .eq. 8) then
                cyi = 'XGRID'
              elseif (ihy .eq. 18) then
                cyi = 'YBASEMENT'
              else
                cyi = 'YANNOTATION'
              endif

              if (dz_vel .lt. 1.) then
                czi = 'TIME'
              else    ! if (dz_vel .lt. 1.) then
                czi = 'DEPTH'
              endif    ! if (dz_vel .lt. 1.) then

C23456789012345678901234567890123456789012345678901234567890123456789012
      print'('' found the output vertical grid size ''
     1,/,'' velocity mod_type='',a
     1,/,'' ihx='',i10,'' ihy='',i10
     1,/,'' cxi='',a16,'' cyi='',a16,'' zccord='',a16
     1,/,'' z n='',i10,'' min='',f10.2,'' max='',f12.4
     1,'' inc='',f12.4)'
     1,v_type(1:util_len_r(v_type))
     1,ihx,ihy
     1,cxi(1:util_len_r(cxi))
     1,cyi(1:util_len_r(cyi))
     1,czi(1:util_len_r(czi))
     1,nz_vel,z0_vel,(nz_vel-1)*dz_vel+z0_vel,dz_vel
              print'(''function  name        x          y'')'

            elseif (i_pass .eq. 2 .and. i .eq. 1) then

              print'(''function  name        x          y''
     1,''        ix     iy      n   v_min      v_max'')'

            endif    ! if (i_pass .eq. 2 .and. i .eq. 1) then

c          call util_min_max(v_min,v_max,nv_vel,work(iv_work))
c      print*,' i=',i,' x=',work(jx_work),' y=',work(jy_work)
c     1,' v_min=',v_min,' v_max=',v_max

c  if reading velocities ...
            if (i_pass .eq. 2) then

              if (mod(i,20) .eq. 0)
     1print'(1x,i6,1x,a8,1x,f10.2,1x,f10.2)'
     1,i,v_name,work(jx_work),work(jy_work)

            else    ! if (i_pass .eq. 2) then

              ix_vel = min(nx_vel,max(1
     1,nint((work(jx_work)-x0_vel)/dx_vel)+1))
            iy_vel = min(ny_vel,max(1
     1,nint((work(jy_work)-y0_vel)/dy_vel)+1))
            i_xyz_vel = (iy_vel - 1) * nx_vel * nz_vel
     1                + (ix_vel - 1) * nz_vel + 1

            call util_min_max(v_min,v_max,nv_vel,work(iv_work))

            if (ix_vel .eq. 1 .or. iy_vel .eq.1)
     1print'(1x,i6,1x,a8,1x,f10.2,1x,f10.2
     1,1x,i6,1x,i6,1x,i6,1x,f10.2,1x,f10.2)'
     1,i,v_name,work(jx_work),work(jy_work)
     1,ix_vel,iy_vel,nv_vel,v_min,v_max

c  convert input velocity to slowness
            call util_invert(nv_vel,work(iv_work))

c  interpolate output slowness
            call rmod_interpolate_velocity(
     1nv_vel,work(it_work),work(iv_work)
     1,nz_vel,z0_vel,dz_vel,vel(i_xyz_vel))

          endif    ! if (i_pass .eq. 2) then

        enddo    ! do i = 1 , n_vel

        if (i_pass .eq. 2) then

c  get x grid size
          call rmod_array_size(nx_vel,x0_vel,dx_vel
     1,n_vel,work(ix_work),i_err)

c  get y grid size
            call rmod_array_size(ny_vel,y0_vel,dy_vel
     1,n_vel,work(iy_work),i_err)
            if (i_err .ne. 0) goto 999

          endif    ! if (i_pass .eq. 2) then

        endif    ! if (i_pass .eq. 1) then

        rewind(lu)
        call close_cps_velfile(lu,'READ')

      enddo    ! do i_pass = 1 , 3

c  get min,max velocity
      call util_invert(nx_vel*ny_vel*nz_vel,vel)
      call util_min_max(v_min,v_max,nx_vel*ny_vel*nz_vel,vel)
      call util_invert(nx_vel*ny_vel*nz_vel,vel)

c  set defaults
      call rmod_transform_defaults(cxi,cyi,czi
     1,m_cord,n_cord,x_cord,cord)

      x_min = x0_vel
      x_max = (nx_vel - 1) * dx_vel + x0_vel
      y_min = y0_vel
      y_max = (ny_vel - 1) * dy_vel + y0_vel
      z_min = z0_vel
      z_max = (nz_vel - 1) * dz_vel + z0_vel
      z_datum = 0.
      mod_type = 'GRID'

      return

  999 continue
      call rmod_pause(' error reading cps file',vel_file)
      return
      end

C23456789012345678901234567890123456789012345678901234567890123456789012
      subroutine rmod_interpolate_velocity(nt_inp,t_inp,v_inp
     1,nt_out,t0_out,dt_out,v_out)
      implicit none
      integer  nt_inp,nt_out
      real     t_inp(nt_inp),v_inp(nt_inp),t0_out,dt_out,v_out(nt_out)

      integer  it_inp,it_out,i0,i1,i2
      real     t_out,dt,dv

c      write(88,'(/,'' rmod_interpolate velocity'','' nt_inp='',i8
c     1,/,'' it_inp t  v'')'),nt_inp
c      write(88,'(1x,i5,1x,f8.4,1x,f8.1)')
c     1(it_inp,t_inp(it_inp),1./v_inp(it_inp),it_inp=1,nt_inp)
c      write(88,'('' it_out   i1    i2   t_outt_inp1   t_inp2''
c     1,''         v_out  v_inp1  v_inp2'')')

      do it_out = 1 , nt_out

        t_out = (it_out - 1) * dt_out + t0_out

        if (t_out .le. t_inp(1)) then

          i1 = 1
          i2 = 1

        elseif (t_out .ge. t_inp(nt_inp)) then

          i1 = nt_inp
          i2 = nt_inp

        else    ! if (t_out .le. t_inp(1)) then

          do it_inp = 1 , nt_inp-1
            i1 = it_inp
            i2 = it_inp + 1

            if (t_out .ge. min(t_inp(i1),t_inp(i2))
     1    .and. t_out .le. max(t_inp(i1),t_inp(i2))) goto 1

          enddo    ! do it_inp = 2 , nt_inp

    1     continue

          if (t_inp(i2) .lt. t_inp(i1)) then

            i0 = i1
            i1 = i2
            i2 = i0

          endif    ! if (t_inp(i2) .lt. t_inp(i1) then

        endif    ! if (t_out .le. t_inp(1)) then

        dv = v_inp(i2) - v_inp(i1)
        dt = t_inp(i2) - t_inp(i1)
        if (dt .eq. 0.) dt = 1.

        v_out(it_out) = max(min(v_inp(i1),v_inp(i2))
     1                 ,min(max(v_inp(i1),v_inp(i2)),
     1v_inp(i1) + (t_out - t_inp(i1)) * dv / dt))

c      write(88,'(1x,i5,1x,i5,1x,i5,1x,f8.4,1x,f8.4,1x,f8.4
c     1,1x,f8.1,1x,f8.1,1x,f8.1)')
c     1it_out,i1,i2,t_out,t_inp(i1),t_inp(i2)
c     1,1./v_out(it_out),1./v_inp(i1),1./v_inp(i2)

      enddo    ! do it_out = 1 , nt_out

      return
      end

c23456789012345678901234567890123456789012345678901234567890123456789012
      subroutine rmod_write_cps_v(h_file,mod_type,cx0,cy0,cz0
     1,nx,x0,dx,ny,y0,dy,nz,z0,dz,v,m_work,work)

      implicit  none
      integer   nx,ny,nz,m_work
      real      x0,dx,y0,dy,z0,dz,v(nz,nx,ny),work(m_work)
      character cx0*(*),cy0*(*),cz0*(*),h_file*(*),mod_type*(*)

      real      x,y,z
      integer   head_x,head_y,i_file,ix,iy,i_err
      integer   nx_o,ny_o,nz_o,nxy,nvpp
      character cx*16,cy*16,cz*16
      character file*80,v_name*8,vmod_type*4,project*10,line*10
      character r_date*5,p_date*5,id*3,comment*15,ans*1,bufd*9
      character vmod_type0*4
      data      i_file/1/

      print'(/,'' This option writes the current gridded velocity''
     1,'' model as a set of CPS velocity functions'')'
      if (mod_type(1:1) .ne. 'G') then
        print'('' Model mod_type must be grid or G3DL for this option''
     1,'' mod_type='',a16)',mod_type
        call rmod_pause(' ',' ')
        return
      endif

c  cps velocity functions do not like more than 99 points
      if (nz .gt. 99) then
        print'('' the number of depth points should be less than 99''
     1,'' current value='',i8
     1,/,'' use option 12 to interpolate to a new grid with nz=99,''
     1,'' dz='',f10.2)',nz,((nz-1)*dz) / 98
        return
      endif    ! if (nz .gt. 99) then

c  determine the x header word
      call rmodcaps(cx0,cx)
      if (cx .eq. 'XANNOTATION') then
        head_x = 37
      elseif (cx .eq. 'XGRID') then
        head_x = 7
      else
        head_x = 17
      endif

c  determine the y header word
      call rmodcaps(cy0,cy)
      if (cy .eq. 'YANNOTATION') then
        head_y = 38
      elseif (cy .eq. 'YGRID') then
        head_y = 8
      else
        head_y = 18
      endif

      call rmodcaps(cz0,cz)
      if (cz .eq. 'TIME') then
        vmod_type = 'VTIN'
      else
        vmod_type = 'VZIN'
      endif

      project = h_file(1:10)
      id      = 'ID'
      comment = 'xxx'
      call date(bufd)
      r_date  = bufd(1:5)
      p_date  = bufd(6:9)

      print'('' cx='',a16,'' cy='',a16,'' cz='',a16
     1,/,'' head_x='',i8,'' head_y='',i8,'' vmod_type='',a4)'
     1,cx,cy,cz,head_x,head_y,vmod_type

      call util_copy_file_name(h_file,file)
      call util_get_file_name(' Enter the velocity filename'
     1,file,'vel')
      vmod_type0 = vmod_type
      print'(/,'' enter velocity mod_type default='',a4)',vmod_type
      vmod_type0 = ' '
      read(*,'(a)',end=1)vmod_type0
      if (vmod_type0 .ne. ' ') vmod_type = vmod_type0
    1 continue

      call rmod_ans(ans,'N','Y','N',' Add PHASE SHIFT reference?')

      nxy = nx * ny
      nvpp = 2

c  add reference function (replaces first function)
      if (ans .eq. 'Y') then
        nxy = nxy + nx
        call rmod_average_vz(mod_type,nx,ny,nz,v
     1,nx_o,ny_o,nz_o,v(1,nx+1,ny))
      endif    ! if (ans .eq. 'Y') then

      call open_cps_velfile2(i_file,file,'WRITE',nvpp,nxy,*999,*999
     1,head_x,head_y)

c  write functions
      call util_invert(nx*ny*nz,v)
      call util_line(nz,work,z0,dz)

      if (ans .eq. 'Y') then
        call util_invert(nz,v(1,nx+1,ny))
        iy = 1
        y = (iy - 1) * dy + y0
        y = y - dy
        do ix = 1 , nx
          x = (ix - 1) * dx + x0
          if (ix .eq. 1) then
            v_name = 'PHASE'
          write(line,'(i6)')min(999999,max(-99999,nint(y)))
          call write_cps_velfile (i_file,nvpp,v_name,nz,x,y,vmod_type
     1,work,v(1,nx+1,ny),project,line,r_date,p_date,id,comment,work)
          else
            v_name = 'V'
            call rmod_v_name(v_name,2,4,iy)
            call rmod_v_name(v_name,5,7,ix)
          write(line,'(i6)')min(999999,max(-99999,nint(y)))
          call write_cps_velfile (i_file,nvpp,v_name,nz,x,y,vmod_type
     1,work,v(1,ix,iy),project,line,r_date,p_date,id,comment,work)
          endif

        enddo    ! do ix = 1 , nx
      endif    ! if (ans .eq. 'Y') then

      do iy = 1 , ny
        y = (iy - 1) * dy + y0
        do ix = 1 , nx
          x = (ix - 1) * dx + x0
          v_name = 'V'
          call rmod_v_name(v_name,2,4,iy)
          call rmod_v_name(v_name,5,7,ix)

          write(line,'(i6)')min(999999,max(-99999,nint(y)))
          call write_cps_velfile (i_file,nvpp,v_name,nz,x,y,vmod_type
     1,work,v(1,ix,iy),project,line,r_date,p_date,id,comment,work)

        enddo    ! do ix = 1 , nx
      enddo    ! do iy = 1 , ny
      call close_cps_velfile(i_file,i_err)
      call util_invert(nx*ny*nz,v)
      if (ans .eq. 'Y') call util_copy(nz,work(nz+1),v)

      return
  999 continue
      call rmod_pause(' error reading cps file',file)
      return
      end

c23456789012345678901234567890123456789012345678901234567890123456789012
      subroutine rmod_v_name(v_name,i1,i2,i)
      implicit none
      integer  i1,i2,i
      character v_name*(*)
      integer j
      do j = i1 , i2
      v_name(j:j) = '0'
      enddo    ! j = i1 , i2
      if     (i .le. 9  ) then
        write(v_name(i2-0:i2),'(i1)')i
      elseif (i .le. 99 ) then
        write(v_name(i2-1:i2),'(i2)')i
      elseif (i .le. 999) then
        write(v_name(i2-2:i2),'(i3)')i
      elseif (i .le. 9999) then
        write(v_name(i2-3:i2),'(i4)')i
      elseif (i .le. 99999) then
        write(v_name(i2-4:i2),'(i5)')i
      endif
      return
      end

c23456789012345678901234567890123456789012345678901234567890123456789012
      subroutine rmod_3dv_to_2dv(nv,imv,itv,ixv,nxv,xv,yv,zv,vel
     1,m_work,work
     1,nxh,xhmin,xhinc
     1,nyh,yhmin,yhinc
     1,nxyh,xyhmin,xyhinc
     1,nxyv
     1,xl_end,xr_end)
      implicit none
      integer  m_work,nv
      integer  nxh,nyh,nxyh,nxyv
      integer imv(1),itv(1),ixv(1),nxv(1)
      real    xhmin,xhinc,yhmin,yhinc,xyhmin,xyhinc
      real    xv(1),yv(1),zv(1),vel(1),work(1)
      integer i,jimv,jitv,jixv,jnxv,jxv,jyv,jzv,jvel
      integer jiv_h,jnv_h,jiz_h,jnz_h,jiy_h,jny_h
      integer i_work,n_work,i_err,nv_i
      integer   i_work_i,i_work_n
      real    xl_end,xr_end
      call util_wors(i_work_i,i_work_n,m_work)
      call util_work(i_work_i,i_work_n,jimv,nv)
      call util_work(i_work_i,i_work_n,jitv,nv)
      call util_work(i_work_i,i_work_n,jixv,nv)
      call util_work(i_work_i,i_work_n,jnxv,nv)
      call util_work(i_work_i,i_work_n,jxv,nv)
      call util_work(i_work_i,i_work_n,jyv,nv)
      call util_work(i_work_i,i_work_n,jzv,nv)
      call util_work(i_work_i,i_work_n,jvel,nv)
      call util_work(i_work_i,i_work_n,jiv_h,nv)
      call util_work(i_work_i,i_work_n,jnv_h,nv)
      call util_work(i_work_i,i_work_n,jiz_h,nv)
      call util_work(i_work_i,i_work_n,jnz_h,nv)
      call util_work(i_work_i,i_work_n,jiy_h,nv)
      call util_work(i_work_i,i_work_n,jny_h,nv)
      call util_worl(i_work_i,i_work_n,n_work)
      call util_work(i_work_i,i_work_n,i_work,n_work)
      call util_worc(i_work_i,i_work_n,i_err)
      if (i_err .ne. 0) goto 999

c  copy to temporary location
      nv_i = nv
      call util_copy_n(8,nv_i
     1,imv,work(jimv),itv,work(jitv),ixv,work(jixv),nxv,work(jnxv)
     1,xv,work(jxv),yv,work(jyv),zv,work(jzv),vel,work(jvel))

      call rmod_3dv_to_2dv_v(
     1 xl_end,xr_end
     1,nv_i,work(jimv),work(jitv),work(jixv),work(jnxv)
     1,work(jxv),work(jyv),work(jzv),work(jvel)
     1,nv,imv,itv,ixv,nxv,xv,yv,zv,vel
     1,work(jiv_h),work(jnv_h),work(jiz_h),work(jnz_h)
     1,work(jiy_h),work(jny_h)
     1,nxh,xhmin,xhinc,nyh,yhmin,yhinc,nxyh,xyhmin,xyhinc,nxyv
     1,n_work,work(i_work))

c      call rmod_pause(' end of rmod_3dv_to_2dv',' ')

      return
  999 continue
      call rmod_pause(' error in rmod_3dv_to_2dv',' ')
      return

      end

c23456789012345678901234567890123456789012345678901234567890123456789012
      subroutine rmod_3dv_to_2dv_v(
     1 xl_end,xr_end
     1,nv_i,imv_i,itv_i,ixv_i,nxv_i,xv_i,yv_i,zv_i,vel_i
     1,nv_o,imv_o,itv_o,ixv_o,nxv_o,xv_o,yv_o,zv_o,vel_o
     1,iv_h,nv_h,iz_h,nz_h,iy_h,ny_h
     1,nxh,xhmin,xhinc,nyh,yhmin,yhinc,nxyh,xyhmin,xyhinc,nxyv
     1,m_work,work)
      implicit none
      real     xl_end,xr_end,dx_end
      integer  m_work,nv_i,nv_o
      integer  nxh,nyh,nxyh,nxyv
      integer iv_h(1),nv_h(1),iz_h(1),nz_h(1),iy_h(1),ny_h(1)
      integer imv_i(1),itv_i(1),ixv_i(1),nxv_i(1)
      real    xv_i(1),yv_i(1),zv_i(1),vel_i(1)
      integer imv_o(1),itv_o(1),ixv_o(1),nxv_o(1)
      real    xv_o(1),yv_o(1),zv_o(1),vel_o(1)
      real    xhmin,xhinc,yhmin,yhinc,xyhmin,xyhinc,work(1)
      real    xyhmax

      integer jv_h,jv_1,jz_h,jz_1,mv_h,mz_h,my_h,iv_o,ny_min,ny_max
      integer i,i_err,nx
      real    util_invert_1
      real    dx,dy,xhmax,yhmax,dxv,xv,yv
      character ans*1,crd80*80

c      print'('' nxyh='',i6,'' xyhmin='',f8.0,'' xyhinc='',f8.2
c     1,/,'' xhmin='',f8.0,'' xhinc='',f8.2
c     1,/,'' yhmin='',f8.0,'' yhinc='',f8.2)'
c     1,nxyh,xyhmin,xyhinc,xhmin,xhinc,yhmin,yhinc

      ans = 'N'
      call rmod_ans(ans,ans,'Y','N',' ask for velocity point spacing?')

c  determine the number of velocity types
      call gtol_vel_hor_size(mv_h,iv_h,nv_h,nv_i,itv_i)

c      print'('' mv_h='',i5)',mv_h
      iv_o = 0
      nv_o = 0
      do jv_h = 1 , mv_h
        jv_1 = iv_h(jv_h) + 1

c  determine the number of z planes for each velocity mod_type
        call gtol_vel_hor_size(mz_h,iz_h,nz_h,nv_h(jv_h),ixv_i(jv_1))
c      print'('' jv_h='',i5,'' mz_h='',i5)',jv_h,mz_h
c  for each z plane extract a velocity line
        do jz_h = 1 , mz_h
          jz_1 = jv_1 + iz_h(jz_h)
          nv_o = nv_o + 1
          imv_o(nv_o) = itv_i(jz_1)
          ixv_o(nv_o) = iv_o
          nxv_o(nv_o) = 0

c      print'('' jz_h='',i5,'' nz_h='',i5,'' jz_1='',i5
c     1,'' vel='',f10.2)'
c     1,jz_h,nz_h(jz_h),jz_1,vel_i(jz_1)

c  intialize an array to zero for extracting z values
          call util_setr(nz_h(jz_h),work,0.)

c  convert the input to slowness
          call util_invert(nz_h(jz_h),vel_i(jz_1))

c  determine the maximum number of x points in each y line
          call gtol_vel_hor_size(my_h,iy_h,ny_h,nz_h(jz_h),nxv_i(jz_1))
          call util_min_max(ny_min,ny_max,my_h,ny_h)

c  set the number of velocity control points to extract
c          nx = min(nxyh,max(2,nz_h(jz_h),max(my_h,ny_max)))
c          dx = (nxyh-1)*xhinc / max(1,nx-1)
c          dy = (nxyh-1)*yhinc / max(1,nx-1)
c          dx = (nxyh/nx)*xhinc
c          dy = (nxyh/nx)*yhinc

          xhmax = (nxyh - 1) * xhinc + xhmin
          yhmax = (nxyh - 1) * yhinc + yhmin
          nx = (nxyh - 1) / 4 + 1
          dx = (xhmax - xhmin) / max(1,nx-1)
          dy = (yhmax - yhmin) / max(1,nx-1)

          if ((nx-1)*dx .lt. (nxyh-1)*xhinc-1.e-3
     1   .or. (nx-1)*dy .lt. (nxyh-1)*yhinc-1.e-3) nx = nx + 1

c      print'('' my_h='',i5,'' nz_h='',i5,'' nh='',i5,'' nx='',i5
c     1,'' hx='',f10.2,'' dx='',f10.2)'
c     1,my_h,nz_h,nxyh,nx,xhinc,dx

          if (my_h .eq. 1 .and. ny_max .eq. 1) then

            nx = 1

          elseif (ans .eq. 'Y') then

    1 continue
            print'('' velocity horizon '',i5,'' value='',i5
     1,/    ,'' depth    horizon='',i5,'' value='',i5)'
     1,jv_h,imv_i(jv_h),jz_h,itv_i(jz_1)

            print'(/,'' enter the number of x,y velocity nodes''
     1,'' default='',i8)',nx
            crd80 = ' '
            read(*,'(a)',err=1,end=2)crd80
            if (crd80 .ne. ' ') read(crd80,*,err=1,end=2)nx
    2 continue

          endif    ! if (ny_max .eq. 1) then

          xyhmax = (nxyh - 1) * xyhinc + xyhmin
          dxv = (xyhmax - xyhmin) / max(1,nx-1)
          dx = (xhmax - xhmin) / max(1,nx-1)
          dy = (yhmax - yhmin) / max(1,nx-1)
          dx_end = (xr_end - xl_end) / max(1,nx-1)
c      print*,' nx=',nx,' xhmin=',xhmin,' dx=',dx
c     1,' yhmin=',yhmin,' dy=',dx

        if (((nx-1)*dx .lt. (nxyh-1)*xhinc-1.e-3
     1  .or. (nx-1)*dy .lt. (nxyh-1)*yhinc-1.e-3)
     1 .and. (my_h .gt. 1. or. ny_max .gt. 1)) nx = nx + 1

c  for each output velocity control point
          do i = 1 , nx
            iv_o = iv_o + 1
            nxv_o(nv_o) = nxv_o(nv_o) + 1
            itv_o(iv_o) = ixv_i(jz_1)

c  set the x,y location
      if (nx .eq. 1) then
            xv = (xhmin + xhmax) / 2.
            yv = (yhmin + yhmax) / 2.
      else
            xv = (i - 1) * dx + xhmin
            yv = (i - 1) * dy + yhmin
      endif
c      xv_o(iv_o) = (i - 1) * dxv + xyhmin
      xv_o(iv_o) = (i - 1) * dx_end + xl_end
      yv_o(iv_o) = 0.

c  get the z value at this x,y location
      call fitcell6(nz_h(jz_h),itv_i(jz_1),ixv_i(jz_1),nxv_i(jz_1)
     1,xv_i(jz_1),yv_i(jz_1),work,zv_i(jz_1),itv_i(jz_1)
     1,1,xv,1.,1,yv,1.,1,0.,1.,zv_o(iv_o)
     1,m_work-nz_h(jz_h),work(nz_h(jz_h)+1),i_err)

c  get the v value at this x,y location
      call fitcell6(nz_h(jz_h),itv_i(jz_1),ixv_i(jz_1),nxv_i(jz_1)
     1,xv_i(jz_1),yv_i(jz_1),zv_i(jz_1),vel_i(jz_1),itv_i(jz_1)
     1,1,xv,1.,1,yv,1.,1,zv_o(iv_o),1.,vel_o(iv_o)
     1,m_work-nz_h(jz_h),work(nz_h(jz_h)+1),i_err)

c      print'('' i='',i5,'' iv='',i5,'' x='',f8.0,'' y='',f8.0
c     1,'' z='',f8.0,'' v='',f8.0)'
c     1,i,iv_o,xv,yv,zv_o(iv_o),util_invert_1(vel_o(iv_o))

c  convert the output back to velocity
            call util_invert(1,vel_o(iv_o))

          enddo     ! do i = 1 , nxyh

c  convert the input back to velocity
          call util_invert(nz_h(jz_h),vel_i(jz_1))

        enddo    ! do jz_h = 1 , mz_h

      enddo    ! do jv_h = 1 , mv_h

      return
      end

cqqq
c23456789012345678901234567890123456789012345678901234567890123456789012
      subroutine rmod_ascii_time_to_depth(h_file,mod_type,cxi,cyi,czi
     1,z_datum
     1,nx_vel,x0_vel,dx_vel
     1,ny_vel,y0_vel,dy_vel
     1,nz_vel,z0_vel,dz_vel
     1,vel)

c  i_dir = 1 depth to time
c  i_dir = -1 time to depth

      implicit none
      integer  util_len_r

      character h_file*(*),mod_type*(*),cxi*(*),cyi*(*),czi*(*)

      real     z_datum

      integer  nx_vel
      real     x0_vel,dx_vel

      integer  ny_vel
      real     y0_vel,dy_vel

      integer  nz_vel
      real     z0_vel,dz_vel

      real     vel(nz_vel,nx_vel,ny_vel)

      integer   m_column
      parameter (m_column=20)
      integer   n_column
      real      data(m_column)
      integer   ix_column,iy_column,iz_column,iv_column
      character cz_ascii*16

      integer   m_data,n_data
      parameter (m_data=1000)
      real      x_data(m_data)
      real      y_data(m_data)
      real      z_data(m_data)
      real      v_data(m_data)
      real      w_data(m_data)

      real      scale_inp,scale_out,zt_inp

      character file*80,v_name*8,vmod_type*4,project*10,line*10
      character r_date*5,p_date*5,id*3,comment*15,ans*1,bufd*9

      integer   i_file_inp
      character file_inp*80

      integer   i_file_out
      character file_out*80

      integer   i_file_vel
      character file_vel*80

      integer   nvpp,nxy,head_x,head_y

      character crd132*132,BAD_FORMAT*1

      integer   i_dir,i_err,n_card,j,i_f
      integer   i_pass,ic,ic1,ic2,nc

      vmod_type = 'VZIN'
      print'(/,'' This option converts and ascii file''
     1,'' from time to depth''
     1,/,'' using the current gridded velocity model''
     1,/,'' Note the X,Y compnents of the ascii file should''
     1,/,'' be in the same units as the velocity model''
     1,/,'' cxi='',a16,'' cyi='',a16,'' czi='',a15)'
     1,cxi,cyi,czi

      if (mod_type .ne. 'GRID' .or. czi .ne. 'DEPTH') then
        print'('' Model mod_type must be grid and depth coordinate''
     1,'' must be DEPTH for this option''
     1,'' mod_type= '',a16,'' czi='',a16)',mod_type,czi
        call rmod_pause(' ',' ')
        goto 999
      endif

    1 continue
      call util_copy_file_name(h_file,file_inp)
      call util_get_file_name(' Enter the input ascii file name'
     1,file_inp,'xyz')
      call rmod_inquire_exist(file_inp,*1)
      call rmodopen(i_file_inp,file_inp,'FORMATTED','OLD','SEQUENTIAL'
     1,'IEEE',0,i_err)
      if (i_err .ne. 0) goto 999

      call util_copy_file_name(file_inp,file_out)
      call util_get_file_name(' Enter the output ascii file name'
     1,file_out,'xyz')
      call rmodopen(i_file_out,file_out,'FORMATTED','NEW','SEQUENTIAL'
     1,'IEEE',0,i_err)
      if (i_err .ne. 0) goto 999

      call util_copy_file_name(file_inp,file_vel)
      call util_get_file_name(' Enter the output ascii file name'
     1,file_vel,'vel')
      head_x = 7
      head_y = 8
      nvpp = 2
      nxy = nx_vel * ny_vel
      project = h_file(1:10)
      id      = 'ID'
      comment = 'xxx'
      call date(bufd)
      r_date  = bufd(1:5)
      p_date  = bufd(6:9)
      n_data = 0
      i_pass = 1
      call open_cps_velfile2(i_file_vel,file_vel
     1,'WRITE',nvpp,nxy,*999,*999,head_x,head_y)

c  get the number of columns
      call rmod_get_columns(i_file_inp,n_column
     1,ix_column,iy_column,iz_column,iv_column)
      n_column = max(ix_column,iy_column,iz_column,iv_column)
c  check input depth coordinates
      call rmod_ans(cz_ascii,'TIME','TIME','DEPTH'
     1,' is the input data in TIME or DEPTH')

      call rmodcaps(cz_ascii,cz_ascii)
      if (cz_ascii .eq. 'DEPTH') then

        i_dir = 1
        print'('' converting from depth to time'')'
        bad_format = 'N'
        scale_inp = 1.
        scale_out = 1.
      else

        i_dir = -1
        print'('' converting from time to depth'')'
        bad_format = 'Y'
        scale_inp = 1.
        scale_out = 1.

      endif

        bad_format = 'N'
c      call rmod_ans(bad_format,bad_format,'Y','N'
c     1,' is this a bad format file')
      call rmodcaps(bad_format,bad_format)
      print'(/,'' the data in the z column will be scaled''
     1,/,'' by the in and out scale factors'')'

      call rmod_read_float(scale_inp,scale_inp
     1,' enter input  scale factor IN = IN*scale before conversion')

      call rmod_read_float(scale_out,scale_out
     1,' enter output scale factor OUT = OUT*scale after  conversion')

      n_card = 0

    3 continue

        call util_setr(n_column,data,0.)
        read(i_file_inp,'(a)',err=999,end=4)crd132
        call rmodfchr(i_f,'F',1,util_len_r(crd132),crd132)

        if (bad_format .eq. 'Y') then

          read(crd132( 1:9),'(f9.2)')data(1)
          read(crd132(10: ),*,end=5)(data(j),j=2,n_column)

        else    ! if (bad_format .eq. 'Y') then

          read(crd132,*,err=1001,end=5)(data(j),j=1,n_column)
          goto 1002

 1001     continue
          read(crd132,'(10x,f10.2,f10.2,f11.2,f11.2,f11.4)'
     1,err=999,end=5)(data(j),j=1,n_column)

 1002     continue

        endif    ! if (bad_format .eq. 'Y') then

        n_card = n_card + 1

c     1print'('' n='',i10,'' c='',a)'
c     1,n_card,crd132(1:util_len_r(crd132))
c      print'('' top '',6(1x,g16.9))',(data(j),j=1,min(5,n_column))

    5 continue

        data(iz_column) = data(iz_column) * scale_inp
        zt_inp = data(iz_column)
c      print'('' bef convert x='',g16.9,'' y='',g16.9,'' z='',g16.9)'
c     1,data(ix_column),data(iy_column),data(iz_column)
        call ztot_time_to_depth(i_dir
     1,1,data(ix_column),data(iy_column),data(iz_column),data(iz_column)
     1,z_datum
     1,nx_vel,x0_vel,dx_vel
     1,ny_vel,y0_vel,dy_vel
     1,nz_vel,z0_vel,dz_vel
     1,vel)

      if (cz_ascii .eq. 'DEPTH') then

        if (n_card.le.20.or.mod(n_card-1,1000) .eq. 0)
     1print'('' n='',i10,'' x='',f10.2,'' y='',f10.2
     1,'' z inp='',g12.5,'' t out='',g12.5)'
     1,n_card,data(ix_column),data(iy_column),zt_inp,data(iz_column)

      else

        if (n_card.le.20.or.mod(n_card-1,1000) .eq. 0)
     1print'('' n='',i10,'' x='',f10.2,'' y='',f10.2
     1,'' t inp='',g12.5,'' z out='',g12.5)'
     1,n_card,data(ix_column),data(iy_column),zt_inp,data(iz_column)

      endif

c      print'('' aft convert x='',g16.9,'' y='',g16.9,'' z='',g16.9)'
c     1,data(ix_column),data(iy_column),data(iz_column)
        data(iz_column) = data(iz_column) * scale_out

c  read the ascii file
        if (n_column .eq. 4) then
        if (n_data .eq. 0) then

        else    ! if (n_data .eq. 0) then

! new column write out the current column
          if (x_data(n_data) .ne. data(ix_column)
     1   .or. y_data(n_data) .ne. data(iy_column) ) then

          v_name = 'V'
          call rmod_v_name(v_name,2,7,i_pass)
          write(line,'(i6)')
     1min(999999,max(-99999,nint(x_data(n_data))))
          call write_cps_velfile (i_file_vel,nvpp,v_name
     1,n_data,x_data(n_data),y_data(n_data),vmod_type
     1,z_data,v_data,project,line,r_date,p_date,id,comment,w_data)
            n_data = 0
            i_pass = i_pass + 1

          endif    ! if (x_data(n_data) .ne. data(ix_column)

        endif    ! if (n_data .eq. 0) then

        n_data = n_data + 1
        x_data(n_data) = data(ix_column)
        y_data(n_data) = data(iy_column)
        z_data(n_data) = data(iz_column)
        v_data(n_data) = data(iv_column)

        end if    ! if (n_column .eq. 4) then

c  write this row to the next card image
      call rmodwval(i_file_out,n_column,1,data)
c        ic1 = 11
c        crd132 = ' '
c        do ic = 1 , n_column
c          if (ic .eq. 3 .or. ic .eq. 4) ic1 = ic1 + 1
c          ic2 = ic1 + 9
c          write(crd132(ic1:ic2),'(f10.2)')data(ic)
c          ic1 = ic2 + 1
c        enddo
c        ic1 = ic1 + 1
c        if (i_f .le. 0) write(crd132(ic1:ic1),'(a)')'F'
c        write(i_file_out,'(a)')crd132(1:util_len_r(crd132))

c      print'('' bot '',5(1x,g16.9))',(data(j),j=1,min(5,n_column))

      goto 3
    4 continue
      print'('' number of cards read='',i10)',n_card

! write the last file
        if (n_column .eq. 4) then
          v_name = 'V'
          call rmod_v_name(v_name,2,7,i_pass)
          write(line,'(i6)')
     1min(999999,max(-99999,nint(x_data(n_data))))
          call write_cps_velfile (i_file_vel,nvpp,v_name
     1,n_data,x_data(n_data),y_data(n_data),vmod_type
     1,z_data,v_data,project,line,r_date,p_date,id,comment,w_data)
        end if    ! if (n_column .eq. 4) then

      call rmodclos(i_file_inp)
      call rmodclos(i_file_out)
        if (n_column .eq. 4) then
      call close_cps_velfile(i_file_vel,i_err)
        end if    ! if (n_column .eq. 4) then
      if (i_err .ne. 0) goto 999

      return
  999 continue
      call rmod_pause(' error in rmod_ascii_time_to_depth',' ')
      return

      end

c23456789012345678901234567890123456789012345678901234567890123456789012
      subroutine rmod_write_velocity_cards(h_file
     1,nx_vel,x0_vel,dx_vel
     1,ny_vel,y0_vel,dy_vel
     1,nz_vel,z0_vel,dz_vel)

      implicit  none
      character h_file*(*)
      integer   nx_vel
      real      x0_vel,dx_vel
      integer   ny_vel
      real      y0_vel,dy_vel
      integer   nz_vel
      real      z0_vel,dz_vel

      integer   nx,ix
      real      x0,dx,x,x1
      integer   ny,iy
      real      y0,dy,y,y1
      integer   nz,iz,jz
      real      z0,dz,z,z1
      integer   iv
      real      v0,vx,vy,vz,v
      integer   i_file,i_pass,i_err
      character file*80
      character crd80*80

      print'(/,'' this will create a set of layered velocity horzion''
     1,'' cards on a rectangular grid''
     1,/,'' v = v0 + (x - x0) * vx + (y - y0) * vy + (z - z0) * vz'')'
      i_pass = 0
      nx = nx_vel
      x0 = x0_vel
      dx = dx_vel
      ny = ny_vel
      y0 = y0_vel
      dy = dy_vel
      nz = nz_vel
      z0 = z0_vel
      dz = dz_vel

      nx = 31
      x0 = 32800.
      dx = 1640.
      ny = 15
      y0 = 93890.
      dy = 3280.
      nz = 21
      z0 = 0.
      dz = 1000.
      iv = 1
      v0 = 5000
      vx = 0.
      vy = 0.
      vz = .25

      call util_copy_file_name(h_file,file)
      call util_get_file_name(' Enter output velocity crad file name'
     1,file,'vel')

      call rmodopen(i_file,file,'FORMATTED','NEW','SEQUENTIAL'
     1,'IEEE',0,i_err)
      if (i_err .ne. 0) return

    1 continue
      i_pass = i_pass + 1
      print'(/,'' i_pass='',i5)',i_pass

      if (i_pass .eq. 2) then
        nz = 4
        z0 = 22000
        dz = 2000
      elseif (i_pass .eq. 3) then
        nz = 4
        z0 = 31000
        dz = 3000
      elseif (i_pass .ge. 4) then
        nx = -1
      endif

 2001 continue
      crd80 = ' '
      print'(/,'' enter nx,x0,dx defaults=''
     1,i10,1x,f10.2,1x,f10.2)',nx,x0,dx
      print'(/,'' enter -1 to end'')'
      read(*,'(a)',err=2001,end=2002)crd80
      if (crd80 .ne. ' ') read(crd80,*,err=2001,end=2002)nx,x0,dx
 2002 continue
      if (nx .le. 0) then
        call rmodclos(i_file)
c        close(i_file)
        return
      endif

 3001 continue
      crd80 = ' '
      print'(/,'' enter ny,y0,dy defaults=''
     1,i10,1x,f10.2,1x,f10.2)',ny,y0,dy
      read(*,'(a)',err=3001,end=3002)crd80
      if (crd80 .ne. ' ') read(crd80,*,err=2001,end=2002)ny,y0,dy
 3002 continue

 4001 continue
      crd80 = ' '
      print'(/,'' enter nz,z0,dz defaults=''
     1,i10,1x,f10.2,1x,f10.2)',nz,z0,dz
      read(*,'(a)',err=4001,end=4002)crd80
      if (crd80 .ne. ' ') read(crd80,*,err=2001,end=2002)nz,z0,dz
 4002 continue

 5001 continue
      crd80 = ' '
      print'(/,'' enter iv,v0,vx,vy,vz defaults=''
     1,1x,i5,f10.4,1x,f10.4,1x,f10.4,1x,f10.4)',iv,v0,vx,vy,vz

      read(*,'(a)',err=5001,end=5002)crd80
      if (crd80 .ne. ' ') read(crd80,*,err=5001,end=5002)
     1iv,v0,vx,vy,vz
 5002 continue

      if (i_pass .eq. 1) then
        x1 = x0
        y1 = y0
        z1 = z0
      endif

      do iz = 1 , nz
        jz = jz + 1
        z = (iz - 1) * dz + z0
        do iy = 1 , ny
          y = (iy - 1) * dy + y0
          do ix = 1 , nx
            x = (ix - 1) * dx + x0
            v = v0 + (x - x1) * vx + (y - y1) * vy + (z - z1) * vz
      write(i_file,'(1x,f10.2,1x,f10.4,1x,i8,1x,i8
     1,1x,f10.2,1x,f10.2,1x,i8)')x,z,iv,jz,v,y,iy
          enddo    ! do ix = 1 , nx
        enddo    ! do iy = 1 , ny
       print'('' iz='',i10,'' jz='',i10,'' z='',f10.2
     1,'' v='',f10.2)',iz,jz,z,v
      enddo    ! do iz = 1 , nz

      goto 1

      end

c23456789012345678901234567890123456789012345678901234567890123456789012
      subroutine rmod_replace_grid(h_file,mod_type
     1,cxi,cyi,czi,m_cord,n_cord,x_cord,cord
     1,x_min,x_max,y_min,y_max,z_min,z_max,z_datum
     1,nxv,iv1,iv2,iv3,xv,yv,zv,vel
     1,ncv,icv,xcv,ycv,zcv
     1,mv_grd
     1,nx_vel,x0_vel,dx_vel
     1,ny_vel,y0_vel,dy_vel
     1,nz_vel,z0_vel,dz_vel
     1,v_grd,zl
     1,m_work,work)
      implicit none
      character h_file*(*),mod_type*(*),crd80*80
      character cxi*(*),cyi*(*),czi*(*)
      character cord(*)*(*)
      character cxv*16,cyv*16,czv*16,omod_type*16,h_file0*80
      integer   m_cord,n_cord
      real      x_cord
      real      x_min,x_max
      real      y_min,y_max
      real      z_min,z_max
      real      z_datum
      integer   mv_grd
      integer   nx_vel
      real      x0_vel,dx_vel
      integer   ny_vel
      real      y0_vel,dy_vel
      integer   nz_vel
      real      z0_vel,dz_vel
      integer   nx_lay
      real      x0_lay,dx_lay
      integer   ny_lay
      real      y0_lay,dy_lay
      integer   nz_lay
      real      v_grd(1),zl(1)
      integer   nxv,iv1,iv2,iv3,ncv,icv
      real      xv,yv,zv,vel,xcv,ycv,zcv

      integer   m_work
      real      work(1)

      integer   ixg,iyg
      real      xg,yg

      integer   lh,lf,ndim,lu,i_err

      if (mod_type .ne. 'G3DL') then
        print'('' Model mod_type must be G3DL mod_type='',a16)',mod_type
        call rmod_pause(' ',' ')
        return
      endif

      print'(/,'' This option replaces node values in a ''
     1,''3d gridded model.''
     1,/,'' It may replace some or all nodes depending upon ''
     1,/,'' what cell pointers are in the layered file'')'
      nx_lay   = nx_vel
      x0_lay = x0_vel
      dx_lay = dx_vel
      ny_lay   = ny_vel
      y0_lay = y0_vel
      dy_lay = dy_vel
      nz_lay   = nz_vel
      call util_copy(nz_lay*nx_lay*ny_lay,v_grd,zl)

        ndim = 3
        call util_copy_file_name(h_file,h_file0)
        call util_get_file_name(' Enter 3d gridded model'
     1,h_file0,'hgrid')
        crd80 = ' '
        write(crd80,'(a)')
     1' RMOD replacing 3D gridded values from a layered model'
        call rmod_title_to_history(crd80)
        call rmodlenr(lh,h_file0)
        crd80 = ' '
        write(crd80,'('' file:'',a)')h_file0(1:lf)
        call rmod_card_to_history(crd80)
c  read the gridded velocity - note z_lay is used for gridded vleocity
        lu = -1
        call rmodrgrd(h_file0,omod_type
     1,cxv,cyv,czv,cxi,cyi,czi,m_cord,n_cord,x_cord,cord
     1,x_min,x_max,y_min,y_max,z_min,z_max,z_datum
     1,mv_grd
     1,nx_vel,x0_vel,dx_vel
     1,ny_vel,y0_vel,dy_vel
     1,nz_vel,z0_vel,dz_vel
     1,v_grd,m_work,work,lu
     1,i_err)
      if (i_err .ne. 0) goto 999

        if (omod_type .ne. 'GRID') then
          print'('' this model must be a grid
     1 omod_type='',a16)',omod_type
          return
        endif

      print'(/,'' layered grid characteristics''
     1,/,'' nx_lay='',i5,'' x0_lay='',f10.2,'' dx_lay='',f10.4
     1,/,'' ny_lay='',i5,'' y0_lay='',f10.2,'' dy_lay='',f10.4
     1,/,'' nz_lay='',i5,'' zlmin='',f10.2,'' zlmax='',f10.4)'
     1,nx_lay,x0_lay,dx_lay
     1,ny_lay,y0_lay,dy_lay
     1,nz_lay,z_min,z_max
      print'(/,'' 3d grid characteristics''
     1,/,'' nx_vel='',i5,'' x0_vel='',f10.2,'' dx_vel='',f10.4
     1,/,'' ny_vel='',i5,'' y0_vel='',f10.2,'' dy_vel='',f10.4
     1,/,'' nz_vel='',i5,'' z0_vel='',f10.2,'' dz_vel='',f10.4)'
     1,nx_vel,x0_vel,dx_vel
     1,ny_vel,y0_vel,dy_vel
     1,nz_vel,z0_vel,dz_vel
      crd80 = ' '
      write(crd80,'(a)')' RMOD replacing 3D grid from 3D layer'
      call rmod_title_to_history(crd80)
      crd80 = ' '
      write(crd80,'('' nz_lay:'',i5)')nz_lay
      call rmod_card_to_history(crd80)
      crd80 = ' '
      write(crd80,'('' nx_lay:'',i5,'' x0_lay:'',f10.2
     1,'' dx_lay:'',f10.4)')
     1nx_lay,x0_lay,dx_lay
      call rmod_card_to_history(crd80)
      crd80 = ' '
      write(crd80,'('' ny_lay:'',i5,'' y0_lay:'',f10.2
     1,'' dy_lay:'',f10.4)')
     1ny_lay,y0_lay,dy_lay
      call rmod_card_to_history(crd80)
      crd80 = ' '
      write(crd80,'('' nx_vel:'',i5,'' x0_vel:'',f10.2,''
     1 dx_vel:'',f10.4)')
     1nx_vel,x0_vel,dx_vel
      call rmod_card_to_history(crd80)
      crd80 = ' '
      write(crd80,'('' ny_vel:'',i5,'' y0_vel:'',f10.2,''
     1 dy_vel:'',f10.4)')
     1ny_vel,y0_vel,dy_vel
      call rmod_card_to_history(crd80)
      crd80 = ' '
      write(crd80,'('' nz_vel:'',i5,'' z0_vel:'',f10.2,''
     1 dz_vel:'',f10.4)')
     1nz_vel,z0_vel,dz_vel
      call rmod_card_to_history(crd80)

      call util_invert(nxv,vel)

      call gtol_replace_grid(
     1nx_lay,x0_lay,dx_lay
     1,ny_lay,y0_lay,dy_lay
     1,nz_lay,zl
     1,nxv,iv1,iv2,iv3,xv,yv,zv,vel
     1,ncv,icv,xcv,ycv,zcv
     1,nx_vel,x0_vel,dx_vel
     1,ny_vel,y0_vel,dy_vel
     1,nz_vel,z0_vel,dz_vel
     1,v_grd,m_work,work
     1,i_err)
      if (i_err .ne. 0) goto 999

      call util_invert(nxv,vel)

      mod_type ='GRID'

      return
  999 continue
      print'(''errror in rmod_replace_grid'')'
      return
      end

c23456789012345678901234567890123456789012345678901234567890123456789012
      subroutine rmod_write_ascii(h_file,mod_type
     1,nx,x0,dx,ny,y0,dy,nz,z0,dz,v)

      implicit  none
      integer   nx,ny,nz
      real      x0,dx,y0,dy,z0,dz,v(nz*nx*ny)
      character h_file*(*),mod_type*(*)
      character form*80,crd80*80

      real      x,y,z
      integer   util_len_r
      integer   i_file,ix,iy,iz,i_err,i,j
      integer   nx_o,ny_o,nz_o,nxy,nvpp
      character file*80
      character ans*1,bufd*9

      integer   m_column
      integer   n_column,ix_column,iy_column,iz_column,iv_column
      parameter (m_column=20)
      real      data(m_column)
      data      i_file/1/

      print'(/,'' This option writes the current gridded velocity''
     1,'' model as an ascci file'')'
      if (mod_type(1:1) .ne. 'G') then
        print'('' Model mod_type must be grid or G3DL for this option''
     1,'' mod_type='',a16)',mod_type
        call rmod_pause(' ',' ')
        return
      endif

      call date(bufd)

      call util_copy_file_name(h_file,file)
      call util_get_file_name(' Enter the ascii filename'
     1,file,'xyz')

      nxy = nx * ny

      call rmodopen(i_file,file,'FORMATTED','NEW'
     1,'SEQUENTIAL','IEEE',0,i_err)
      if (i_err .ne. 0) goto 999

      if (mod_type(1:4) .eq. 'G3DL') then
        n_column = 3
        form = '(1x,g16.9,1x,g16.9,1x,g16.9)'
      else
        n_column = 4
        form = '(1x,g16.9,1x,g16.9,1x,g16.9,1x,g16.9)'
      endif

c      call rmod_get_columns(-1,n_column
c     1,ix_column,iy_column,iz_column,iv_column)
      ix_column = 1
      iy_column = 2
      iz_column = 3
      iv_column = 4
c    1 continue
c      print'(/,'' enter the format to write the data default='',a
c     1,/,'' you will have'',i5,'' columns'')'
c     1,form(1:util_len_r(form)),n_column
c      crd80 = form
c      read(*,'(a)',err=1,end=2)crd80
c      if (crd80 .ne. ' ') read(crd80,'(a)',err=1)form
c    2 continue
c      print'('' format='',a)',form(1:util_len_r(form))

      if (mod_type(1:4) .eq. 'G3DL') then
        i = 0
        do iz = 1 , nz
          do iy = 1 , ny
            y = (iy - 1) * dy + y0
            do ix = 1 , nx
              i = i + 1
              x = (ix - 1) * dx + x0
              data(ix_column) = x
              data(iy_column) = y
              data(iz_column) = v(i)
c              write(i_file,form)(data(i),i=1,n_column)
              write(i_file
     1,'('' XLINE:'',i7,'' YLINE:'',i7,1x,f10.3)')
     1nint(x),nint(y),v(i)
            enddo    ! do ix = 1 , nx
          enddo    ! do iy = 1 , ny
        enddo    ! do iz = 1 , nz
      else
        call util_invert(nx*ny*nz,v)
        i = 0
        do iy = 1 , ny
          y = (iy - 1) * dy + y0
          do ix = 1 , nx
            x = (ix - 1) * dx + x0
            do iz = 1 , nz
              i = i + 1
              z = (iz - 1) * dz + z0
              data(ix_column) = x
              data(iy_column) = y
              data(iz_column) = z
              data(iv_column) = v(i)
c              write(i_file,form)(data(j),j=1,n_column)
            enddo    ! do iz = 1 , nz
          enddo    ! do ix = 1 , nx
        enddo    ! do iy = 1 , ny
        call util_invert(nx*ny*nz,v)
      endif
      call rmodclos(i_file)

      return
  999 continue
      call rmod_pause(' error reading ascii file',file)
      return
      end

c23456789012345678901234567890123456789012345678901234567890123456789012
      subroutine rmod_write_charisma(h_file,mod_type,nbh,b_name
     1,nx_vel,x0_vel,dx_vel
     1,ny_vel,y0_vel,dy_vel
     1,nz_vel,z0_vel,dz_vel
     1,v_grd)
      implicit none
      integer nbh

      integer nx_vel
      real    x0_vel,dx_vel
      integer ny_vel
      real    y0_vel,dy_vel
      integer nz_vel
      real    z0_vel,dz_vel
      real    v_grd(1)
      real    x,y
      character h_file*(*),mod_type*(*),b_name(1)*16

      integer i_file,n0,i_err,ix,iy,iz,lf,i
      character crd80*80,file*64,ans*1
      if (mod_type(1:4) .ne. 'G3DL') return
      call rmod_ans(ans,'N','Y','N',' create charisma horizon files?')
      if (ans .eq. 'N') return

      print'('' number of gridded horizons:'',i8
     1,'' number of horizon names:'',i8)',nz_vel,nbh
      crd80 = ' '
      write(crd80,'(a)')' RMOD writing generic files'
      call rmod_title_to_history(crd80)
      crd80 = ' '
      write(crd80,'('' Number of gridded horizons:'',i8)')nz_vel
      call rmod_card_to_history(crd80)

      i = 0
      do iz = 1 , nz_vel
        if (iz .le. nbh) then
          call util_copy_file_name(b_name(iz),file)
        else
          call util_copy_file_name(h_file,file)
        endif
        print'('' horizon number ='',i8,'' horizon name ='',a16)'
     1,iz,b_name(min(iz,nbh))
        print'('' creating generic file for horizon '',i8)',iz
        call util_get_file_name(' Enter output generic file',file
     1,'charisma')
        call rmodopen(i_file,file,'FORMATTED','NEW','SEQUENTIAL'
     1,'IEEE',n0,i_err)
        call rmodlenr(lf,file)
        crd80 = ' '
        write(crd80,'('' horizon number ='',i8,'' horizon name ='',a16
     1,'' file name='',a)')iz,b_name(min(iz,nbh)),file(1:lf)
        call rmod_card_to_history(crd80)

        if (i_err .eq. 0) then
          do iy = 1 , ny_vel
            y = (iy - 1) * dy_vel + y0_vel

            do ix = 1 , nx_vel
              x = (ix - 1) * dx_vel + x0_vel
              i = i + 1
c              write(i_file,*)
c     1nint(x),nint(y),v_grd(i)
              write(i_file,
     1'(''INLINE :'',i7,'' XLINE :'',i7,31x,f14.5)')
     1nint(x),nint(y),v_grd(i)
            enddo    ! do ix = 1 , nx_vel
          enddo    ! do iy = 1 , ny_vel
          call rmodclos(i_file)
        else    ! if (i_err .eq. 0) then
          print'('' could not open file for horizon='',i8)',iz
        endif    ! if (i_err .eq. 0) then
      enddo    ! do iz = 1 , nz_vel
      return
      end

c23456789012345678901234567890123456789012345678901234567890123456789012
      subroutine rmod_write_grid_to_ascii(mod_type,h_file,cxi,cyi,czi
     1,m_cord,n_cord,x_cord,cord
     1,nx,x0,dx
     1,ny,y0,dy
     1,nz,z0,dz
     1,v
     1,m_work,work)
c  view columns and rows of gridded i_model
      implicit none
      integer  util_len_r
      character mod_type*(*),h_file*(*)
      character mod_type_new*16
      character cxi*16,cyi*16,czi*16
      integer   m_cord,n_cord
      character cord(*)*16
      real      x_cord
      integer  nx
      real     x0,dx
      integer  ny
      real     y0,dy
      integer  nz
      real     z0,dz
      real     v(nx*ny*nz)
      integer  m_work
      real     work(m_work)

      integer nxg
      real    xgmin,xginc
      integer nyg
      real    ygmin,yginc
      integer nzg
      real    zgmin,zginc
      real    x_min,x_max
      real    y_min,y_max
      real    z_min,z_max
      real    z_datum
      integer  i_format,i_file,i_err
      integer  ix_1,ix_2,ix_inc,lx
      integer  iy_1,iy_2,iy_inc,ly
      integer  iz_1,iz_2,iz_inc,lz
      integer  i_r_file,n0,n_one
      integer  n_rec,l_rec
      integer  nt_glb
      real     t0_glb,dt_glb
      real     tr_max

      double precision axx,axy,ax0,ayx,ayy,ay0
      character r_file*80
      character crd80*80,a_file*80,b_file*80,d_ext*16,h_ext*16
      character word_type*8,status*3,m_file*64,t_file*64

      print'(/,'' This option displays gridded velocity values''
     1,/,'' nx='',i5,'' x0='',f10.2,'' dx='',f10.4
     1,/,'' ny='',i5,'' y0='',f10.2,'' dy='',f10.4
     1,/,'' nz='',i5,'' z0='',f10.2,'' dz='',f10.4)'
     1,nx,x0,dx,ny,y0,dy,nz,z0,dz

      if (mod_type(1:1) .ne. 'G') then
        print'('' model mod_type must be grid or G3DL for this option''
     1,'' mod_type='',a16)',mod_type
        call rmod_pause(' ',' ')
        return
      endif

      if (mod_type .eq. 'G3DL') then
        lx = 1
        ly = nx
        lz = nx * ny
      else
        lx = nz
        ly = nx * nz
        lz = 1
      endif

    1 continue
      i_format = 1
      crd80 = ' '

      if (mod_type .eq. 'G3DL') then

      print'(/,'' model mod_type='',a
     1,/,'' enter the output format type''
     1,/,'' 1  - X,Y,Z''
     1,/,'' 2  - Z(x,y)''
     1,/,'' 3  - HANDVEL''
     1,/,'' 4  - CHARISMA''
     1,/,'' 5  - CHARISMA with rotation''
     1,/,'' 6  - GENERIC''
     1,/,'' 7  - CPS VELOCITY''
     1,/,'' default='',i1)',mod_type,i_format

      else    ! if (mod_type .eq. 'G3DL') then

      print'(/,'' model mod_type='',a
     1,/,'' enter the output format type''
     1,/,'' 1  - X,Y,Z,V''
     1,/,'' 2  - V(z,x,y)''
     1,/,'' 3  - HANDVEL''
     1,/,'' 4  - CHARISMA''
     1,/,'' 5  - CHARISMA with rotation''
     1,/,'' 6  - GENERIC''
     1,/,'' 7  - CPS VELOCITY''
     1,/,'' 8  - SIMPLE BINARY''
     1,/,'' default='',i1)',mod_type,i_format

      endif    ! if (mod_type .eq. 'G3DL') then

      read(*,'(a)',err=1,end=2)crd80
      if (crd80 .ne. ' ') read(crd80,*,err=1,end=2)i_format
    2 continue

      if (i_format .eq. 5) then

        print'(/,'' enter the rotation file name''
     1,/,'' this file should have 6 numbers in the order''
     1,/,''axx,axy,ax0,ayx,ayy,ay0''
     1,/,'' x_new = axx * x_old + axy * y_old + ax0''
     1,/,'' y_new = ayx * x_old + ayy * y_old + ay0'')'
        r_file = 'rmod'
        call util_get_file_name(' Enter file name',r_file,'rotation')
        call rmod_inquire_exist(r_file,*1)
        call rmodopen(i_r_file,r_file,'FORMATTED','OLD','SEQUENTIAL'
     1,'IEEE',n0,i_err)
        if (i_err .ne. 0) goto 1
        read(i_r_file,*,err=1)axx,axy,ax0,ayx,ayy,ay0
        call rmodclos(i_r_file)
      endif    ! if (i_format .eq. 4) then

      if (i_format .eq. 1 .and. mod_type(1:4) .eq. 'G3DL') then
        d_ext = 'xyz'
        h_ext = 'hxyz'
        word_type = 'ASCII'
      elseif (i_format .eq. 1) then
        d_ext = 'xyzv'
        h_ext = 'hxyzv'
        word_type = 'ASCII'
      elseif (i_format .eq. 2) then
        d_ext = 'agrid'
        h_ext = 'hagrid'
        word_type = 'AGRID'
      elseif (i_format .eq. 3) then
        d_ext = 'handvel'
      elseif (i_format .eq. 4) then
        d_ext = 'charisma'
      elseif (i_format .eq. 5) then
        d_ext = 'charisma'
      elseif (i_format .eq. 6) then
        d_ext = 'generic'
      elseif (i_format .eq. 7) then
        d_ext = 'vel'
      elseif (i_format .eq. 8) then
        h_ext = 'head'
        d_ext = 'data'
      else
       return
      endif     ! if (i_format .eq. 1) then

      call rmod_efil(h_file,a_file)
      print'(/,'' enter screen for screen dump'')'
      call util_get_file_name(' Enter output file for printout'
     1,a_file,d_ext)
      call rmodcaps(a_file,b_file)

c  read the output grid size
      call rmod_write_grid_to_ascii1('x',nx,ix_1,ix_2,ix_inc)
      call rmod_write_grid_to_ascii1('y',ny,iy_1,iy_2,iy_inc)
      call rmod_write_grid_to_ascii1('z',nz,iz_1,iz_2,iz_inc)

      if (b_file(1:6) .eq. 'SCREEN') then

        i_file = 6

      elseif (i_format .eq. 8) then

      elseif (i_format .ne. 7) then

        call rmodopen(i_file,a_file,'FORMATTED','NEW'
     1,'SEQUENTIAL','IEEE',0,i_err)
        if (i_err .ne. 0) return

      endif

      if (mod_type .ne. 'G3DL') call util_invert(nx*ny*nz,v)
c  x,y,z,v format
      if (i_format .eq. 1) then

        if (mod_type .eq. 'G3DL') then

          call rmod_write_to_xyzv_g3dl(i_file
     1,ix_1,ix_2,ix_inc,lx,x0,dx
     1,iy_1,iy_2,iy_inc,ly,y0,dy
     1,iz_1,iz_2,iz_inc,lz,z0,dz
     1,v)

        else    ! if (mod_type .eq. 'G3DL') then

          call rmod_write_to_xyzv_grid(i_file
     1,ix_1,ix_2,ix_inc,lx,x0,dx
     1,iy_1,iy_2,iy_inc,ly,y0,dy
     1,iz_1,iz_2,iz_inc,lz,z0,dz
     1,v)

        endif    ! if (mod_type .eq. 'G3DL') then

      elseif (i_format .eq. 2) then

        if (mod_type .eq. 'G3DL') then

          call rmod_write_to_agrid_g3dl(i_file
     1,ix_1,ix_2,ix_inc,lx,x0,dx
     1,iy_1,iy_2,iy_inc,ly,y0,dy
     1,iz_1,iz_2,iz_inc,lz,z0,dz
     1,v)

        else    ! if (mod_type .eq. 'G3DL') then

          call rmod_write_to_agrid_grid(i_file
     1,ix_1,ix_2,ix_inc,lx,x0,dx
     1,iy_1,iy_2,iy_inc,ly,y0,dy
     1,iz_1,iz_2,iz_inc,lz,z0,dz
     1,v)

        endif    ! if (mod_type .eq. 'G3DL') then

c  handvel
      elseif (i_format .eq. 3) then

        if (mod_type .eq. 'G3DL') then

          call rmod_write_to_handvel_g3dl(i_file
     1,ix_1,ix_2,ix_inc,lx,x0,dx
     1,iy_1,iy_2,iy_inc,ly,y0,dy
     1,iz_1,iz_2,iz_inc,lz,z0,dz
     1,v)

        else    ! if (mod_type .eq. 'G3DL') then

          call rmod_write_to_handvel_grid(i_file
     1,ix_1,ix_2,ix_inc,lx,x0,dx
     1,iy_1,iy_2,iy_inc,ly,y0,dy
     1,iz_1,iz_2,iz_inc,lz,z0,dz
     1,v)

        endif    ! if (mod_type .eq. 'G3DL') then

c  charisma
      elseif (i_format .eq. 4) then

        if (mod_type .eq. 'G3DL') then

          call rmod_write_to_charisma_g3dl(i_file
     1,ix_1,ix_2,ix_inc,lx,x0,dx
     1,iy_1,iy_2,iy_inc,ly,y0,dy
     1,iz_1,iz_2,iz_inc,lz,z0,dz
     1,v)

        else    ! if (mod_type .eq. 'G3DL') then

          call rmod_write_to_charisma_grid(i_file
     1,ix_1,ix_2,ix_inc,lx,x0,dx
     1,iy_1,iy_2,iy_inc,ly,y0,dy
     1,iz_1,iz_2,iz_inc,lz,z0,dz
     1,v)

        endif    ! if (mod_type .eq. 'G3DL') then

c  charisma
      elseif (i_format .eq. 5) then

        if (mod_type .eq. 'G3DL') then

          call rmod_write_to_charisma_g3dl_wr(i_file
     1,axx,axy,ax0,ayx,ayy,ay0
     1,ix_1,ix_2,ix_inc,lx,x0,dx
     1,iy_1,iy_2,iy_inc,ly,y0,dy
     1,iz_1,iz_2,iz_inc,lz,z0,dz
     1,v)

        else    ! if (mod_type .eq. 'G3DL') then

          call rmod_write_to_charisma_grid_wr(i_file
     1,axx,axy,ax0,ayx,ayy,ay0
     1,ix_1,ix_2,ix_inc,lx,x0,dx
     1,iy_1,iy_2,iy_inc,ly,y0,dy
     1,iz_1,iz_2,iz_inc,lz,z0,dz
     1,v)

        endif    ! if (mod_type .eq. 'G3DL') then

c  generic
      elseif (i_format .eq. 6) then

        if (mod_type .eq. 'G3DL') then

          call rmod_write_to_generic_g3dl(i_file
     1,ix_1,ix_2,ix_inc,lx,x0,dx
     1,iy_1,iy_2,iy_inc,ly,y0,dy
     1,iz_1,iz_2,iz_inc,lz,z0,dz
     1,v)

        else    ! if (mod_type .eq. 'G3DL') then

          call rmod_write_to_generic_grid(i_file
     1,ix_1,ix_2,ix_inc,lx,x0,dx
     1,iy_1,iy_2,iy_inc,ly,y0,dy
     1,iz_1,iz_2,iz_inc,lz,z0,dz
     1,v)

        endif    ! if (mod_type .eq. 'G3DL') then

      elseif (i_format .eq. 7) then

        call rmod_write_to_cps_grid(a_file,cxi,cyi,czi
     1,ix_1,ix_2,ix_inc,lx,nx,x0,dx
     1,iy_1,iy_2,iy_inc,ly,ny,y0,dy
     1,iz_1,iz_2,iz_inc,lz,nz,z0,dz,v,m_work,work)

      elseif (i_format .eq. 8) then

        if (mod_type .eq. 'G3DL') then

        call rmod_write_to_util_trace_g3dl(a_file
     1,ix_1,ix_2,ix_inc,lx,nx,x0,dx
     1,iy_1,iy_2,iy_inc,ly,ny,y0,dy
     1,iz_1,iz_2,iz_inc,lz,nz,z0,dz,v,m_work,work)

        else    ! if (mod_type .eq. 'G3DL') then

        call rmod_write_to_util_trace_grid(a_file
     1,ix_1,ix_2,ix_inc,lx,nx,x0,dx
     1,iy_1,iy_2,iy_inc,ly,ny,y0,dy
     1,iz_1,iz_2,iz_inc,lz,nz,z0,dz,v,m_work,work)

        endif    ! if (mod_type .eq. 'G3DL') then

      endif    ! if (i_format .eq. 1) then

      if (i_format .ne. 8) call rmodclos(i_file)

      if (mod_type .ne. 'G3DL') call util_invert(nx*ny*nz,v)

      if (i_format .le. 2) then
      nxg = (ix_2 - ix_1) / ix_inc + 1
      xgmin = (ix_1 - 1) * dx + x0
      xginc = ix_inc * dx
      x_min = xgmin
      x_max = (nxg - 1) * xginc + xgmin
      nyg = (iy_2 - iy_1) / iy_inc + 1
      ygmin = (iy_1 - 1) * dy + y0
      yginc = iy_inc * dy
      y_min = ygmin
      y_max = (nyg - 1) * yginc + ygmin
      nzg = (iz_2 - iz_1) / iz_inc + 1
      zgmin = (iz_1 - 1) * dz + z0
      zginc = iz_inc * dz
      z_min = zgmin
      z_max = (nzg - 1) * zginc + zgmin
      z_datum = 0.

      call rmod_ext_replace(h_file,h_ext)
c      mod_type = 'GRID'
      mod_type_new = 'GRID'
      status = 'NEW'
      m_file = 'NONE'
      t_file = 'SAME'
      n_one = 1
      call rmodwrhd(h_file,a_file,word_type,m_file,t_file
     1,mod_type_new,status,n_one
     1,cxi,cyi,czi
     1,cxi,cyi,czi
     1,m_cord,n_cord,x_cord,cord
     1,x_min,x_max
     1,y_min,y_max
     1,z_min,z_max
     1,z_datum
     1,nxg,xgmin,xginc
     1,nyg,ygmin,yginc
     1,nzg,zgmin,zginc
     1,*999)
      endif    ! if (i_format .le. 2) then

      return

  999 continue
      print'(/,'' error in write_grid_to_ascii'')'
      return

      end

c23456789012345678901234567890123456789012345678901234567890123456789012
      subroutine rmod_write_to_util_trace_grid(a_file
     1,ix_1,ix_2,ix_inc,lx,nx,x0,dx
     1,iy_1,iy_2,iy_inc,ly,ny,y0,dy
     1,iz_1,iz_2,iz_inc,lz,nz,z0,dz
     1,v
     1,m_work,work)
      implicit none
      character a_file*(*)
      integer   ix_1,ix_2,ix_inc,lx,nx
      real      x0,dx
      integer   iy_1,iy_2,iy_inc,ly,ny
      real      y0,dy
      integer   iz_1,iz_2,iz_inc,lz,nz
      real      z0,dz
      real      v(nz,nx,ny)

      integer   m_work
      real      work(m_work)

      integer   ix,nx_do
      integer   iy,ny_do
      integer   iz,nz_do

      integer   i_file
      integer   n_rec,l_rec,i_rec

      integer   nt_glb
      real      t0_glb,dt_glb
      real      tr_max

      integer   i_err

      nx_do = (ix_2 - ix_1) /ix_inc + 1
      ny_do = (iy_2 - iy_1) /iy_inc + 1
      nz_do = (iz_2 - iz_1) /iz_inc + 1

      n_rec = nx_do * ny_do
      nt_glb = nz_do
      t0_glb = (iz_1 - 1) * dz + z0
      dt_glb = dz * iz_inc

      call util_trace_open('new',a_file,i_file
     1,n_rec,0,nt_glb,t0_glb,dt_glb,tr_max,i_err)

      i_rec = 0
      do iy = iy_1 , iy_2 , iy_inc

        do ix = ix_1 , ix_2 , ix_inc

        i_rec = i_rec + 1
        call util_copy_inc(nt_glb,1,v(iz_1,ix,iy),iz_inc,work)
        call util_trace_write(i_file,i_rec
     1,0,work,nt_glb,work,i_err)
        if (i_err .ne. 0) goto 999

        enddo    ! do ix = ix_1 , ix_2 , ix_inc

      enddo    ! do iy = iy_1 , iy_2 , iy_inc

      call util_trace_close(i_file,i_err)
      if (i_err .ne. 0) goto 999

      return

  999 continue
      call util_pause(' error in rmod_write_to_util_trace_grid',i_err)
      i_err = -1
      return

      end

c23456789012345678901234567890123456789012345678901234567890123456789012
      subroutine rmod_write_to_util_trace_g3dl(a_file
     1,ix_1,ix_2,ix_inc,lx,nx,x0,dx
     1,iy_1,iy_2,iy_inc,ly,ny,y0,dy
     1,iz_1,iz_2,iz_inc,lz,nz,z0,dz
     1,v
     1,m_work,work)
      implicit none
      character a_file*(*)
      integer   ix_1,ix_2,ix_inc,lx,nx
      real      x0,dx
      integer   iy_1,iy_2,iy_inc,ly,ny
      real      y0,dy
      integer   iz_1,iz_2,iz_inc,lz,nz
      real      z0,dz
      real      v(nx,ny,nz)

      integer   m_work
      real      work(m_work)

      integer   ix,nx_do
      integer   iy,ny_do
      integer   iz,nz_do

      integer   i_file
      integer   n_rec,l_rec,i_rec

      integer   nt_glb
      real      t0_glb,dt_glb
      real      tr_max

      integer   i_err

      nx_do = (ix_2 - ix_1) /ix_inc + 1
      ny_do = (iy_2 - iy_1) /iy_inc + 1
      nz_do = (iz_2 - iz_1) /iz_inc + 1

      n_rec = nz_do * ny_do
      nt_glb = nx_do
      t0_glb = (ix_1 - 1) * dx + x0
      dt_glb = dx * ix_inc

      call util_trace_open('new',a_file,i_file
     1,n_rec,0,nt_glb,t0_glb,dt_glb,tr_max,i_err)

      i_rec = 0

      do iz = iz_1 , iz_2 , iz_inc

        do iy = iy_1 , iy_2 , iy_inc

        i_rec = i_rec + 1
        call util_copy_inc(nt_glb,1,v(ix_1,iy,iz),ix_inc,work)
        call util_trace_write(i_file,i_rec
     1,0,work,nt_glb,work,i_err)
        if (i_err .ne. 0) goto 999

        enddo    ! do iy = iy_1 , iy_2 , iy_inc

      enddo    ! do iz = iz_1 , iz_2 , iz_inc

      call util_trace_close(i_file,i_err)
      if (i_err .ne. 0) goto 999

      return

  999 continue
      call util_pause(' error in rmod_write_to_util_trace_g3dl',i_err)
      i_err = -1
      return

      end

c23456789012345678901234567890123456789012345678901234567890123456789012
      subroutine rmod_write_grid_to_ascii1(title,nx,ix_1,ix_2,ix_inc)
      character title*(*),crd80*80
      ix_1 = 1
      ix_2 = nx
      ix_inc = 1
    1 continue
      crd80 = ' '
      print'(/,'' enter '',a1,'' first, last and spacing defaults=''
     1,3(1x,i5))',title,ix_1,ix_2,ix_inc
      read(*,'(a)',err=1,end=2)crd80
      if (crd80 .ne. ' ')read(crd80,*,err=1,end=2)ix_1,ix_2,ix_inc
    2 continue

      return
      end

c23456789012345678901234567890123456789012345678901234567890123456789012
      subroutine rmod_write_to_xyzv_g3dl(i_file
     1,ix_1,ix_2,ix_inc,lx,x0,dx
     1,iy_1,iy_2,iy_inc,ly,y0,dy
     1,iz_1,iz_2,iz_inc,lz,z0,dz
     1,v)
      implicit none
      integer  i_file
      integer  ix_1,ix_2,ix_inc,lx
      real     x0,dx
      integer  iy_1,iy_2,iy_inc,ly
      real     y0,dy
      integer  iz_1,iz_2,iz_inc,lz
      real     z0,dz
      real     v(1)

      integer  ix,iy,iz,i
      real      x, y, z

      do iz = iz_1 , iz_2 , iz_inc
        do iy = iy_1 , iy_2 , iy_inc
          do ix = ix_1 , ix_2 , ix_inc
            x = (ix - 1) * dx + x0
            y = (iy - 1) * dy + y0
            z = (iz - 1) * dz + z0
            i = (ix - 1) * lx + (iy - 1) * ly + (iz - 1) * lz + 1
                write(i_file
     1,'(1x,f12.4,1x,f12.4,1x,f12.4,1x,i8)')x,y,v(i),iz

          enddo    ! do ix = ix_1 , ix_2 , ix_inc
        enddo    ! do iy = iy_1 , iy_2 , iy_inc
      enddo    ! do iz = iz_1 , iz_2 , iz_inc

      return
      end

c23456789012345678901234567890123456789012345678901234567890123456789012
      subroutine rmod_write_to_xyzv_grid(i_file
     1,ix_1,ix_2,ix_inc,lx,x0,dx
     1,iy_1,iy_2,iy_inc,ly,y0,dy
     1,iz_1,iz_2,iz_inc,lz,z0,dz
     1,v)
      implicit none
      integer  i_file
      integer  ix_1,ix_2,ix_inc,lx
      real     x0,dx
      integer  iy_1,iy_2,iy_inc,ly
      real     y0,dy
      integer  iz_1,iz_2,iz_inc,lz
      real     z0,dz
      real     v(1)

      integer  ix,iy,iz,i
      real      x, y, z

      do iy = iy_1 , iy_2 , iy_inc
        do ix = ix_1 , ix_2 , ix_inc
          do iz = iz_1 , iz_2 , iz_inc
            x = (ix - 1) * dx + x0
            y = (iy - 1) * dy + y0
            z = (iz - 1) * dz + z0
            i = (ix - 1) * lx + (iy - 1) * ly + (iz - 1) * lz + 1
            write(i_file,'(4(1x,f12.4))')x,y,z,v(i)

          enddo    ! do iz = iz_1 , iz_2 , iz_inc
        enddo    ! do ix = ix_1 , ix_2 , ix_inc
      enddo    ! do iy = iy_1 , iy_2 , iy_inc

      return
      end

c23456789012345678901234567890123456789012345678901234567890123456789012
      subroutine rmod_write_to_agrid_g3dl(i_file
     1,ix_1,ix_2,ix_inc,lx,x0,dx
     1,iy_1,iy_2,iy_inc,ly,y0,dy
     1,iz_1,iz_2,iz_inc,lz,z0,dz
     1,v)
      implicit none
      integer  i_file
      integer  ix_1,ix_2,ix_inc,lx
      real     x0,dx
      integer  iy_1,iy_2,iy_inc,ly
      real     y0,dy
      integer  iz_1,iz_2,iz_inc,lz
      real     z0,dz
      real     v(1)

      integer  ix,iy,iz,i
      real      x, y, z

      do iz = iz_1 , iz_2 , iz_inc
        do iy = iy_1 , iy_2 , iy_inc
          do ix = ix_1 , ix_2 , ix_inc
            x = (ix - 1) * dx + x0
            y = (iy - 1) * dy + y0
            z = (iz - 1) * dz + z0
            i = (ix - 1) * lx + (iy - 1) * ly + (iz - 1) * lz + 1
            write(i_file,'(1x,f12.4)')v(i)

          enddo    ! do ix = ix_1 , ix_2 , ix_inc
        enddo    ! do iy = iy_1 , iy_2 , iy_inc
      enddo    ! do iz = iz_1 , iz_2 , iz_inc

      return
      end

c23456789012345678901234567890123456789012345678901234567890123456789012
      subroutine rmod_write_to_agrid_grid(i_file
     1,ix_1,ix_2,ix_inc,lx,x0,dx
     1,iy_1,iy_2,iy_inc,ly,y0,dy
     1,iz_1,iz_2,iz_inc,lz,z0,dz
     1,v)
      implicit none
      integer  i_file
      integer  ix_1,ix_2,ix_inc,lx
      real     x0,dx
      integer  iy_1,iy_2,iy_inc,ly
      real     y0,dy
      integer  iz_1,iz_2,iz_inc,lz
      real     z0,dz
      real     v(1)

      integer  ix,iy,iz,i
      real      x, y, z

      do iy = iy_1 , iy_2 , iy_inc
        do ix = ix_1 , ix_2 , ix_inc
          do iz = iz_1 , iz_2 , iz_inc
            x = (ix - 1) * dx + x0
            y = (iy - 1) * dy + y0
            z = (iz - 1) * dz + z0
            i = (ix - 1) * lx + (iy - 1) * ly + (iz - 1) * lz + 1
            write(i_file,'(1x,f12.4)')v(i)

          enddo    ! do iz = iz_1 , iz_2 , iz_inc
        enddo    ! do ix = ix_1 , ix_2 , ix_inc
      enddo    ! do iy = iy_1 , iy_2 , iy_inc

      return
      end

c23456789012345678901234567890123456789012345678901234567890123456789012
      subroutine rmod_write_to_handvel_g3dl(i_file
     1,ix_1,ix_2,ix_inc,lx,x0,dx
     1,iy_1,iy_2,iy_inc,ly,y0,dy
     1,iz_1,iz_2,iz_inc,lz,z0,dz
     1,v)
      implicit none
      integer  i_file
      integer  ix_1,ix_2,ix_inc,lx
      real     x0,dx
      integer  iy_1,iy_2,iy_inc,ly
      real     y0,dy
      integer  iz_1,iz_2,iz_inc,lz
      real     z0,dz
      real     v(1)

      integer  ix,iy,iz,i,j,k
      real      x, y, z
      real     z_temp(4),v_temp(4)

      do iz = iz_1 , iz_2 , iz_inc
        do iy = iy_1 , iy_2 , iy_inc
            y = (iy - 1) * dy + y0
            z = (iz - 1) * dz + z0
      write(i_file,'(''HANDVEL '',1x,i7,1x,i7)')nint(z),nint(y)
          j = 0
          do ix = ix_1 , ix_2 , ix_inc
            x = (ix - 1) * dx + x0
            i = (ix - 1) * lx + (iy - 1) * ly + (iz - 1) * lz + 1
                j = j + 1
                z_temp(j) = x
                v_temp(j) = v(i)
                if (j .eq. 4
     1.or. (iz_inc .gt. 0 .and. iz+iz_inc .gt. iz_2)
     1.or. (iz_inc .lt. 0 .and. iz+iz_inc .lt. iz_2)) then
      write(i_file,'(8(1x,i7))')(nint(z_temp(k)),nint(v_temp(k)),k=1,j)
                  j = 0
                endif    ! if (j .eq. 4

          enddo    ! do ix = ix_1 , ix_2 , ix_inc
        enddo    ! do iy = iy_1 , iy_2 , iy_inc
      enddo    ! do iz = iz_1 , iz_2 , iz_inc

      return
      end

c23456789012345678901234567890123456789012345678901234567890123456789012
      subroutine rmod_write_to_handvel_grid(i_file
     1,ix_1,ix_2,ix_inc,lx,x0,dx
     1,iy_1,iy_2,iy_inc,ly,y0,dy
     1,iz_1,iz_2,iz_inc,lz,z0,dz
     1,v)
      implicit none
      integer  i_file
      integer  ix_1,ix_2,ix_inc,lx
      real     x0,dx
      integer  iy_1,iy_2,iy_inc,ly
      real     y0,dy
      integer  iz_1,iz_2,iz_inc,lz
      real     z0,dz
      real     v(1)

      integer  ix,iy,iz,i,j,k
      real     x, y, z
      real     z_temp(4),v_temp(4)
      do iy = iy_1 , iy_2 , iy_inc
        do ix = ix_1 , ix_2 , ix_inc
          x = (ix - 1) * dx + x0
          y = (iy - 1) * dy + y0
      write(i_file,'(''HANDVEL '',1x,i7,1x,i7)')nint(x),nint(y)

          j = 0
          do iz = iz_1 , iz_2 , iz_inc
            z = (iz - 1) * dz + z0
            i = (ix - 1) * lx + (iy - 1) * ly + (iz - 1) * lz + 1

            j = j + 1
            z_temp(j) = z
            v_temp(j) = v(i)
            if (j .eq. 4
     1.or. (iz_inc .gt. 0 .and. iz+iz_inc .gt. iz_2)
     1.or. (iz_inc .lt. 0 .and. iz+iz_inc .lt. iz_2)) then
      write(i_file,'(8(1x,i7))')(nint(z_temp(k)),nint(v_temp(k)),k=1,j)
              j = 0
            endif    ! if (j .eq. 4

          enddo    ! do iz = iz_1 , iz_2 , iz_inc
        enddo    ! do ix = ix_1 , ix_2 , ix_inc
      enddo    ! do iy = iy_1 , iy_2 , iy_inc

      return
      end

c23456789012345678901234567890123456789012345678901234567890123456789012
      subroutine rmod_write_to_charisma_g3dl(i_file
     1,ix_1,ix_2,ix_inc,lx,x0,dx
     1,iy_1,iy_2,iy_inc,ly,y0,dy
     1,iz_1,iz_2,iz_inc,lz,z0,dz
     1,v)
      implicit none
      integer  i_file
      integer  ix_1,ix_2,ix_inc,lx
      real     x0,dx
      integer  iy_1,iy_2,iy_inc,ly
      real     y0,dy
      integer  iz_1,iz_2,iz_inc,lz
      real     z0,dz
      real     v(1)

      integer  ix,iy,iz,i
      real      x, y, z

      do iz = iz_1 , iz_2 , iz_inc
        do iy = iy_1 , iy_2 , iy_inc
          do ix = ix_1 , ix_2 , ix_inc
            x = (ix - 1) * dx + x0
            y = (iy - 1) * dy + y0
            z = (iz - 1) * dz + z0
            i = (ix - 1) * lx + (iy - 1) * ly + (iz - 1) * lz + 1
            write(i_file,
     1'(''INLINE :'',i7,'' XLINE :'',i7,31x,f14.5)')
     1nint(y),nint(x),v(i)

          enddo    ! do ix = ix_1 , ix_2 , ix_inc
        enddo    ! do iy = iy_1 , iy_2 , iy_inc
      enddo    ! do iz = iz_1 , iz_2 , iz_inc

      return
      end

c23456789012345678901234567890123456789012345678901234567890123456789012
      subroutine rmod_write_to_charisma_grid(i_file
     1,ix_1,ix_2,ix_inc,lx,x0,dx
     1,iy_1,iy_2,iy_inc,ly,y0,dy
     1,iz_1,iz_2,iz_inc,lz,z0,dz
     1,v)
      implicit none
      integer  i_file
      integer  ix_1,ix_2,ix_inc,lx
      real     x0,dx
      integer  iy_1,iy_2,iy_inc,ly
      real     y0,dy
      integer  iz_1,iz_2,iz_inc,lz
      real     z0,dz
      real     v(1)

      integer  ix,iy,iz,i
      real      x, y, z

      do iy = iy_1 , iy_2 , iy_inc
        do ix = ix_1 , ix_2 , ix_inc
          do iz = iz_1 , iz_2 , iz_inc
            x = (ix - 1) * dx + x0
            y = (iy - 1) * dy + y0
            z = (iz - 1) * dz + z0
            i = (ix - 1) * lx + (iy - 1) * ly + (iz - 1) * lz + 1
            write(i_file,
     1'(''INLINE :'',i7,'' XLINE :'',i7,31x,f14.5)')
     1nint(y),nint(x),v(i)

          enddo    ! do iz = iz_1 , iz_2 , iz_inc
        enddo    ! do ix = ix_1 , ix_2 , ix_inc
      enddo    ! do iy = iy_1 , iy_2 , iy_inc

      return
      end

c23456789012345678901234567890123456789012345678901234567890123456789012
      subroutine rmod_write_to_charisma_g3dl_wr(i_file
     1,axx,axy,ax0,ayx,ayy,ay0
     1,ix_1,ix_2,ix_inc,lx,x0,dx
     1,iy_1,iy_2,iy_inc,ly,y0,dy
     1,iz_1,iz_2,iz_inc,lz,z0,dz
     1,v)
      implicit none
      integer  i_file
      integer  ix_1,ix_2,ix_inc,lx
      double precision axx,axy,ax0,ayx,ayy,ay0,ax,ay
      real     x0,dx
      integer  iy_1,iy_2,iy_inc,ly
      real     y0,dy
      integer  iz_1,iz_2,iz_inc,lz
      real     z0,dz
      real     v(1)

      integer  ix,iy,iz,i
      real      x, y, z

      do iz = iz_1 , iz_2 , iz_inc
        do iy = iy_1 , iy_2 , iy_inc
          do ix = ix_1 , ix_2 , ix_inc
            x = (ix - 1) * dx + x0
            y = (iy - 1) * dy + y0
            z = (iz - 1) * dz + z0
            ax = axx * x + axy * y + ax0
            ay = ayx * x + ayy * y + ay0

            i = (ix - 1) * lx + (iy - 1) * ly + (iz - 1) * lz + 1
            write(i_file,
     1'(''INLINE :'',i7,'' XLINE :'',i7,1x,f12.2,1x,f12.2,5x,f14.5)')
     1nint(y),nint(x),ax,ay,v(i)
c     1'(''INLINE :'',i7,'' XLINE :'',i7,31x,f14.5)')

          enddo    ! do ix = ix_1 , ix_2 , ix_inc
        enddo    ! do iy = iy_1 , iy_2 , iy_inc
      enddo    ! do iz = iz_1 , iz_2 , iz_inc

      return
      end

c23456789012345678901234567890123456789012345678901234567890123456789012
      subroutine rmod_write_to_charisma_grid_wr(i_file
     1,axx,axy,ax0,ayx,ayy,ay0
     1,ix_1,ix_2,ix_inc,lx,x0,dx
     1,iy_1,iy_2,iy_inc,ly,y0,dy
     1,iz_1,iz_2,iz_inc,lz,z0,dz
     1,v)
      implicit none
      integer  i_file
      integer  ix_1,ix_2,ix_inc,lx
      double precision axx,axy,ax0,ayx,ayy,ay0,ax,ay
      real     x0,dx
      integer  iy_1,iy_2,iy_inc,ly
      real     y0,dy
      integer  iz_1,iz_2,iz_inc,lz
      real     z0,dz
      real     v(1)

      integer  ix,iy,iz,i
      real      x, y, z

      do iy = iy_1 , iy_2 , iy_inc
        do ix = ix_1 , ix_2 , ix_inc
          do iz = iz_1 , iz_2 , iz_inc
            x = (ix - 1) * dx + x0
            y = (iy - 1) * dy + y0
            z = (iz - 1) * dz + z0
            ax = axx * x + axy * y + ax0
            ay = ayx * x + ayy * y + ay0
            i = (ix - 1) * lx + (iy - 1) * ly + (iz - 1) * lz + 1
            write(i_file,
     1'(''INLINE :'',i7,'' XLINE :'',i7,1x,f12.2,1x,f12.2,5x,f14.5)')
     1nint(y),nint(x),ax,ay,v(i)
c     1'(''INLINE :'',i7,'' XLINE :'',i7,31x,f14.5)')
c     1nint(y),nint(x),v(i)

          enddo    ! do iz = iz_1 , iz_2 , iz_inc
        enddo    ! do ix = ix_1 , ix_2 , ix_inc
      enddo    ! do iy = iy_1 , iy_2 , iy_inc

      return
      end

c23456789012345678901234567890123456789012345678901234567890123456789012
      subroutine rmod_write_to_generic_g3dl(i_file
     1,ix_1,ix_2,ix_inc,lx,x0,dx
     1,iy_1,iy_2,iy_inc,ly,y0,dy
     1,iz_1,iz_2,iz_inc,lz,z0,dz
     1,v)
      implicit none
      integer  i_file
      integer  ix_1,ix_2,ix_inc,lx
      real     x0,dx
      integer  iy_1,iy_2,iy_inc,ly
      real     y0,dy
      integer  iz_1,iz_2,iz_inc,lz
      real     z0,dz
      real     v(1)

      integer  ix,iy,iz,i
      real      x, y, z

      do iz = iz_1 , iz_2 , iz_inc
        do ix = ix_1 , ix_2 , ix_inc
          write(i_file,'(8f10.4)')
     1(v((ix - 1) * lx + (iy - 1) * ly + (iz - 1) * lz + 1)
     1,iy=iy_1,iy_2,iy_inc)
        enddo    ! do ix = ix_1 , ix_2 , ix_inc
      enddo    ! do iz = iz_1 , iz_2 , iz_inc

      return
      end

c23456789012345678901234567890123456789012345678901234567890123456789012
      subroutine rmod_write_to_generic_grid(i_file
     1,ix_1,ix_2,ix_inc,lx,x0,dx
     1,iy_1,iy_2,iy_inc,ly,y0,dy
     1,iz_1,iz_2,iz_inc,lz,z0,dz
     1,v)
      implicit none
      integer  i_file
      integer  ix_1,ix_2,ix_inc,lx
      real     x0,dx
      integer  iy_1,iy_2,iy_inc,ly
      real     y0,dy
      integer  iz_1,iz_2,iz_inc,lz
      real     z0,dz
      real     v(1)

      integer  ix,iy,iz,i
      real      x, y, z

      do ix = ix_1 , ix_2 , ix_inc
        do iy = iy_1 , iy_2 , iy_inc
          write(i_file,'(8f10.4)')
     1(v((ix - 1) * lx + (iy - 1) * ly + (iz - 1) * lz + 1)
     1,iz=iz_1,iz_2,iz_inc)
        enddo    ! do iy = iy_1 , iy_2 , iy_inc
      enddo    ! do ix = ix_1 , ix_2 , ix_inc

      return
      end

cqqq
c23456789012345678901234567890123456789012345678901234567890123456789012
      subroutine rmod_write_to_cps_grid(h_file,cxi,cyi,czi
     1,ix_1,ix_2,ix_inc,lx,nx,x0,dx
     1,iy_1,iy_2,iy_inc,ly,ny,y0,dy
     1,iz_1,iz_2,iz_inc,lz,nz,z0,dz,v,m_work,work)

      implicit  none
      character h_file*(*)
      character cxi*(*),cyi*(*),czi*(*)
      integer   ix_1,ix_2,ix_inc,lx,nx
      real      x0,dx
      integer   iy_1,iy_2,iy_inc,ly,ny
      real      y0,dy
      integer   iz_1,iz_2,iz_inc,lz,nz
      real      z0,dz
      real      v(nx*ny*nz)
      integer   m_work
      real      work(m_work)

      real      x,y,z
      integer   head_x,head_y,i_file,ix,iy,i_err,iz,jz,i,it,iv,iw
      integer   nx0,ny0,nz0,nxy,nvpp
      character cx*16,cy*16,cz*16
      character file*80,v_name*8,vmod_type*4,project*10,line*10
      character r_date*5,p_date*5,id*3,comment*15,ans*1,bufd*9
      character vmod_type0*4
      character crd80*80
      data      i_file/1/

c  cps velocity functions do not like more than 99 points
      if (nz .gt. 99) then
        print'('' the number of depth points should be less than 99''
     1,'' current value='',i8
     1,/,'' use option 12 to interpolate to a new grid with nz=99,''
     1,'' dz='',f10.2)',nz,((nz-1)*dz) / 98
        goto 999
      endif    ! if (nz .gt. 99) then

c  determine the x header word
      call rmodcaps(cxi,cx)
      if (cx .eq. 'XANNOTATION') then
        head_x = 37
      elseif (cx .eq. 'XGRID') then
        head_x = 7
      else
        head_x = 17
      endif

c  determine the y header word
      call rmodcaps(cyi,cy)
      if (cy .eq. 'YANNOTATION') then
        head_y = 38
      elseif (cy .eq. 'YGRID') then
        head_y = 8
      else
        head_y = 18
      endif

      call rmodcaps(czi,cz)
      if (cz .eq. 'TIME') then
        vmod_type = 'VTIN'
      else
        vmod_type = 'VZIN'
      endif

      project = h_file(1:10)
      id      = 'ID'
      comment = 'xxx'
      call date(bufd)
      r_date  = bufd(1:5)
      p_date  = bufd(6:9)

      print'('' cx='',a16,'' cy='',a16,'' cz='',a16
     1,/,'' head_x='',i8,'' head_y='',i8,'' vmod_type='',a4)'
     1,cx,cy,cz,head_x,head_y,vmod_type

  102 continue
      print'(/,'' enter x,y header words default=''
     1,i6,1x,i6)',head_x,head_y
      crd80 = ' '
      read(*,'(a)',end=101,err=102)crd80
      if (crd80 .ne. ' ') read(crd80,*,end=101,err=102)head_x,head_y
  101 continue

  103 continue
      vmod_type0 = vmod_type
      print'(/,'' enter velocity type default='',a4)',vmod_type
      vmod_type0 = ' '
      read(*,'(a)',end=1,err=103)vmod_type0
      if (vmod_type0 .ne. ' ') vmod_type = vmod_type0
    1 continue

      nx0 = 0
      do ix = ix_1 , ix_2 , ix_inc
        nx0 = nx0 + 1
      enddo    ! do ix = ix_1 , ix_2 , ix_inc
      ny0 = 0
      do iy = iy_1 , iy_2 , iy_inc
        ny0 = ny0 + 1
      enddo    ! do iy = iy_1 , iy_2 , iy_inc
      nz0 = 0
      do iz = iz_1 , iz_2 , iz_inc
        nz0 = nz0 + 1
      enddo    ! do iz = iz_1 , iz_2 , iz_inc

      nxy = nx0 * ny0
      nvpp = 2
      it = 1
      iv = it + nz0
      iw = iv + nz0
      if (iw+nz .gt. m_work) GOTO 998

      print'('' before opencps h_file='',a)',h_file
      call open_cps_velfile2(i_file,h_file,'WRITE',nvpp,nxy,*999,*999
     1,head_x,head_y)

      do iy = iy_1 , iy_2 , iy_inc
        y = (iy - 1) * dy + y0
        do ix = ix_1 , ix_2 , ix_inc
          x = (ix - 1) * dx + x0
          v_name = 'V'
          call rmod_v_name(v_name,2,4,iy)
          call rmod_v_name(v_name,5,7,ix)
          jz = 0
          do iz = iz_1 , iz_2 , iz_inc
            i = (ix - 1) * lx + (iy - 1) * ly + (iz - 1) * lz + 1
            z = (iz - 1) * dz + z0
            work(it+jz) = z
            work(iv+jz) = v(i)
            jz = jz + 1
          enddo    ! do iz = iz_1 , iz_2 , iz_inc
          write(line,'(i6)')min(999999,max(-99999,nint(y)))
          call write_cps_velfile (i_file,nvpp,v_name,nz,x,y,vmod_type
     1,work(it),work(iv),project,line,r_date,p_date,id,comment,work(iw))

        enddo    ! do ix = 1 , nx
      enddo    ! do iy = 1 , ny
      call close_cps_velfile(i_file,i_err)
      if (i_err .ne. 0) goto 997
      return

  997 continue
      print'(/,'' error during close_cps_velfile'')'
      goto 999

  998 continue
      print'('' need more memory'')'
      goto 999

  999 continue
      call rmod_pause(' error in rmod_write_to_cps file=',file)
      return

      end

c23456789012345678901234567890123456789012345678901234567890123456789012
      subroutine rmod_limit_velocity(mod_type,n,v)
      implicit  none
      character mod_type*(*)
      integer   n
      real      v(n)
      integer   i,m
      real      v_min,v_max
      if (mod_type(1:4) .ne. 'GRID') return
      call util_invert(n,v)
      call util_min_max(v_min,v_max,n,v)
    1 continue
      print'(/,'' enter v_min,v_max defaults=''
     1,'' v_min='',f10.2,''v_max='',f10.2)',v_min,v_max
      read(*,*,err=1,end=2)v_min,v_max
    2 continue

      m = 0
      do i = 1 , n
        if (v(i) .lt. v_min .or. v(i) .gt. v_max) then
          v(i) = max(v_min,min(v_max,v(i)))
          m = m + 1
        endif
      enddo

      call util_min_max(v_min,v_max,n,v)
      print'('' n='',i10,'' n_change='',i10
     1,/,'' v_min='',f10.2,''v_max='',f10.2)'
     1,n,m,v_min,v_max
      call util_invert(n,v)

      return
      end

c23456789012345678901234567890123456789012345678901234567890123456789012
      subroutine rmod_transpose_grid(mod_type,n1,n2,n3,vel,m_work,work)
      implicit  none
      integer   util_len_r
      character mod_type*(*)
      integer   n1,n2,n3,m_work
      real      vel(n1*n2*n3),work(n1*n2*n3)

      character crd80*80
      integer   j_new,i_new(3)
      integer   j_old,i_old(3)
      integer   nt_new(3),it_new(3)
      integer   nt_old(3),it_old(3)
      integer   i_of_o(3)

      print'(/,'' rmod_transpose_grid''
     1,/,'' This option transposes dimensions in a gridded model''
     1,/,'' Note this does not alter ''
     1,'' XMIN,XINC, YMIN,YINC, ZMIN,ZINC in the header file''
     1,/,'' To transpose from Gocad to Conoco use 3,1,2 ''
     1,''(x,y,z to z,x,y)''
     1,/,'' To transpose from Conoco to Gocad use 2,3,1 ''
     1,''(z,x,y to x,y,z)''
     1,/,'' n1='',i10
     1,'' first  (fastest) dimension Gocad - x Conoco - z''
     1,/,'' n2='',i10
     1,'' second (middle ) dimension Gocad - y Conoco - x''
     1,/,'' n3='',i10
     1,'' third  (slowest) dimension Gocad - z Conoco - y''
     1,/,'' n1*n2*n3='',i10,'' m_work='',i10)'
     1,n1,n2,n3,n1*n2*n3,m_work

      if (mod_type(1:4) .ne. 'GRID') then
         print'('' model mod_type should be GRID mod_type='',a)'
     1,mod_type(1:util_len_r(mod_type))
         return
      endif

      if (m_work .lt. n1*n2*n3) then
        print'('' insufficent space to copy velocity''
     1,/,'' n1='',i10,'' n2='',i10,'' n3='',i10
     1,'' n1*n2*n3='',i10,'' m_work='',i10)',n1,n2,n3,n1*n2*n3,m_work
        return
      endif    ! if (m_work .lt. n1*n2*n3) then

    1 continue
      i_of_o(1) = 1
      i_of_o(2) = 2
      i_of_o(3) = 3
      print'(/,'' enter the new column order default='',i2,1x,i2,1x,i2)'
     1,i_of_o(1),i_of_o(2),i_of_o(3)
      crd80 = ' '
      read(*,'(a)',err=1,end=2)crd80
      if (crd80 .ne. ' ')read(crd80,*,err=1,end=2)
     1i_of_o(1),i_of_o(2),i_of_o(3)
    2 continue

      if ( i_of_o(1) .lt. 1 .or. i_of_o(1) .gt. 3
     1.or. i_of_o(2) .lt. 1 .or. i_of_o(2) .gt. 3
     1.or. i_of_o(3) .lt. 1 .or. i_of_o(3) .gt. 3
     1.or. i_of_o(1) .eq. i_of_o(2)
     1.or. i_of_o(1) .eq. i_of_o(3)
     1.or. i_of_o(2) .eq. i_of_o(3)) then
        print'(/,'' doing nothing because of selection of ''
     1,'' column order ='',1x,i5,1x,i5,1x,i5)'
     1,i_of_o(1),i_of_o(2),i_of_o(3)
        return
      endif

      call util_copy(n1*n2*n3,vel,work)

      nt_old(1) = n1       ! number of points in dimension 1
      nt_old(2) = n2       ! number of points in dimension 2
      nt_old(3) = n3       ! number of points in dimension 3
      it_old(1) = 1        ! increment between points in dimension 1
      it_old(2) = n1       ! increment between points in dimension 2
      it_old(3) = n1 * n2  ! increment between points in dimension 3

      n1 = nt_old(i_of_o(1))
      n2 = nt_old(i_of_o(2))
      n3 = nt_old(i_of_o(3))

      nt_new(1) = n1       ! number of points in dimension 1
      nt_new(2) = n2       ! number of points in dimension 2
      nt_new(3) = n3       ! number of points in dimension 3
      it_new(1) = 1        ! increment between points in dimension 1
      it_new(2) = n1       ! increment between points in dimension 2
      it_new(3) = n1 * n2  ! increment between points in dimension 3

c      write(6,'('' i1 i_of_o nt_old nt_old nt_new it_new'')')
c      write(6,'(7(1x,i2))')(i,i_of_o(i)
c     1,nt_old(i),it_old(i)
c     1,nt_new(i),it_new(i)
c     1,i=1,3)

      do j_old = 1 , n1*n2*n3

        i_old(3) = (j_old
     1                     - 1) / it_old(3) + 1
        i_old(2) = (j_old
     1                     - (i_old(3) - 1) * it_old(3)
     1                     - 1) / it_old(2) + 1
        i_old(1)  = (j_old
     1                     - (i_old(3) - 1) * it_old(3)
     1                     - (i_old(2) - 1) * it_old(2)
     1                     - 1) / it_old(1) + 1


        i_new(1) = i_old(i_of_o(1))
        i_new(2) = i_old(i_of_o(2))
        i_new(3) = i_old(i_of_o(3))

        j_new = 1
     1        + (i_new(1) - 1) * it_new(1)
     1        + (i_new(2) - 1) * it_new(2)
     1        + (i_new(3) - 1) * it_new(3)

        vel(j_new) = work(j_old)

c      write(88,'(
c     1 '' i_old='',i3,'' i_new='',i3,'' x='',i3
c     1,'' io='',i2,1x,i2,1x,i2
c     1,'' in='',i2,1x,i2,1x,i2
c     1)')
c     1,j_old,j_new,nint(vel(j_new))
c     1 i_old(1),i_old(2),i_old(3)
c     1,i_new(1),i_new(2),i_new(3)

      enddo    ! do j_old = 1 , n1*n2*n3

      crd80 = ' '
      write(crd80,'('' RMOD transposing dimensions:''
     1,i2,1x,i2,1x,i2)')i_of_o(1),i_of_o(2),i_of_o(3)
      call rmod_card_to_history(crd80)

      return
      end

c23456789012345678901234567890123456789012345678901234567890123456789012
      subroutine xrmod_transpose_grid(mod_type,n1,n2,n3,vel,m_work,work)
      implicit  none
      integer   util_len_r
      character mod_type*(*)
      integer   n1,n2,n3,m_work
      real      vel(n2,n1,n3),work(n1,n2,n3)

      character crd80*80
      integer   j_new,i_new(3)
      integer   j_old,i_old(3)
      integer   nt_new(3),it_new(3)
      integer   nt_old(3),it_old(3)
      integer   i_of_o(3)
      integer   i1,i2,i3

      print'(/,'' rmod_transpose_grid''
     1,/,'' This option transposes dimensions in a gridded model''
     1,/,'' Note this does not alter ''
     1,'' XMIN,XINC, YMIN,YINC, ZMIN,ZINC in the header file''
     1,/,'' To transpose from Gocad to Conoco use 3,1,2 ''
     1,''(x,y,z to z,x,y)''
     1,/,'' To transpose from Conoco to Gocad use 2,3,1 ''
     1,''(z,x,y to x,y,z)''
     1,/,'' n1='',i10
     1,'' first  (fastest) dimension Gocad - x Conoco - z''
     1,/,'' n2='',i10
     1,'' second (middle ) dimension Gocad - y Conoco - x''
     1,/,'' n3='',i10
     1,'' third  (slowest) dimension Gocad - z Conoco - y''
     1,/,'' n1*n2*n3='',i10,'' m_work='',i10)'
     1,n1,n2,n3,n1*n2*n3,m_work

      if (mod_type(1:4) .ne. 'GRID') then
         print'('' model mod_type should be GRID mod_type='',a)'
     1,mod_type(1:util_len_r(mod_type))
         return
      endif

      if (m_work .lt. n1*n2*n3) then
        print'('' insufficent space to copy velocity''
     1,/,'' n1='',i10,'' n2='',i10,'' n3='',i10
     1,'' n1*n2*n3='',i10,'' m_work='',i10)',n1,n2,n3,n1*n2*n3,m_work
        return
      endif    ! if (m_work .lt. n1*n2*n3) then
      call util_copy(n1*n2*n3,vel,work)
      print*,' transposing 1,2'
      do i3 = 1 , n3
      do i1 = 1 , n1
      if (mod(i1,10) .eq. 1) print*,' i1=',i1
      do i2 = 1 , n2
        vel(i2,i1,i3) = work(i1,i2,i3)
      enddo    ! do i2 = 1 , n2
      enddo    ! do i1 = 1 , n1
      enddo    ! do i3 = 1 , n3
      crd80 = ' '
      write(crd80,'('' RMOD transposing dimensions:''
     1,i2,1x,i2,1x,i2)')2,1,3
      call rmod_card_to_history(crd80)

      return
      end

c23456789012345678901234567890123456789012345678901234567890123456789012
      subroutine rmod_find_and_replace_values(mod_type,h_file
     1,nx,x0,dx,ny,y0,dy,nz,z0,dz,v)
      implicit  none
      integer   util_len_r
      real      util_invert_1
      character mod_type*(*)
      character h_file*(*)
      integer   nx
      real      x0,dx
      integer   ny
      real      y0,dy
      integer   nz
      real      z0,dz
      real      v(nx*ny*nz)

      character crd80*80,ans1*1,ans2*1,ans3*1
      integer   i_err,i_file
      integer   i,j,n,m
      integer   ix,iy,iz
      integer   jx,jy,jz
      integer   n_v0
      real      v0
      real      x,y,z
      real      a_min,a_max
      real      v_min,v_max

      print'(/,'' rmod_find_values''
     1,/,'' This option finds the lcoations of grid points''
     1,/,'' whose values falls between two values''
     1,/,'' and allows you to replace those nodes'')'

      if (mod_type(1:1) .ne. 'G') then
         print'('' model mod_type should be GRID or G3DL mod_type='',a)'
     1,mod_type(1:util_len_r(mod_type))
         goto 3
      endif

      if (mod_type(1:4) .ne. 'G3DL') call util_invert(nx*ny*nz,v)
      call util_min_max(v_min,v_max,nx*ny*nz,v)
      print'('' min and max values found='',f12.4,1x,f12.4)'
     1,v_min,v_max

    1 continue
      a_min = 0.
      a_max = 0.
      crd80 = ' '
      print'(/,'' enter search range default min='',g16.9
     1,'' max='',g16.9)',a_min,a_max
      read(*,'(a)',err=1,end=2)crd80
      if (crd80 .ne. ' ') read(crd80,*,err=1,end=2)a_min,a_max
    2 continue

c      print'('' search range min='',g16.9
c     1,'' max='',g16.9)',a_min,a_max

      n = 0
      do i = 1 , nx*ny*nz
        if (v(i) .ge. a_min .and. v(i) .le. a_max) n = n + 1
      enddo    ! do i = 1 , nx*ny*nz
      m = n

      print'('' there are '',i10,'' points out of a total of '',i10
     1,/,'' whose value falls within the range '',g16.9,'' - '',g16.9)'
     1,m,nx*ny*nz,a_min,a_max

      call rmod_ans(ans1,'Y','Y','N',' Write these values to a file?')
      if (ans1 .eq. 'Y') then
        call util_copy_file_name(h_file,h_file)
        call util_get_file_name(' Enter file name',h_file,'range')
        call rmodopen(i_file,h_file,'FORMATTED','NEW','SEQUENTIAL'
     1,'IEEE',0,i_err)
        if (i_err .ne. 0) goto 999
      endif    ! if (ans1 .eq. 'Y') then

      call rmod_ans(ans2,'Y','Y','N',' Replace these values?')
      if (ans2 .eq. 'Y') call rmod_ans(ans3,'Y','Y','N'
     1,' By hand? (otherwise average surrounding values)')

      if (ans1 .eq. 'Y' .or. ans2 .eq. 'Y') then

        n = 0
        do i = 1 , nx*ny*nz

          if (mod_type(1:4) .eq. 'G3DL') then
            iz = (i - 1) / (nx*ny) + 1
            iy = (i - (iz - 1) * (nx*ny) - 1) / nx + 1
            ix = mod(i-1,nx) + 1

          else
            iy = (i - 1) / (nz*nx) + 1
            ix = (i - (iy - 1) * (nz*nx) - 1) / nz + 1
            iz = mod(i-1,nz) + 1
          endif

          x = (ix - 1) * dx + x0
          y = (iy - 1) * dy + y0
          z = (iz - 1) * dz + z0

          if (v(i) .ge. a_min .and. v(i) .le. a_max) then
            n = n + 1

            if (mod_type(1:4) .eq. 'G3DL') then

c23456789012345678901234567890123456789012345678901234567890123456789012
              if (ans1 .eq. 'Y') write(i_file
     1,'(1x,f10.2,1x,f10.2,1x,12x,1x,f12.4,1x,i6,1x,i6,1x,i6,1x,i6)')
     1x,y,v(i),ix,iy,iz,n

              if (ans2 .eq. 'Y') then

                write(6,'(/
     1,'' found value '',i6,'' of '',i6
     1,'' at ix='',i6,'' iy='',i6,'' iz='',i6,'' i='',i10
     1,/,'' x='',f10.2,'' y='',f10.2,'' value='',f12.4)')
     1n,m,ix,iy,iz,i,x,y,v(i)

                 write(6,'(''  values in x direction'',5(1x,f12.4))')
     1(v((iz-1)*nx*ny+(iy-1)*nx+max(1,min(nx,jx))),jx=ix-2,ix+2)

                 write(6,'(''  values in y direction'',5(1x,f12.4))')
     1(v((iz-1)*nx*ny+(max(1,min(ny,jy))-1)*nx+ix),jy=iy-2,iy+2)

                 n_v0 = 0
                 v0 = 0.
                 do jy = max(1,iy-1) , min(ny,iy+1)
                   do jx = max(1,ix-1) , min(nx,ix+1)
                     j = (jz-1)*nx*ny+(jy-1)*nx+jx
                     if (i .ne. j
     1.and. (v(j) .lt. a_min .or. v(j) .gt. a_max)) then
                       v0 = v0 + v(j)
                       n_v0 = n_v0 + 1
                     endif
                   enddo    ! do jx = max(1,ix-1) , min(nx,ix+1)
                 enddo    ! do jy = max(1,iy-1) , min(ny,iy+1)
                 v0 = v0 / max(1,n_v0)

              endif    ! if (ans2 .eq. 'Y') then

            else    ! if (mod_type(1:4) .eq. 'G3DL') then

              if (ans1 .eq. 'Y') write(i_file
     1,'(1x,f10.2,1x,f10.2,1x,f12.4,1x,f12.4,1x,i6,1x,i6,1x,i6,1x,i6)')
     1x,y,z,v(i),ix,iy,iz,n

              if (ans2 .eq. 'Y') then

                write(6,'(/
     1,'' found value '',i6,'' of '',i6
     1,'' at ix='',i6,'' iy='',i6,'' iz='',i6,'' i='',i10
     1,/,'' x='',f10.2,'' y='',f10.2,'' z='',f12.4,'' value='',f12.4)')
     1n,m,ix,iy,iz,i,x,y,z,v(i)

                 write(6,'(''  x direction'',5(1x,f12.4))')
     1(v((iy-1)*nz*nx+(max(1,min(nx,jx))-1)*nz+iz),jx=ix-2,ix+2)

                 write(6,'(''  y direction'',5(1x,f12.4))')
     1(v((max(1,min(ny,jy))-1)*nz*nx+(ix-1)*nz+iz),jy=iy-2,iy+2)

                 write(6,'(''  z direction'',5(1x,f12.4))')
     1(v((iy-1)*nz*nx+(ix-1)*nz+max(1,min(nz,jz))),jz=iz-2,iz+2)

                 n_v0 = 0
                 v0 = 0.
                 do jz = max(1,iz-1) , min(nz,iz+1)
                   do jy = max(1,iy-1) , min(ny,iy+1)
                     do jx = max(1,ix-1) , min(nx,ix+1)
                       j = (jy-1)*nz*nx+(jx-1)*nz+jz
                       if (i .ne. j
     1.and. (v(j) .lt. a_min .or. v(j) .gt. a_max)) then
                         v0 = v0 + util_invert_1(v(j))
                         n_v0 = n_v0 + 1
                       endif
                     enddo    ! do jx = max(1,ix-1) , min(nx,ix+1)
                   enddo    ! do jy = max(1,iy-1) , min(ny,iy+1)
                 enddo    ! do jz = max(1,iz-1) , min(nz,iz+1)
                 v0 = util_invert_1(v0/max(1,n_v0))
              endif    ! if (ans2 .eq. 'Y') then

            endif    ! if (mod_type(1:4) .eq. 'G3DL') then

            if (n_v0 .eq. 0)
     1print'('' could not find any values surrounding''
     1,'' ix='',i6,'' iy='',i6,'' iz='',i6)',ix,iy,iz
            if (ans2 .eq. 'Y') then

              if (ans3 .eq. 'Y') call rmod_read_float(v0,v0
     1,' enter the new value')

              write(6,'('' replaceing '',i6,'' of '',i6
     1,'' old='',f12.4,'' new='',f12.4)')n,m,v(i),v0
              v(i) = v0

            endif    ! if (ans2 .eq. 'Y') then

          endif    ! if (v(i) .ge. a_min

        enddo    ! do i = 1 , nx*ny*nz

      endif    ! if (ans1 .eq. 'Y') then

    3 continue
      if (mod_type(1:4) .ne. 'G3DL') call util_invert(nx*ny*nz,v)

      return

  999 continue
      print'(/,'' error during open'')'
      if (mod_type(1:4) .ne. 'G3DL') call util_invert(nx*ny*nz,v)
      return

      end

c23456789012345678901234567890123456789012345678901234567890123456789012
      subroutine rmod_shift_model(h_file,mod_type
     1,cxi,cyi,czi
     1,m_cord,n_cord,x_cord,cord
     1,nb,ixb,nxb,xb,yb,zb
     1,nv,ixv,nxv,xv,yv,zv
     1,ncv,xcv,ycv,zcv
     1,m_vel
     1,nx_vel,x0_vel,dx_vel
     1,ny_vel,y0_vel,dy_vel
     1,nz_vel,z0_vel,dz_vel
     1,vel,zel
     1,m_work,work)
      implicit none

      integer   util_len_r
      real      util_invert_1

      character h_file*(*),mod_type*(*),crd80*80
      character cxi*(*),cyi*(*),czi*(*)
      character cord(*)*(*)

      integer   m_cord,n_cord
      real      x_cord

      integer   nb
      integer   ixb(1),nxb(1)
      real      xb(1),yb(1),zb(1)

      integer   nv
      integer   ixv(1),nxv(1)
      real      xv(1),yv(1),zv(1)

      integer   ncv
      real      xcv(1),ycv(1),zcv(1)

      integer   m_vel

      integer   nx_vel
      real      x0_vel,dx_vel

      integer   ny_vel
      real      y0_vel,dy_vel

      integer   nz_vel
      real      z0_vel,dz_vel

      real      vel(1),zel(1)

      integer   m_work
      real      work(1)

      integer   i_err

      integer   nx_inp
      real      x0_inp,dx_inp

      integer   ny_inp
      real      y0_inp,dy_inp

      integer   nz_inp
      real      z0_inp,dz_inp

      integer   nx_out
      real      x0_out,dx_out

      integer   ny_out
      real      y0_out,dy_out

      integer   nz_out
      real      z0_out,dz_out

      integer   i_z_inp,i_z_out,i_zel

      integer   ix_vel,iy_vel,iz_vel,lu
      real      x_vel, y_vel, z_vel
      real      x_min,x_max,y_min,y_max,z_min,z_max,z_datum
      real      z_inp_min,z_inp_max
      real      z_out_min,z_out_max
      real      v_replace,v_replace_default
      integer   iv_replace ! 0 no replace =/ 0 replace

      character cxv*16,cyv*16,czv*16,mod_type_tmp*16
      character h_file_1*80,h_file_2*80
      character u_none*4

      i_err = 0

      if (mod_type .ne. 'LAYER' .and. mod_type .ne. 'G3DL'
     1.and. mod_type .ne. 'GRID') then
        print'('' Model mod_type must be LAYER, G3DL or GRID''
     1,'' mod_type='',a16)',mod_type
        call rmod_pause(' ',' ')
        return
      endif

c23456789012345678901234567890123456789012345678901234567890123456789012
      print'(
     1 /,'' This option verticaly shifts a layered or gridded model by''
     1,/,'' an amount defined by the difference in 2 HG3DL files''
     1,/,'' you will be asked for the names of the two datum files''
     1,/,'' value output = input + (first - second)''
     1,/,'' Note this will use only the first horizon in the G3DL file''
     1,/,'' Note if you use NONE as either G3DL file that file is not''
     1,/,'' read, instead the depths are set to zero.''
     1)'

c  get the replacement value
      if (mod_type .ne. 'GRID') then

        print'(
     1 /,'' This is a layered model''
     1,/,'' the horizons, velocity horizons and cell pointers ''
     1,/,'' all get shifted. ''
     1)'

      else    ! if (mod_type .ne. 'GRID') then

        print'(
     1 /,'' This is a gridded velocity model.''
     1,/,'' You will be asked for a replacement value for the grid ''
     1,/,'' values above the shift horizon.''
     1,/,'' If you take the default the replacement value will be ''
     1,/,'' that of the first node at each x,y location of the ''
     1,/,'' original grid file''
     1,/,'' For gridded velocity models the following is done: ''
     1,/
     1,/,'' 1 Each velocity trace is inteprolated to a finer''
     1,'' vertical grid ''
     1,/
     1,/,'' 2 the fine velocity trace is shifted by the datum''
     1,'' correction ''
     1,/
     1,/,'' 3 if requested the velocity nodes above the output''
     1,/,'' datum level are set to the repolacement velocity ''
     1,/
     1,/,'' 4 - the final velocity trace with the original coarse''
     1,/,'' spacing is constructed from the finely sampled,''
     1,/,'' shifted velocity grid. Each output sample is the''
     1,/,'' average of the fine points about it''
     1,/)'
        v_replace_default = -999.

        call rmod_get_real('replacement velocity'
     1,v_replace_default,v_replace)

        if (abs(v_replace-v_replace_default) .ge. 1.e-3) then

          iv_replace = 1    ! replace
          v_replace = util_invert_1(v_replace)

        else    ! if (abs(v_replace-v_replace_default) .ge. 1.e-3) then

          iv_replace = 0    ! no replace

        endif    ! if (abs(v_replace-v_replace_default) .ge. 1.e-3) then

      endif    ! if (mod_type .ne. 'GRID') then


c  read the gridded velocity - note vel is used for gridded vleocity
      lu = -1

      i_z_inp = 1
      call util_copy_file_name(h_file,h_file_1)
      print'(/,'' enter NONE to set this horizon depth to zero'')'
      call util_get_file_name(' Enter the first HG3DL file name'
     1,h_file_1,'hg3dl')

      u_none = h_file_1(1:4)
      call util_caps(u_none,u_none)

      if (u_none(1:4) .eq. 'NONE') then

        nx_inp = 1
        x0_inp = 0.
        dx_inp = 1.

        ny_inp = 1
        y0_inp = 0.
        dy_inp = 1.

        nz_inp = 1
        z0_inp = 0.
        dz_inp = 1.

        zel(i_z_inp) = 0.

      else    ! if (u_none(1:4) .eq. 'NONE') then

      call rmodrgrd(h_file_1,mod_type_tmp
     1,cxv,cyv,czv
     1,cxi,cyi,czi
     1,m_cord,n_cord,x_cord,cord
     1,x_min,x_max
     1,y_min,y_max
     1,z_min,z_max
     1,z_datum
     1,m_vel
     1,nx_inp,x0_inp,dx_inp
     1,ny_inp,y0_inp,dy_inp
     1,nz_inp,z0_inp,dz_inp
     1,zel(i_z_inp)
     1,m_work,work
     1,lu,i_err)
      if (i_err .ne. 0) goto 999

      if (mod_type_tmp(1:4) .ne. 'G3DL') then

        print'('' This must be a G3DL grid file - mod_type_tmp:'',a)'
     1,mod_type_tmp(1:util_len_r(mod_type_tmp))

          i_err = -1
          goto 999

      endif

      endif    ! if (u_none(1:4) .eq. 'NONE') then

c  get the second file name
      i_z_out = i_z_inp + nx_inp * ny_inp
      call util_copy_file_name(h_file,h_file_2)
      print'(/,'' enter NONE to set this horizon depth to zero'')'
      call util_get_file_name(' Enter the second HG3DL file name'
     1,h_file_2,'hg3dl')

      u_none = h_file_2(1:4)
      call util_caps(u_none,u_none)

      if (u_none(1:4) .eq. 'NONE') then

        nx_out = 1
        x0_out = 0.
        dx_out = 1.

        ny_out = 1
        y0_out = 0.
        dy_out = 1.

        nz_out = 1
        z0_out = 0.
        dz_out = 1.

        zel(i_z_out) = 0.

      else    ! if (u_none(1:4) .eq. 'NONE') then

      call rmodrgrd(h_file_2,mod_type_tmp
     1,cxv,cyv,czv
     1,cxi,cyi,czi
     1,m_cord,n_cord,x_cord,cord
     1,x_min,x_max
     1,y_min,y_max
     1,z_min,z_max
     1,z_datum
     1,m_vel-i_z_out+1
     1,nx_out,x0_out,dx_out
     1,ny_out,y0_out,dy_out
     1,nz_out,z0_out,dz_out
     1,zel(i_z_out)
     1,m_work,work
     1,lu,i_err)
      if (i_err .ne. 0) goto 999

      if (mod_type_tmp(1:4) .ne. 'G3DL') then

        print'('' This must be a G3DL grid file - mod_type_tmp:'',a)'
     1,mod_type_tmp(1:util_len_r(mod_type_tmp))

          i_err = -1
          goto 999

      endif

      endif    ! if (u_none(1:4) .eq. 'NONE') then

      call util_min_max(z_inp_min,z_inp_max,nx_inp*ny_inp,zel(i_z_inp))
      call util_min_max(z_out_min,z_out_max,nx_out*ny_out,zel(i_z_out))

      print'(/,'' hg3dl file grid characteristics''
     1,/,'' nx_inp='',i5,'' x0_inp='',f10.2,'' dx_inp='',f10.4
     1,/,'' ny_inp='',i5,'' y0_inp='',f10.2,'' dy_inp='',f10.4
     1,/,'' z1_inp='',f10.2,'' z2_inp='',f10.2
     1,/,'' nx_out='',i5,'' x0_out='',f10.2,'' dx_out='',f10.4
     1,/,'' ny_out='',i5,'' y0_out='',f10.2,'' dy_out='',f10.4
     1,/,'' z1_out='',f10.2,'' z2_out='',f10.2
     1)'
     1,nx_inp,x0_inp,dx_inp
     1,ny_inp,y0_inp,dy_inp
     1,z_inp_min,z_inp_max
     1,nx_out,x0_out,dx_out
     1,ny_out,y0_out,dy_out
     1,z_out_min,z_out_max

c  shift gridded files
      if (mod_type .eq.'GRID') then

        i_zel = i_z_out + nx_out*ny_out

        call rmod_shift_grid(
     1 iv_replace,v_replace
     1,m_vel-i_zel+1
     1,nx_vel,x0_vel,dx_vel
     1,ny_vel,y0_vel,dy_vel
     1,nz_vel,z0_vel,dz_vel
     1,vel,zel(i_zel)
     1,nx_inp,x0_inp,dx_inp
     1,ny_inp,y0_inp,dy_inp
     1,zel(i_z_inp)
     1,nx_out,x0_out,dx_out
     1,ny_out,y0_out,dy_out
     1,zel(i_z_out)
     1,m_work,work
     1,i_err
     1)
        if (i_err .ne. 0) goto 999

      elseif (mod_type(1:4) .eq. 'G3DL') then ! if (mod_type .eq.'GRID') then
c  shift hg3dl files

        do iz_vel = 1 , nz_vel

          do iy_vel = 1 , ny_vel

            y_vel = (iy_vel - 1) * dy_vel + y0_vel

            do ix_vel = 1 , nx_vel

              x_vel = (ix_vel - 1) * dx_vel + x0_vel

              call rmod_shift_layer_n(
     1 1,x_vel,y_vel
     1,vel((iz_vel)-1*nx_vel*ny_vel+(iy_vel-1)*nx_vel+ix_vel)
     1,nx_inp,x0_inp,dx_inp
     1,ny_inp,y0_inp,dy_inp
     1,zel(i_z_inp)
     1,nx_out,x0_out,dx_out
     1,ny_out,y0_out,dy_out
     1,zel(i_z_out)
     1)

            enddo    ! do ix_vel = 1 , nx_vel

          enddo    ! do iy_vel = 1 , ny_vel

        enddo    ! do iz_vel = 1 , nz_vel

c  shift cell pointers
        call rmod_shift_layer_n(
     1 ncv,xcv,ycv,zcv
     1,nx_inp,x0_inp,dx_inp
     1,ny_inp,y0_inp,dy_inp
     1,zel(i_z_inp)
     1,nx_out,x0_out,dx_out
     1,ny_out,y0_out,dy_out
     1,zel(i_z_out)
     1)

c  shift velocity boundaries
        call rmod_shift_layer_n(
     1 ixv(nv)+nxv(nv),xv,yv,zv
     1,nx_inp,x0_inp,dx_inp
     1,ny_inp,y0_inp,dy_inp
     1,zel(i_z_inp)
     1,nx_out,x0_out,dx_out
     1,ny_out,y0_out,dy_out
     1,zel(i_z_out)
     1)

      else    ! if (mod_type(1:4) .eq. 'GRID') then

c  shift horizons
        call util_setr(ixb(nb)+nxb(nb),yb,y0_vel)
        call util_setr(ncv,ycv,y0_vel)
        call util_setr(ixv(nv)+nxv(nv),yv,y0_vel)
        call rmod_shift_layer_n(
     1 ixb(nb)+nxb(nb),xb,work,zb
     1,nx_inp,x0_inp,dx_inp
     1,ny_inp,y0_inp,dy_inp
     1,zel(i_z_inp)
     1,nx_out,x0_out,dx_out
     1,ny_out,y0_out,dy_out
     1,zel(i_z_out)
     1)

c  shift cell pointers
        call rmod_shift_layer_n(
     1 ncv,xcv,ycv,zcv
     1,nx_inp,x0_inp,dx_inp
     1,ny_inp,y0_inp,dy_inp
     1,zel(i_z_inp)
     1,nx_out,x0_out,dx_out
     1,ny_out,y0_out,dy_out
     1,zel(i_z_out)
     1)

c  shift velocity boundaries
        call rmod_shift_layer_n(
     1 ixv(nv)+nxv(nv),xv,yv,zv
     1,nx_inp,x0_inp,dx_inp
     1,ny_inp,y0_inp,dy_inp
     1,zel(i_z_inp)
     1,nx_out,x0_out,dx_out
     1,ny_out,y0_out,dy_out
     1,zel(i_z_out)
     1)

      endif    ! if (mod_type(1:4) .eq. 'G3DL') then


      crd80 = ' '
      write(crd80,'(a)')
     1' RMOD shifting layered model using G3DL files'
      call rmod_title_to_history(crd80)
      crd80 = ' '
      write(crd80,'('' file1:'',a)')h_file_1(1:util_len_r(h_file_1))
      call rmod_card_to_history(crd80)
      crd80 = ' '
      write(crd80,'('' file2:'',a)')h_file_2(1:util_len_r(h_file_2))
      call rmod_card_to_history(crd80)

      return

  999 continue
      print'(''errror in rmod_shift_model'')'
      return

      end

c23456789012345678901234567890123456789012345678901234567890123456789012
      subroutine rmod_shift_layer_n(
     1 n_lay,x_lay,y_lay,z_lay
     1,nx_inp,x0_inp,dx_inp
     1,ny_inp,y0_inp,dy_inp
     1,z_inp
     1,nx_out,x0_out,dx_out
     1,ny_out,y0_out,dy_out
     1,z_out
     1)
      implicit none

      integer   n_lay
      real      x_lay(1),y_lay(1),z_lay(1)

      integer   nx_inp
      real      x0_inp,dx_inp

      integer   ny_inp
      real      y0_inp,dy_inp

      real      z_inp(nx_inp,ny_inp)

      integer   nx_out
      real      x0_out,dx_out

      integer   ny_out
      real      y0_out,dy_out

      real      z_out(nx_out,ny_out)

      integer   i_lay
      real      z_lay_inp,z_lay_out

      do i_lay = 1 , n_lay

        call util_interpolate_2d(
     1 nx_inp,x0_inp,dx_inp
     1,ny_inp,y0_inp,dy_inp
     1,z_inp
     1,1,x_lay(i_lay),dx_inp
     1,1,y_lay(i_lay),dy_inp
     1,z_lay_inp
     1)

        call util_interpolate_2d(
     1 nx_out,x0_out,dx_out
     1,ny_out,y0_out,dy_out
     1,z_out
     1,1,x_lay(i_lay),dx_out
     1,1,y_lay(i_lay),dy_out
     1,z_lay_out
     1)

        z_lay(i_lay) = z_lay(i_lay) + (z_lay_inp - z_lay_out)

      enddo    ! do i_lay = 1 , n_lay

      return
      end

c23456789012345678901234567890123456789012345678901234567890123456789012
      subroutine rmod_shift_grid(
     1 iv_replace,v_replace
     1,m_vel
     1,nx_vel,x0_vel,dx_vel
     1,ny_vel,y0_vel,dy_vel
     1,nz_vel,z0_vel,dz_vel
     1,vel,zel
     1,nx_inp,x0_inp,dx_inp
     1,ny_inp,y0_inp,dy_inp
     1,z_inp
     1,nx_out,x0_out,dx_out
     1,ny_out,y0_out,dy_out
     1,z_out
     1,m_work,work
     1,i_err
     1)
      implicit none

      real     util_invert_1

      integer   iv_replace
      real      v_replace

      integer   m_vel

      integer   nx_vel
      real      x0_vel,dx_vel

      integer   ny_vel
      real      y0_vel,dy_vel

      integer   nz_vel
      real      z0_vel,dz_vel

      real      vel(1),zel(1)

      integer   nx_inp
      real      x0_inp,dx_inp

      integer   ny_inp
      real      y0_inp,dy_inp

      real      z_inp(nx_inp,ny_inp)

      integer   nx_out
      real      x0_out,dx_out

      integer   ny_out
      real      y0_out,dy_out

      real      z_out(nx_out,ny_out)
      integer   m_work
      real      work(1)

      integer   i_err

      integer   ix_vel,iy_vel,iz_vel

      real       x_vel, y_vel, z_vel
      real      z_vel_inp,z_vel_out,dz,dz_min,dz_max
      integer   nz_zel
      real      z0_zel
      real      z1_vel,z2_vel
      real      z1_zel,z2_zel

      integer   n1,n2,n3
      real      r1,r2,r3

      integer  nz_replace
      integer  nz_vel_fine,nz_fine
      real     dz_vel_fine
      integer  i_vel_fine,n_vel_fine
      integer  i_vel,i_zel
      integer  i_work,n_work
      integer  i_work_i,i_work_n

      character crd80*80

      i_err = 0

c  get the min and max shift
      do iy_vel = 1 , ny_vel

        y_vel = (iy_vel - 1) * dy_vel + y0_vel

        do ix_vel = 1 , nx_vel

          x_vel = (ix_vel - 1) * dx_vel + x0_vel

c  interpolate the static shift
        call util_interpolate_2d(
     1 nx_inp,x0_inp,dx_inp
     1,ny_inp,y0_inp,dy_inp
     1,z_inp
     1,1,x_vel,dx_vel
     1,1,y_vel,dy_vel
     1,z_vel_inp
     1)

        call util_interpolate_2d(
     1 nx_out,x0_out,dx_out
     1,ny_out,y0_out,dy_out
     1,z_out
     1,1,x_vel,dx_vel
     1,1,y_vel,dy_vel
     1,z_vel_out
     1)

          dz = z_vel_inp - z_vel_out

          if (ix_vel .eq. 1 .and. iy_vel .eq. 1) then

            dz_min = dz
            dz_max = dz

          else    ! if (ix_vel .eq. 1 .and. iy_vel .eq. 1) then

            dz_min = min(dz_min,dz)
            dz_max = max(dz_max,dz)

          endif    ! if (ix_vel .eq. 1 .and. iy_vel .eq. 1) then

        enddo    ! do ix_vel = 1 , nx_vel

      enddo    ! do iy_vel = 1 , ny_vel

c  determine the origin and length of the shifted grid file
      nz_zel = nz_vel
      z0_zel = z0_vel
      z1_zel = z0_zel
      z2_zel = (nz_zel - 1) * dz_vel + z0_zel

      z1_vel = nint(min(z1_zel,dz_min)/dz_vel) * dz_vel
      if (z1_vel .gt. min(z1_zel,dz_min)) z1_vel = z1_vel - dz_vel

      z2_vel = nint(max(z2_zel,z2_zel+dz_max)/dz_vel) * dz_vel
      if (z2_vel .lt. min(z2_zel,z2_zel+dz_max))
     1z2_vel = z2_vel + dz_vel

      if (nx_vel*ny_vel*nz_vel .gt. m_vel) then

        nz_vel = m_vel / (nx_vel*ny_vel)
        print'('' new model depth is greater than the allocated space''
     1,/,'' reducing the model depth nz_vel='',i8)',nz_vel

      endif    ! if (nx_vel*ny_vel*nz_vel .gt. m_vel) then
c23456789012345678901234567890123456789012345678901234567890123456789012

      nz_vel = nint((z2_vel-z1_vel)/dz_vel) + 1
      z0_vel = z1_vel

      nz_fine = 11
      nz_vel_fine = (nz_vel - 1) * nz_fine + 1
      dz_vel_fine = dz_vel / nz_fine

      n_vel_fine = nz_vel_fine

      call util_wors(i_work_i,i_work_n,m_work)
      call util_work(i_work_i,i_work_n,i_vel_fine,n_vel_fine)
      call util_worc(i_work_i,i_work_n,i_err)
      if (i_err .ne. 0) goto 999

      print'(/,'' 3d grid characteristics''
     1,/,'' nx_vel='',i5,'' x0_vel='',f10.2,'' dx_vel='',f10.4
     1,/,'' ny_vel='',i5,'' y0_vel='',f10.2,'' dy_vel='',f10.4
     1,/,'' original nz_vel='',i5,'' z0_vel='',f10.2
     1,'' dz_vel='',f10.4
     1,/,'' revised  nz_vel='',i5,'' z0_vel='',f10.2
     1,'' dz_vel='',f10.4)'
     1,nx_vel,x0_vel,dx_vel
     1,ny_vel,y0_vel,dy_vel
     1,nz_zel,z0_zel,dz_vel
     1,nz_vel,z0_vel,dz_vel

      n1 = 2
      r1 = dz_vel
      n2 = 0
      r2 = 0.
      n3 = 0
      r3 = 0.
      call util_copy(nx_vel*ny_vel*nz_vel,vel,zel)

      i_zel = 1
      i_vel = 1

c  compute the new velocity trace
      do iy_vel = 1 , ny_vel

          y_vel = (iy_vel - 1) * dy_vel + y0_vel

          do ix_vel = 1 , nx_vel

            x_vel = (ix_vel - 1) * dx_vel + x0_vel

c  interpolate the static shift
          call util_interpolate_2d(
     1 nx_inp,x0_inp,dx_inp
     1,ny_inp,y0_inp,dy_inp
     1,z_inp
     1,1,x_vel,dx_vel
     1,1,y_vel,dy_vel
     1,z_vel_inp
     1)

          call util_interpolate_2d(
     1 nx_out,x0_out,dx_out
     1,ny_out,y0_out,dy_out
     1,z_out
     1,1,x_vel,dx_vel
     1,1,y_vel,dy_vel
     1,z_vel_out
     1)

          z_vel = z0_zel + z_vel_inp - z_vel_out

c  interpolate the input velocity grid to a finer grid
          call util_interpolate_1d(
     1 nz_zel     ,z_vel,dz_vel
     1,zel(i_zel)
     1,nz_vel_fine,z0_vel,dz_vel_fine
     1,work(i_vel_fine)
     1)

c  if need be replace the replacement values
           if (iv_replace .eq. 0) then

             nz_replace = 0

           else    ! if (iv_replace .eq. 0) then

             nz_replace = max(0,min(nz_vel_fine-1
     1,int((z_vel-z0_vel)/dz_vel_fine)))
             call util_setr(nz_replace,work(i_vel_fine),v_replace)

c      print'('' ix='',i6,'' iz='',i6,'' z='',f10.2
c     1,'' zinp='',f10.2,'' z_out='',f10.2,'' v_rep='',f10.2)'
c     1,ix_vel,nz_replace,z_vel,z_vel_inp,z_vel_out
c     1,1./v_replace

           endif    ! if (iv_replace .eq. 0) then

c  sum from the fine grid to the original coarse grid
            call util_ave_sum(1.,nz_vel_fine,nz_fine
     1,work(i_vel_fine),vel(i_vel))

c          call rmod_smooth_grid_0(n1,r1,n2,r2,n3,r3
c     1,nz_zel, z_vel,dz_vel,1,0.,1.,1,0.,1.,zel(i_zel)
c     1,nz_vel,z0_vel,dz_vel,1,0.,1.,1,0.,1.,vel(i_vel)
c     1,m_work,work,i_err)
c          if (i_err .ne. 0) goto 999

      if ( ix_vel .eq.      1 .and. iy_vel .eq.      1
     1.or. ix_vel .eq. nx_vel .and. iy_vel .eq. ny_vel)
     1print'('' ix='',i6,'' iy='',i6,'' iz='',i6,'' z='',f10.2
     1,'' vel='',f10.2,1x,f10.2)'
     1,ix_vel,iy_vel,nz_replace,z_vel
     1,util_invert_1(vel(i_vel))
     1,util_invert_1(vel(i_vel+nz_vel))

          i_zel = i_zel + nz_zel
          i_vel = i_vel + nz_vel

        enddo    ! do ix_vel = 1 , nx_vel

      enddo    ! do iy_vel = 1 , ny_vel

      crd80 = ' '
      write(crd80
     1,'('' original nz_vel:'',i5,'' z0_vel:'',f10.2
     1,'' dz_vel:'',f10.4)')nz_zel,z0_zel,dz_vel
      call rmod_card_to_history(crd80)
      crd80 = ' '
      write(crd80
     1,'('' revised  nz_vel:'',i5,'' z0_vel:'',f10.2
     1,'' dz_vel:'',f10.4)')nz_vel,z0_vel,dz_vel
      call rmod_card_to_history(crd80)

      return

  999 continue
      print'(''errror in rmod_shift_grid'')'
      i_err = -1
      return

      end

c23456789012345678901234567890123456789012345678901234567890123456789012
      subroutine rmod_operate_on_grid(h_file,mod_type
     1,cxi,cyi,czi,m_cord,n_cord,x_cord,cord
     1,m_vel
     1,nx_vel,x0_vel,dx_vel
     1,ny_vel,y0_vel,dy_vel
     1,nz_vel,z0_vel,dz_vel
     1,vel,zel
     1,m_work,work)
c  modify a grid in a number of different modes
      implicit none

      integer   util_len_r

      character h_file*(*),mod_type*(*),crd80*80
      character cxi*(*),cyi*(*),czi*(*)
      character cord(*)*(*)
      integer   m_cord,n_cord
      real      x_cord

      integer   m_vel
      integer   nx_vel
      real      x0_vel,dx_vel
      integer   ny_vel
      real      y0_vel,dy_vel
      integer   nz_vel
      real      z0_vel,dz_vel
      real      vel(1),zel(1)
      integer   m_work
      real      work(1)

      integer   i_err

      character h_file0*80
      integer   i_option,i
      integer   ix_1,ix_2,ix_inc
      integer   iy_1,iy_2,iy_inc
      integer   iz_1,iz_2,iz_inc
      real      c1,s1,e1,e2,s3,e3

      if (mod_type(1:1) .ne. 'G') then
        print'('' Model mod_type must be GRID or G3DL''
     1,'' mod_type='',a16)',mod_type
        call rmod_pause(' ',' ')
        return
      endif

c23456789012345678901234567890123456789012345678901234567890123456789012
      print'(/,'' This option operates on a grid file''
     1,/,'' It will allow you to modify the grid value via''
     1,'' an equation of the form''
     1,/,'' new grid = c1 + s1 * old grid**e1 * temp grid**e2''
     1,'' + s3 * temp grid**e3''
     1,/,'' temp grid will be a grid that you may input''
     1,'' if e2 or s3 are nonzero''
     1,/,'' The original grid values are operated on.''
     1,/,'' (such as velocity and not slowness)''
     1)'

    1 continue
      c1 = 0.
      s1 = 1.
      e1 = 1.
      e2 = 0.
      s3 = 0.
      e3 = 1.

      crd80 = ' '
      print'(/,'' enter c1='',f10.2
     1,'' s1='',f10.2,'' e1='',f10.2
     1,/,'' e2='',f10.2
     1,'' s3='',f10.2,'' e3='',f10.2)'
     1,c1,s1,e1,e2,s3,e3
      read(*,'(a)',err=1,end=2)crd80
      if (crd80 .ne. ' ') read(crd80,*,err=1,end=2)
     1c1,s1,e1,e2,s3,e3
    2 continue

      call util_setr(nx_vel*ny_vel*nz_vel,zel,1.)

      if (e2 .ne. 0. .or. s3 .ne. 0.)
     1call rmod_operate_read_grid(h_file,h_file0,mod_type
     1,cxi,cyi,czi,m_cord,n_cord,x_cord,cord
     1,m_vel
     1,nx_vel,x0_vel,dx_vel
     1,ny_vel,y0_vel,dy_vel
     1,nz_vel,z0_vel,dz_vel
     1,vel,zel
     1,m_work,work)

      call rmod_write_grid_to_ascii1('x',nx_vel,ix_1,ix_2,ix_inc)
      call rmod_write_grid_to_ascii1('y',ny_vel,iy_1,iy_2,iy_inc)
      call rmod_write_grid_to_ascii1('z',nz_vel,iz_1,iz_2,iz_inc)

      if (mod_type(1:4) .ne. 'G3DL') then
        call util_invert(nx_vel*ny_vel*nz_vel,vel)
        call util_invert(nx_vel*ny_vel*nz_vel,zel)
      endif    ! if (mod_type(1:4) .ne. 'G3DL') then

      print*,' before operation'
      print*,' first and last input vel='
     1,vel(1),vel(nx_vel*ny_vel*nz_vel)
      print*,' first and last temp  zel='
     1,zel(1),zel(nx_vel*ny_vel*nz_vel)

      if (mod_type .eq. 'G3DL') then

        call rmod_operate_on_grid_1(
     1 c1,s1,e1,e2,s3,e3
     1,nx_vel,ix_1,ix_2,ix_inc
     1,ny_vel,iy_1,iy_2,iy_inc
     1,nz_vel,iz_1,iz_2,iz_inc
     1,vel,zel
     1)

      else    ! if (mod_type .eq. 'G3DL') then

        call rmod_operate_on_grid_1(
     1 c1,s1,e1,e2,s3,e3
     1,nz_vel,iz_1,iz_2,iz_inc
     1,nx_vel,ix_1,ix_2,ix_inc
     1,ny_vel,iy_1,iy_2,iy_inc
     1,vel,zel
     1)

      endif    ! if (mod_type .eq. 'G3DL') then

      print*,' after operation'
      print*,' first and last output vel='
     1,vel(1),vel(nx_vel*ny_vel*nz_vel)

c  convert gridded velocities back to slowness
      if (mod_type(1:4) .ne. 'G3DL') then

        call util_invert(nx_vel*ny_vel*nz_vel,vel)
        call util_invert(nx_vel*ny_vel*nz_vel,zel)
        print*,' after inversion'
        print*,' first and last output vel='
     1,vel(1),vel(nx_vel*ny_vel*nz_vel)

      endif    ! if (mod_type(1:4) .ne. 'G3DL') then

      crd80 = ' '
      write(crd80,'(a)')' RMOD operating on grid'
      call rmod_title_to_history(crd80)

      if (e2 .ne. 0. .or. s3 .ne. 0.) then
        crd80 = ' '
        write(crd80,'('' file:'',a)')h_file0(1:util_len_r(h_file0))
        call rmod_card_to_history(crd80)
      endif    ! if (i_option .ge. 5 .and. i_option .le. 8) then

      crd80 = ' '
      write(crd80,'(
     1'' new grid : c1 + s1 * old grid**e1 * temp grid**e2''
     1,'' + s3 * temp grid**e3'')')
      call rmod_card_to_history(crd80)

      crd80 = ' '
      write(crd80,'('' c1:'',f10.2
     1,'' s1:'',f10.2,'' e1:'',f10.2)')
     1c1,s1,e1
      call rmod_card_to_history(crd80)

      crd80 = ' '
      write(crd80,'('' e2:'',f10.2
     1,'' s3:'',f10.2,'' e3:'',f10.2)')
     1e2,s3,e3
      call rmod_card_to_history(crd80)

      crd80 = ' '
      write(crd80,'('' ix_1:'',i8,'' ix_2:'',i8,'' ix_inc:'',i8)')
     1ix_1,ix_2,ix_inc
      call rmod_card_to_history(crd80)

      crd80 = ' '
      write(crd80,'('' iy_1:'',i8,'' iy_2:'',i8,'' iy_inc:'',i8)')
     1iy_1,iy_2,iy_inc
      call rmod_card_to_history(crd80)

      crd80 = ' '
      write(crd80,'('' iz_1:'',i8,'' iz_2:'',i8,'' iz_inc:'',i8)')
     1iz_1,iz_2,iz_inc
      call rmod_card_to_history(crd80)

      return

  999 continue
      print'(''errror in rmod_operate_on_grid'')'
      return

      end

c23456789012345678901234567890123456789012345678901234567890123456789012
      subroutine rmod_operate_on_grid_1(
     1 c1,s1,e1,e2,s3,e3
     1,n1,i1_a,i1_b,i1_c
     1,n2,i2_a,i2_b,i2_c
     1,n3,i3_a,i3_b,i3_c
     1,x,y
     1)
      implicit none
      real     c1,s1,e1,e2,s3,e3
      integer  n1,i1_a,i1_b,i1_c
      integer  n2,i2_a,i2_b,i2_c
      integer  n3,i3_a,i3_b,i3_c
      real     x(n1,n2,n3)
      real     y(n1,n2,n3)

      integer  i1,i2,i3
      real     f1,f2,f3

      print*,' c1=',c1,' s1=',s1,' e1=',e1,' e2=',e2
     1,' s3=',s3,' e3=',e3

      do i3 = i3_a , i3_b , i3_c

        do i2 = i2_a , i2_b , i2_c

          do i1 = i1_a , i1_b , i1_c

            if (e1 .eq. 0.) then
              f1 = 1.
            elseif (e1 .eq. 1.) then    ! if (e1 .eq. 0.) then
              f1 = x(i1,i2,i3)
            else    ! if (e1 .eq. 0.) then
              f1 = x(i1,i2,i3) ** e1
            endif    ! if (e1 .eq. 0.) then

            if (e2 .eq. 0.) then
              f2 = 1.
            elseif (e2 .eq. 1.) then    ! if (e2 .eq. 0.) then
              f2 = y(i1,i2,i3)
            else    ! if (e2 .eq. 0.) then
              f2 = y(i1,i2,i3) ** e2
            endif    ! if (e2 .eq. 0.) then

            if (e3 .eq. 0.) then
              f3 = 1.
            elseif (e3 .eq. 1.) then    ! if (e3 .eq. 0.) then
              f3 = y(i1,i2,i3)
            else    ! if (e3 .eq. 0.) then
              f3 = y(i1,i2,i3) ** e3
            endif    ! if (e3 .eq. 0.) then

            x(i1,i2,i3) = c1 + s1 * f1 * f2 + s3 * f3

c     1                  + s1 * x(i1,i2,i3) ** e1 * y(i1,i2,i3) ** e2
c     1                  + s3 * y(i1,i2,i3) ** e3

          enddo    ! do i1 = i1_a , i1_b , i1_c

        enddo    ! do i2 = i2_a , i2_b , i2_c

      enddo    ! do i3 = i3_a , i3_b , i3_c

      return
      end

c23456789012345678901234567890123456789012345678901234567890123456789012
      subroutine rmod_operate_read_grid(h_file,h_file0,mod_type
     1,cxi,cyi,czi,m_cord,n_cord,x_cord,cord
     1,m_vel
     1,nx_vel,x0_vel,dx_vel
     1,ny_vel,y0_vel,dy_vel
     1,nz_vel,z0_vel,dz_vel
     1,vel,zel
     1,m_work,work)
      implicit none

      integer   util_len_r

      character h_file*(*),h_file0*(*),mod_type*(*)
      character cxi*(*),cyi*(*),czi*(*)
      character cord(*)*(*)
      integer   m_cord,n_cord
      real      x_cord

      integer   m_vel
      integer   nx_vel
      real      x0_vel,dx_vel
      integer   ny_vel
      real      y0_vel,dy_vel
      integer   nz_vel
      real      z0_vel,dz_vel
      real      vel(1),zel(1)
      integer   m_work
      real      work(1)

      integer   i_file
      integer   i_err

      character cxv*16,cyv*16,czv*16,mod_type_out*16,crd80*80

      real      x_min,x_max,y_min,y_max,z_min,z_max,z_datum
      integer   nx_mix,ny_mix,nz_mix

      integer   nx_zel
      real      x0_zel,dx_zel
      integer   ny_zel
      real      y0_zel,dy_zel
      integer   nz_zel
      real      z0_zel,dz_zel

      integer  n1,n2,n3
      real     r1,r2,r3
      integer   i_work_i,i_work_n

      integer  na1
      real     oa1,da1
      integer  na2
      real     oa2,da2
      integer  na3
      real     oa3,da3

      integer  nb1
      real     ob1,db1
      integer  nb2
      real     ob2,db2
      integer  nb3
      real     ob3,db3
      integer  n_vel,i_vel,n_work,i_work,lu

      print'(/,'' Enter the name of the grid file''
     1,'' to use for the conversion.''
     1,/,'' This will be converted to a gridded model''
     1,/,'' before transforming the input grid'')'
      call util_copy_file_name(h_file,h_file0)
      call util_get_file_name(' Enter file for conversion'
     1,h_file0,'hgrid')

c  read the gridded velocity - note zel is used for gridded vleocity
      lu = -1
      call rmodrgrd(h_file0,mod_type_out
     1,cxv,cyv,czv,cxi,cyi,czi,m_cord,n_cord,x_cord,cord
     1,x_min,x_max,y_min,y_max,z_min,z_max,z_datum
     1,m_work,nx_zel,x0_zel,dx_zel,ny_zel,y0_zel,dy_zel,nz_zel
     1,z0_zel,dz_zel,work,m_vel,zel,lu
     1,i_err)
      if (i_err .ne. 0) goto 999

      if ( (mod_type(1:4) .eq. 'G3DL' .and. mod_type_out(1:4) .ne.
     1 'G3DL')
     1.or. (mod_type(1:4) .ne. 'G3DL' .and. mod_type_out(1:4) .eq.
     1 'G3DL')) then

        print'('' boith grid files must have the same form''
     1,/,'' input grid mod_type:'',a
     1,/,'' new   grid mod_type:'',a)'
     1,mod_type(1:util_len_r(mod_type))
     1,mod_type_out(1:util_len_r(mod_type_out))

          i_err = -1
          goto 999

      endif    ! if ((mod_type(1:4) .eq. 'G3DL' .and. mod_type_out(1:4) 


c  interpolate to the new grid to the original grid
      if (mod_type .eq. 'G3DL') then

        nz_zel = nz_vel
        z0_zel = z0_vel
        dz_zel = dz_vel

        na1 = nx_zel
        oa1 = x0_zel
        da1 = dx_zel

        na2 = ny_zel
        oa2 = y0_zel
        da2 = dy_zel

        na3 = nz_zel
        oa3 = z0_zel
        da3 = dz_vel

        nb1 = nx_vel
        ob1 = x0_vel
        db1 = dx_vel

        nb2 = ny_vel
        ob2 = y0_vel
        db2 = dy_vel

        nb3 = nz_vel
        ob3 = z0_vel
        db3 = dz_vel

      else

        na2 = nx_zel
        oa2 = x0_zel
        da2 = dx_zel

        na3 = ny_zel
        oa3 = y0_zel
        da3 = dy_zel

        na1 = nz_zel
        oa1 = z0_zel
        da1 = dz_zel

        nb2 = nx_vel
        ob2 = x0_vel
        db2 = dx_vel

        nb3 = ny_vel
        ob3 = y0_vel
        db3 = dy_vel

        nb1 = nz_vel
        ob1 = z0_vel
        db1 = dz_vel

      endif

      n_vel = na1 * na2 * na3
      n_work = 4 * (nb1 + nb2 + nb3)
      call util_wors(i_work_i,i_work_n,m_work)
      call util_work(i_work_i,i_work_n,i_vel,n_vel)
      call util_work(i_work_i,i_work_n,i_work,n_work)
      call util_worc(i_work_i,i_work_n,i_err)

      if (nx_vel*ny_vel*nz_vel .gt. m_vel .or. i_err .ne. 0) then
        print'('' insufficient memory for new grid''
     1,/,'' m_vel='',i8,'' nx_vel*ny_vel*nz_vel='',i8,'' i_err='',i8)'
     1,m_vel,nx_vel*ny_vel*nz_vel,i_err
        return
      endif

c      print*,' na1=',na1,na2,na3,' nb1=',nb1,nb2,nb3
c      print*,' oa1=',oa1,oa2,oa3,' ob1=',ob1,ob2,ob3
c      print*,' da1=',da1,da2,da3,' db1=',db1,db2,db3

      print'(
     1   '' na1='',i10,'' oa1='',f10.2,'' da1='',f10.2
     1,/,'' na2='',i10,'' oa2='',f10.2,'' da2='',f10.2
     1,/,'' na3='',i10,'' oa3='',f10.2,'' da3='',f10.2
     1,/,'' nb1='',i10,'' ob1='',f10.2,'' db1='',f10.2
     1,/,'' nb2='',i10,'' ob2='',f10.2,'' db2='',f10.2
     1,/,'' nb3='',i10,'' ob3='',f10.2,'' db3='',f10.2)'
     1,na1,oa1,da1
     1,na2,oa2,da2
     1,na3,oa3,da3
     1,nb1,ob1,db1
     1,nb2,ob2,db2
     1,nb3,ob3,db3

      n1 = max(0,nint(db1/da1)/2)
      n2 = max(0,nint(db2/da2)/2)
      n3 = max(0,nint(db3/da3)/2)

      r1 = n1 * da1
      r2 = n2 * da2
      r3 = n3 * da3

      n1 = max(0,nint(r1/da1))
      n2 = max(0,nint(r2/da2))
      n3 = max(0,nint(r3/da3))

c      subroutine rmod_smooth_grid_0(
c     1 r1_top,r2_top,r3_top
c     1,d1_top,d2_top,d3_top
c     1,r1_bot,r2_bot,r3_bot
c     1,d1_bot,d2_bot,d3_bot
c     1,na1,oa1,da1,na2,oa2,da2,na3,oa3,da3,a
c     1,nb1,ob1,db1,nb2,ob2,db2,nb3,ob3,db3,b)
      call rmod_smooth_grid_0(
     1 r1,r2,r3
     1,da1,da2,da3
     1,r1,r2,r3
     1,da1,da2,da3
     1,na1,oa1,da1,na2,oa2,da2,na3,oa3,da3,work(i_vel)
     1,nb1,ob1,db1,nb2,ob2,db2,nb3,ob3,db3,zel,n_work,work(i_work)
     1,i_err)
      if (i_err .ne. 0) goto 999

      return
  999 continue
      call rmod_pause(' error during grid interpolation',' ')
      print'(/,'' error in rmod_interpolate_grid'')'
      return

      end

c23456789012345678901234567890123456789012345678901234567890123456789012
      subroutine rmod_get_real(title,default,value)
      implicit  none
      character title*(*)
      real      default,value
      character crd80*80
      integer   util_len_r
      value = default

    1 continue
      crd80 = ' '
      print'(/,'' enter '',a,/,'' default='',g16.9)'
     1,title(1:util_len_r(title)),value
      read(*,'(a)',err=1,end=2)crd80
      if (crd80 .ne. ' ') read(crd80,*,err=1,end=2) value
    2 continue
      return
      end

c23456789012345678901234567890123456789012345678901234567890123456789012
      subroutine rmod_get_real_2(title
     1,default_1,value_1
     1,default_2,value_2
     1)
      implicit  none
      character title*(*)
      real      default_1,default_2
      real      value_1,value_2
      character crd80*80
      integer   util_len_r
      value_1 = default_1
      value_2 = default_2

    1 continue
      crd80 = ' '
      print'(/,'' enter '',a,/,'' defaults='',g16.9,1x,g16.9)'
     1,title(1:util_len_r(title)),value_1,value_2
      read(*,'(a)',err=1,end=2)crd80
      if (crd80 .ne. ' ') read(crd80,*,err=1,end=2) value_1,value_2
    2 continue
      return
      end

c23456789012345678901234567890123456789012345678901234567890123456789012
      subroutine rmod_get_int_real(title
     1,default_1,value_1
     1,default_2,value_2)

      implicit  none

      character title*(*)

      integer   default_1
      real      default_2

      integer   value_1
      real      value_2

      character crd80*80

      integer   util_len_r

      value_1 = default_1
      value_2 = default_2

    1 continue
      crd80 = ' '
      print'(/,'' enter '',a,/,'' defaults='',i10,1x,g16.9)'
     1,title(1:util_len_r(title)),value_1,value_2
      read(*,'(a)',err=1,end=2)crd80
      if (crd80 .ne. ' ') read(crd80,*,err=1,end=2) value_1,value_2
    2 continue
      return
      end

c23456789012345678901234567890123456789012345678901234567890123456789012
      subroutine rmod_get_real_2_end(title
     1,default_1,value_1
     1,default_2,value_2
     1,i_end)
      implicit  none
      character title*(*)
      real      default_1,default_2
      real      value_1,value_2
      character crd80*80
      integer   util_len_r
      logical   rmod_char_end
      integer   i_end
      value_1 = default_1
      value_2 = default_2

      i_end = 0
    1 continue
      crd80 = ' '
      print'(/,'' enter '',a,/,'' defaults='',g16.9,1x,g16.9)'
     1,title(1:util_len_r(title)),value_1,value_2
      read(*,'(a)',err=1,end=2)crd80
      if (crd80 .ne. ' ') then
        if (rmod_char_end(crd80)) then
          i_end = 1
        else
          read(crd80,*,err=1,end=2) value_1,value_2
        endif
      endif
    2 continue
      return
      end

c23456789012345678901234567890123456789012345678901234567890123456789012
      subroutine rmod_get_integer(title,default,value)
      implicit  none
      character title*(*)
      integer   default,value
      character crd80*80
      integer   util_len_r
      value = default

    1 continue
      crd80 = ' '
      print'(/,'' enter '',a,/,'' default='',i16)'
     1,title(1:util_len_r(title)),value
      read(*,'(a)',err=1,end=2)crd80
      if (crd80 .ne. ' ') read(crd80,*,err=1,end=2) value
    2 continue
      return
      end

c23456789012345678901234567890123456789012345678901234567890123456789012
      subroutine rmod_convert_velocity_type(mod_type,czi
     1,nx_vel,ny_vel,nz_vel,z0_vel,dz_vel,vel,m_work,work)
      implicit  none
      integer   util_len_r
      character mod_type*(*),czi*(*)
      integer   nx_vel
      integer   ny_vel
      integer   nz_vel
      real      z0_vel,dz_vel
      real      vel(nz_vel,1)
      integer   m_work
      real      work(1)
      integer   i_work_i,i_work_n

      integer   i_t_inp,i_t_out,i_z_out,i_v_rms,i_v_int,i_v_ave
      integer   i_xy,i_err
      character mod_type_inp*4,mod_type_out*4,crd80*80

      print'(/,'' This option converts between CPS velocity types''
     1,'' VRMS,VAVE and VINT'')'

      if (mod_type .ne. 'GRID') then
        print'('' Model mod_type must be grid for this option ''
     1,'' mod_type='',a16)',mod_type
        call rmod_pause(' ',' ')
        return
      endif

      if (czi .ne. 'TIME' .and. czi .ne. 'DEPTH') then
        print'('' convert to TIME or DEPTH  units  czi='',a)'
     1,czi(1:util_len_r(czi))
        return
      endif

      call rmod_convert_velocity_get_types(czi,mod_type_inp
     1,mod_type_out)

c  get work space for arrays
      call util_wors(i_work_i,i_work_n,m_work)
      call util_work(i_work_i,i_work_n,i_t_inp,nz_vel)
      call util_work(i_work_i,i_work_n,i_t_out,nz_vel)
      call util_work(i_work_i,i_work_n,i_z_out,nz_vel)
      call util_work(i_work_i,i_work_n,i_v_rms,nz_vel)
      call util_work(i_work_i,i_work_n,i_v_ave,nz_vel)
      call util_work(i_work_i,i_work_n,i_v_int,nz_vel)
      call util_worc(i_work_i,i_work_n,i_err)
      if (i_err .ne. 0) goto 997

      call util_line(nz_vel,work(i_t_inp),z0_vel,dz_vel)

      print'(/,'' converting from '',i8,'' velocity functions ''
     1,/,'' nz_vel='',i8,'' z0_vel='',f10.2,'' dz_vel='',f10.2
     1,/,'' from input type='',a4
     1,'' to output type='',a4,'' in the '',a16,'' domain'')'
     1,nx_vel*ny_vel,nz_vel,z0_vel,dz_vel,mod_type_inp,mod_type_out,czi

c  convert from slowness to velocity
      call util_invert(nx_vel*ny_vel*nz_vel,vel)

      do i_xy = 1 , nx_vel * ny_vel

      print*,' i_xy=',i_xy,' vel=',vel(1,i_xy),vel(nz_vel,i_xy)
        call velgen(mod_type_inp,nz_vel,work(i_t_inp),vel(1,i_xy)
     1,work(i_t_out),work(i_z_out)
     1,work(i_v_rms),work(i_v_ave),work(i_v_int)
     1,i_err)
        if (i_err .ne. 0) goto 998

        if (mod_type_out .eq. 'VTRM' .or. mod_type_out .eq. 'VZRM') then

      print*,' vtrm'
          call util_copy(nz_vel,work(i_v_rms),vel(1,i_xy))

        elseif (mod_type_out .eq. 'VTIN' .or. mod_type_out .eq. 'VZIN')
     1 then

      print*,' vtin'
          call util_copy(nz_vel,work(i_v_int),vel(1,i_xy))

        elseif (mod_type_out .eq. 'VTAV' .or. mod_type_out .eq. 'VZAV')
     1 then

      print*,' vtav'
          call util_copy(nz_vel,work(i_v_ave),vel(1,i_xy))

        endif

      print*,' i_xy=',i_xy,' vel=',vel(1,i_xy),vel(nz_vel,i_xy)

      enddo    ! do i_xy = 1 , nx_vel * ny_vel

c  convert back from velocity to slowness
      call util_invert(nx_vel*ny_vel*nz_vel,vel)

      crd80 = ' '
      write(crd80,'('' RMOD converting from input type ''
     1,a4,'' to output type '',a4)')mod_type_inp,mod_type_out
      call rmod_title_to_history(crd80)
      crd80 = ' '
      write(crd80,'('' in the'',a16,'' domain'')')czi
      call rmod_card_to_history(crd80)

      return

  997 continue
      print'(/,'' error in rmod_convert_velocity ''
     1,'' during work allocation m_work='',i8)',m_work
      goto 999

  998 continue
      print'(/,'' error in rmod_convert_velocity ''
     1,'' inside velgen i_xy='',i8,'' i_err='',i8)',i_xy,i_err
      goto 999

  999 continue
c      print'(/,'' error in rmod_convert_velocity '')'
      call rmod_pause(' error in rmod_convert_velocity',' ')
      return

      end

c23456789012345678901234567890123456789012345678901234567890123456789012
      subroutine rmod_convert_velocity_get_types(czi,mod_type_inp
     1,mod_type_out)
      character  czi*(*),mod_type_inp*4,mod_type_out*4
      character  crd_04*4

      print'('' supported types are''
     1,/,'' VTRM  -->      RMS VELOCITY versus 2-WAY TIME.''
     1,/,'' VTAV  -->  AVERAGE VELOCITY versus 2-WAY TIME.''
     1,/,'' VTIN  --> INTERVAL VELOCITY versus 2-WAY TIME.''
     1,/,'' VZRM  -->      RMS VELOCITY versus DEPTH.''
     1,/,'' VZAV  -->  AVERAGE VELOCITY versus DEPTH.''
     1,/,'' VZIN  --> INTERVAL VELOCITY versus DEPTH.''
     1)'

      if (czi .eq. 'TIME') then
        mod_type_inp = 'VTIN'
        mod_type_out = 'VTRM'
      else    ! if (czi .eq. 'TIME') then
        mod_type_inp = 'VZIN'
        mod_type_out = 'VZRM'
      endif    ! if (czi .eq. 'TIME') then

    1 continue
      if (czi .eq. 'TIME') then
        mod_type_inp = 'VTIN'
      else    ! if (czi .eq. 'TIME') then
        mod_type_inp = 'VZIN'
      endif    ! if (czi .eq. 'TIME') then
      print'(/,'' enter input mod_type default='',a4)',mod_type_inp
      crd_04 = ' '
      read(*,'(a)',err=1,end=2)crd_04
      if (crd_04 .ne. ' ') mod_type_inp = crd_04
    2 continue

      call rmodcaps(mod_type_inp,mod_type_inp)

      if (
     1 (czi .eq. 'TIME'
     1.and. mod_type_inp .ne. 'VTRM'
     1.and. mod_type_inp .ne. 'VTIN'
     1.and. mod_type_inp .ne. 'VTAV')
     1 .or.
     1 (czi .eq. 'DEPTH'
     1.and. mod_type_inp .ne. 'VZRM'
     1.and. mod_type_inp .ne. 'VZIN'
     1.and. mod_type_inp .ne. 'VZAV')
     1) goto 1

    3 continue
      if (czi .eq. 'TIME') then
        mod_type_out = 'VTRM'
      else    ! if (czi .eq. 'TIME') then
        mod_type_out = 'VZRM'
      endif    ! if (czi .eq. 'TIME') then
      print'(/,'' enter output mod_type default='',a4)',mod_type_out
      crd_04 = ' '
      read(*,'(a)',err=1,end=2)crd_04
      if (crd_04 .ne. ' ') mod_type_out = crd_04
    4 continue

      call rmodcaps(mod_type_out,mod_type_out)

      if (
     1 (czi .eq. 'TIME'
     1.and. mod_type_out .ne. 'VTRM'
     1.and. mod_type_out .ne. 'VTIN'
     1.and. mod_type_out .ne. 'VTAV')
     1 .or.
     1 (czi .eq. 'DEPTH'
     1.and. mod_type_out .ne. 'VZRM'
     1.and. mod_type_out .ne. 'VZIN'
     1.and. mod_type_out .ne. 'VZAV')
     1) goto 3

      return
      end

c23456789012345678901234567890123456789012345678901234567890123456789012
      subroutine rmod_grid_2dl(mod_type
     1,nb,ixb,nxb,xb,zb
     1,nx_out,x0_out,dx_out
     1,ny_out,y0_out,dy_out
     1,nz_out,z0_out,dz_out
     1,a_out,m_work,work)

      implicit  none

      character mod_type*(*)

      integer   nx_out
      real      x0_out,dx_out

      integer   ny_out
      real      y0_out,dy_out

      integer   nz_out
      real      z0_out,dz_out

      real      a_out(nx_out,ny_out,1)

      integer   nb,ixb(1),nxb(1)
      real      xb(1),zb(1)

      integer   m_work
      real      work(1)

      integer   ix_out,iy_out,ib,ixb_1,ixb_2,ixb_inc,jxb
      real      x_out,z_out,dzdx
      character crd80*80

c  make sure this is the proper model mod_type
      print'(/
     1,'' This option interpolates boundaries to a uniform grid'')'

      print'(/,'' 3d grid characteristics''
     1,/,'' nx_out='',i5,'' x0_out='',f10.2,'' dx_out='',f10.4
     1,/,'' ny_out='',i5,'' y0_out='',f10.2,'' dy_out='',f10.4
     1,/,'' original nz_out='',i5,'' z0_out='',f10.2
     1,'' dz_out='',f10.4
     1,/,'' revised  nz_out='',i5,'' z0_out='',f10.2
     1,'' dz_out='',f10.4)'
     1,nx_out,x0_out,dx_out
     1,ny_out,y0_out,dy_out
     1,nz_out,z0_out,dz_out
     1,nb    ,z0_out,dz_out

      if (mod_type .ne. 'LAYER') then
        print'('' Model mod_type must be layer''
     1,'' mod_type='',a16)',mod_type
        call rmod_pause(' ',' ')
        return
      endif    ! if (mod_type .ne. 'LAYER') then

      do ib = 1 , nb

        do ix_out = 1 , nx_out

          x_out = (ix_out - 1) * dx_out + x0_out

          if (xb(ixb_1) .le. xb(ixb_2)) then

            ixb_1 = ixb(ib) + 1
            ixb_2 = ixb(ib) + nxb(ib)

          else    ! if (xb(ixb_1) .le. xb(ixb_2)) then

            ixb_2 = ixb(ib) + 1
            ixb_1 = ixb(ib) + nxb(ib)

          endif    ! if (xb(ixb_1) .le. xb(ixb_2)) then

          ixb_inc = isign(1,ixb_2-ixb_1)

          if (x_out .le. xb(ixb_1)) then

             z_out = zb(ixb_1)

          elseif (x_out .ge. xb(ixb_2)) then

             z_out = zb(ixb_2)

          else

             do jxb = ixb_1 , ixb_2-ixb_inc , ixb_inc

               if (x_out .ge. xb(jxb)
     1       .and. x_out .le. xb(jxb+ixb_inc)) then

                 dzdx = 0
                 if (xb(jxb) .ne. xb(jxb+ixb_inc))
     1dzdx = (zb(jxb+ixb_inc) - zb(jxb)) / (xb(jxb+ixb_inc) - xb(jxb))
                 z_out = zb(jxb) + (x_out - xb(jxb)) * dzdx
                 goto 1

               endif    ! if (x_out .ge. xb(jxb) .abnd. x_out .le.

             enddo    ! do jxb = ixb_1 , ixb_2-ixb_inc , ixb_inc

             print'('' could not find x s to brackett x_out=''
     1,f10.2)',x_out
             write(88,*)' x_out=',x_out
             write(88,*)' ixb_1=',ixb_1,' ixb_2=',ixb_2
     1,' ixb_inc=',ixb_inc
             do jxb = ixb_1 , ixb_2 , ixb_inc
      write(88,*)jxb,xb(jxb),zb(jxb)
             enddo    ! do jxb = ixb_1 , ixb_2 , ixb_inc
             stop

    1 continue

          endif    ! if (x_out .le. xb(ixb_1)) then

          do iy_out = 1 , ny_out

            a_out(ix_out,iy_out,ib) = z_out

          enddo    ! do iy_out = 1 , ny_out

        enddo    ! do ix_out = 1 , nx_out

      enddo    ! do ib = 1 , nb

      nz_out = nb
      mod_type = 'G3DL'

      return
      end

c23456789012345678901234567890123456789012345678901234567890123456789012
      subroutine rmod_read_data(i_file,n_card,n_fill,i_save
     1,ix,nx,x0,dx,x_min,x_max
     1,iy,ny,y0,dy,y_min,y_max
     1,iz,nz,z0,dz,z_min,z_max
     1,iv,v_min,v_max,v,i_err)

      implicit none
      integer  i_file,n_card,n_fill,i_save
      integer  ix,nx
      real     x0,dx,x_min,x_max
      integer  iy,ny
      real     y0,dy,y_min,y_max
      integer  iz,nz
      real     z0,dz,z_min,z_max
      integer  iv
      real     v_min,v_max
      real     v(1)
      integer  i_err

      integer  util_len_r
      integer  m,n
      parameter (m=20)
      real     data(m)
      character crd132*132
      integer  j,jx,jy,jz,jxyz,i_f,i_pound
      real     x_min_2,y_min_2,z_min_2

      i_err = 0
      n = max(ix,iy,iz,iv)
      n_card = 0
      n_fill = 0

    1 continue

        call util_setr(n,data,0.)
        read(i_file,'(a)',err=999,end=2)crd132
        call rmodfchr(i_f,'F',1,util_len_r(crd132),crd132)
        call rmodfchr(i_pound,'#',1,util_len_r(crd132),crd132)
        if (i_pound .ne. 0) goto 1

          read(crd132,*,err=1001,end=3)(data(j),j=1,n)
          goto 1002

 1001     continue
          read(crd132,'(10x,f10.2,f10.2,f11.2,f11.2,f11.4)'
     1,err=999,end=3)(data(j),j=1,n)

 1002     continue

    3    continue
        n_card = n_card + 1

        if (n_card .eq. 1) then

          x_min = data(ix)
          x_max = data(ix)
          y_min = data(iy)
          y_max = data(iy)
          z_min = data(iz)
          z_max = data(iz)
          v_min = data(iv)
          v_max = data(iv)

        else    ! if (n_card .eq. 1) then

          if (data(ix) .lt. x_min) x_min_2 = x_min
          if (data(iy) .lt. y_min) y_min_2 = y_min
          if (data(iz) .lt. z_min) z_min_2 = z_min
          x_min = min(x_min,data(ix))
          x_max = max(x_max,data(ix))
          y_min = min(y_min,data(iy))
          y_max = max(y_max,data(iy))
          z_min = min(z_min,data(iz))
          z_max = max(z_max,data(iz))
          v_min = min(v_min,data(iv))
          v_max = max(v_max,data(iv))

        endif    ! if (n_card .eq. 1) then

        if (i_save .ne. 0) then

          jx = nint((data(ix)-x0)/dx) + 1
          jy = nint((data(iy)-y0)/dy) + 1
          jz = nint((data(iz)-z0)/dz) + 1

          jxyz = (jz - 1) * nx * ny + (jy - 1) * nx + jx

          if (jx .ge. 1. and. jx .le. nx
     1  .and. jy .ge. 1. and. jy .le. ny
     1  .and. jz .ge. 1. and. jz .le. nz) then
            v(jxyz) = data(iv)
            n_fill = n_fill + 1
          endif

        endif    ! if (i_save .ne. 0) then

    2 continue

      print'('' x_min='',g16.9,'' x_max='',g16.9
     1,'' x_min_2='',g16.9,'' nx='',i10,'' dx='',g16.9)'
     1,x_min,x_max,x_min_2
     1,int((x_max-x_min)/max(1e-10,x_min_2-x_min))+1
     1,x_min_2-x_min

      print'('' y_min='',g16.9,'' y_max='',g16.9
     1,'' y_min_2='',g16.9,'' ny='',i10,'' dy='',g16.9)'
     1,y_min,y_max,x_min_2
     1,int((y_max-y_min)/max(1e-10,y_min_2-y_min))+1
     1,y_min_2-y_min

      print'('' z_min='',g16.9,'' z_max='',g16.9
     1,'' z_min_2='',g16.9,'' nz='',i10,'' dz='',g16.9)'
     1,z_min,z_max,z_min_2
     1,int((z_max-z_min)/max(1e-10,z_min_2-z_min))+1
     1,z_min_2-z_min

      return

  999 continue
      i_err = -1
      return

      end

c23456789012345678901234567890123456789012345678901234567890123456789012
      subroutine rmod_read_data_2(i_file,n_card,n_fill,i_save
     1,ix,nx,x0,dx,x_min,x_max
     1,iy,ny,y0,dy,y_min,y_max
     1,iz,z_min,z_max,z,i_err)

      implicit none
      integer  i_file,n_card,n_fill,i_save
      integer  ix,nx
      real     x0,dx,x_min,x_max
      integer  iy,ny
      real     y0,dy,y_min,y_max
      integer  iz
      real     z_min,z_max
      real     z(1)
      integer  i_err

      integer  util_len_r
      integer  m,n
      parameter (m=20)
      real     data(m)
      character crd132*132
      integer  j,jx,jy,jxy,i_f,i_pound
      real     x_min_2,y_min_2

      i_err = 0
      n = max(ix,iy,iz)
      n_card = 0
      n_fill = 0

    1 continue

        call util_setr(n,data,0.)
        read(i_file,'(a)',err=999,end=2)crd132
        call rmodfchr(i_f,'F',1,util_len_r(crd132),crd132)
        call rmodfchr(i_pound,'#',1,util_len_r(crd132),crd132)
        if (i_pound .ne. 0) goto 1

          read(crd132,*,err=1001,end=3)(data(j),j=1,n)
          goto 1002

 1001     continue
          read(crd132,'(10x,f10.2,f10.2,f11.2,f11.2,f11.4)'
     1,err=999,end=3)(data(j),j=1,n)

 1002     continue

    3    continue
        n_card = n_card + 1

        if (n_card .eq. 1) then

          x_min = data(ix)
          x_max = data(ix)
          y_min = data(iy)
          y_max = data(iy)
          z_min = data(iz)
          z_max = data(iz)

        else    ! if (n_card .eq. 1) then

c          if (data(ix) .lt. x_min) x_min_2 = x_min
c          if (data(iy) .lt. y_min) y_min_2 = y_min
          x_min = min(x_min,data(ix))
          x_max = max(x_max,data(ix))
          y_min = min(y_min,data(iy))
          y_max = max(y_max,data(iy))
          z_min = min(z_min,data(iz))
          z_max = max(z_max,data(iz))

        endif    ! if (n_card .eq. 1) then

        if (i_save .ne. 0) then

          jx = nint((data(ix)-x0)/dx) + 1
          jy = nint((data(iy)-y0)/dy) + 1

          jxy = (jy - 1) * nx + jx

          if (jx .ge. 1. and. jx .le. nx
     1  .and. jy .ge. 1. and. jy .le. ny) then
            z(jxy) = data(iz)
            n_fill = n_fill + 1
          endif

        endif    ! if (i_save .ne. 0) then
        goto 1

    2 continue

c      print'('' x_min='',g16.9,'' x_max='',g16.9
c     1,'' x_min_2='',g16.9,'' nx='',i10,'' dx='',g16.9)'
c     1,x_min,x_max,x_min_2
c     1,int((x_max-x_min)/max(1e-3,x_min_2-x_min))+1
c     1,x_min_2-x_min

c      print'('' y_min='',g16.9,'' y_max='',g16.9
c     1,'' y_min_2='',g16.9,'' ny='',i10,'' dy='',g16.9)'
c     1,y_min,y_max,x_min_2
c     1,int((y_max-y_min)/max(1e-3,y_min_2-y_min))+1
c     1,y_min_2-y_min

      return

  999 continue
      i_err = -1
      return

      end

c23456789012345678901234567890123456789012345678901234567890123456789012
      subroutine rmod_read_data_3(i_file,n_card,n_fill,i_save,iz_flag
     1,ix_1,ix_2,nx,x0,dx,x_min,x_max
     1,iy_1,iy_2,ny,y0,dy,y_min,y_max
     1,iz_1,iz_2,nz,z,z_min,z_max
     1,if_1,if_2
     1,i_err)
      implicit none
      integer  i_file,n_card,n_fill,i_save,iz_flag

      integer   ix_1,ix_2
      integer  ix,nx
      real     x0,dx,x_min,x_max

      integer   iy_1,iy_2
      integer  iy,ny
      real     y0,dy,y_min,y_max

      integer   iz_1,iz_2
      integer  iz,nz
      real     z_min,z_max
      real     z(nx,ny,1)
      integer   if_1,if_2

      integer  i_err

      integer  util_len_r,lc
      integer  m,n
      parameter (m=20)
      real     data(m)
      character crd132*132
      integer  j,jx,jy,jz,jxy,i_f,i_pound
      real     x_min_2,y_min_2
      real     x1,y1,z1

      i_err = 0
      n_card = 0
      n_fill = 0

    1 continue

        call util_setr(n,data,0.)
        read(i_file,'(a)',err=999,end=2)crd132
        lc = util_len_r(crd132)

        call rmodfchr(i_pound,'#',1,util_len_r(crd132),crd132)
        if (i_pound .ne. 0) goto 1
        call rmodfchr(i_f,'F',1,lc,crd132)
        x1 = x0
        y1 = y0
        z1 = 0
        jz = 1

        if (ix_1 .lt. 1 .or. ix_2 .lt. 1
     1 .or. iy_1 .lt. 1 .or. iy_2 .lt. 1
     1 .or. iz_1 .lt. 1 .or. iz_2 .lt. 1
     1) then

          if (ix_2 .eq. -2) then

            if (iz_flag .eq. 0) then

              jz = 1

              read(crd132,*,err=1,end=3)x1,z1

            else

              read(crd132,*,err=1,end=3)x1,z1,jz

            endif    ! if (iz_flag .eq. 0) then

          else    ! if (ix_2 .eq. -2) then

            if (iz_flag .eq. 0) then

              jz = 1
              read(crd132,*,err=1,end=3)x1,y1,z1

            else

              read(crd132,*,err=1,end=3)x1,y1,z1,jz

            endif    ! if (iz_flag .eq. 0) then

          endif    ! if (ix_2 .eq. -2) then

        else    ! if (ix_1 .lt. 1 .or. ix_2 .lt. 1

          read(crd132(ix_1:min(lc,ix_2)),*,err=1,end=3)x1
          read(crd132(iy_1:min(lc,iy_2)),*,err=1,end=3)y1
          read(crd132(iz_1:min(lc,iz_2)),*,err=1,end=3)z1

          if (iz_flag .eq. 0) then

            jz = 1

          else

            read(crd132(if_1:min(lc,if_2)),*,err=1,end=3)jz

          endif    ! if (iz_flag .eq. 0) then

        endif    ! if (ix_1 .lt. 1 .or. ix_2 .lt. 1

    3    continue
        n_card = n_card + 1

        if (n_card .eq. 1) then

          x_min = x1
          x_max = x1
          y_min = y1
          y_max = y1
          z_min = z1
          z_max = z1

        else    ! if (n_card .eq. 1) then

c          if (x1 .lt. x_min) x_min_2 = x_min
c          if (y1 .lt. y_min) y_min_2 = y_min
          x_min = min(x_min,x1)
          x_max = max(x_max,x1)
          y_min = min(y_min,y1)
          y_max = max(y_max,y1)
          z_min = min(z_min,z1)
          z_max = max(z_max,z1)

        endif    ! if (n_card .eq. 1) then

        if (i_save .ne. 0) then

          jx = nint((x1-x0)/dx) + 1
          jy = nint((y1-y0)/dy) + 1

          if (jx .ge. 1. and. jx .le. nx
     1  .and. jy .ge. 1. and. jy .le. ny
     1  .and. jz .ge. 1. and. jz .le. nz) then

            z(jx,jy,jz) = z1
            n_fill = n_fill + 1

          endif

c      if (n_card .le. 100)
c     1print'('' n_card='',i5,'' n_fill='',i5
c     1,'' jxy='',i5,'' jx='',i5,'' jy='',i5
c     1,'' x='',f8.0,'' y='',f8.0,'' z='',f8.0)'
c     1,n_card,n_fill,jxy,jx,jy,x1,y1,z1

        endif    ! if (i_save .ne. 0) then
        goto 1

    2 continue

c      print'(/,'' x_min='',g16.9,'' x_max='',g16.9
c     1,'' x_min_2='',g16.9,'' nx='',i10,'' dx='',g16.9)'
c     1,x_min,x_max,x_min_2
c     1,int((x_max-x_min)/max(1e-3,x_min_2-x_min))+1
c     1,x_min_2-x_min

c      print'(/,'' y_min='',g16.9,'' y_max='',g16.9
c     1,'' y_min_2='',g16.9,'' ny='',i10,'' dy='',g16.9)'
c     1,y_min,y_max,y_min_2
c     1,int((y_max-y_min)/max(1e-3,y_min_2-y_min))+1
c     1,y_min_2-y_min

c      print'(/,'' i_save='',i4,'' n_card='',i8,'' n_fill='',i8
c     1,'' z1='',f10.2,'' zn='',f10.2)'
c     1,i_save,n_card,n_fill,z(1),z(n_fill)

      return

  999 continue
      i_err = -1
      return

      end

c23456789012345678901234567890123456789012345678901234567890123456789012
      subroutine rmod_read_data_4(i_file,n_card,n_fill,i_save
     1,ix_1,ix_2,nx,x0,dx,x_min,x_max
     1,iy_1,iy_2,ny,y0,dy,y_min,y_max
     1,iz_1,iz_2,nz,z0,dz,z_min,z_max
     1,iv_1,iv_2,v_min,v_max,v,i_err)

      implicit none
      integer  i_file,n_card,n_fill,i_save

      integer   ix_1,ix_2
      integer   ix,nx
      real      x0,dx,x_min,x_max

      integer   iy_1,iy_2
      integer   iy,ny
      real      y0,dy,y_min,y_max

      integer   iz_1,iz_2
      integer   iz,nz
      real      z0,dz,z_min,z_max

      integer   iv_1,iv_2
      integer   iv
      real      v_min,v_max
      real      v(1)

      integer   i_err

      integer    util_len_r,lc
      integer    m,n
      parameter  (m=20)
      real       data(m)
      character   crd132*132
      integer    j,jx,jy,jz,jxyz,i_f,i_pound
      real       x_min_2,y_min_2,z_min_2
      real       x1,y1,z1,v1

      i_err = 0
      n_card = 0
      n_fill = 0

    1 continue

        call util_setr(n,data,0.)
        read(i_file,'(a)',err=999,end=2)crd132
        lc = util_len_r(crd132)

        call rmodfchr(i_pound,'#',1,util_len_r(crd132),crd132)
        if (i_pound .ne. 0) goto 1
        call rmodfchr(i_f,'F',1,lc,crd132)
        if (ix_1 .lt. 1 .or. ix_2 .lt. 1
     1 .or. iy_1 .lt. 1 .or. iy_2 .lt. 1
     1 .or. iz_1 .lt. 1 .or. iz_2 .lt. 1
     1 .or. iv_1 .lt. 1 .or. iv_2 .lt. 1) then
          if (ix_1 .eq. -2) then
          read(crd132,*,err=1,end=3)x1,z1,v1
          y1 = 0
        else
          read(crd132,*,err=1,end=3)x1,y1,z1,v1
          endif
        else
          read(crd132(ix_1:min(lc,ix_2)),*,err=1,end=3)x1
          read(crd132(iy_1:min(lc,iy_2)),*,err=1,end=3)y1
          read(crd132(iz_1:min(lc,iz_2)),*,err=1,end=3)z1
          read(crd132(iv_1:min(lc,iv_2)),*,err=1,end=3)v1
        endif

    3    continue
        n_card = n_card + 1

        if (n_card .eq. 1) then

          x_min = x1
          x_max = x1
          y_min = y1
          y_max = y1
          z_min = z1
          z_max = z1
          v_min = v1
          v_max = v1

        else    ! if (n_card .eq. 1) then

c          if (x1 .lt. x_min) x_min_2 = x_min
c          if (y1 .lt. y_min) y_min_2 = y_min
          x_min = min(x_min,x1)
          x_max = max(x_max,x1)
          y_min = min(y_min,y1)
          y_max = max(y_max,y1)
          z_min = min(z_min,z1)
          z_max = max(z_max,z1)
          v_min = min(v_min,v1)
          v_max = max(v_max,v1)

        endif    ! if (n_card .eq. 1) then

        if (i_save .ne. 0) then

          jx = nint((x1-x0)/dx) + 1
          jy = nint((y1-y0)/dy) + 1
          jz = nint((z1-z0)/dz) + 1

          jxyz= (jy - 1) * nx * nz + (jx - 1) * nz + jz

          if (jx .ge. 1. and. jx .le. nx
     1  .and. jy .ge. 1. and. jy .le. ny
     1  .and. jz .ge. 1. and. jz .le. nz) then
            v(jxyz) = v1
            n_fill = n_fill + 1
          endif

        endif    ! if (i_save .ne. 0) then
        goto 1

    2 continue

c      print'('' x_min='',g16.9,'' x_max='',g16.9
c     1,'' x_min_2='',g16.9,'' nx='',i10,'' dx='',g16.9)'
c     1,x_min,x_max,x_min_2
c     1,int((x_max-x_min)/max(1e-3,x_min_2-x_min))+1
c     1,x_min_2-x_min

c      print'('' y_min='',g16.9,'' y_max='',g16.9
c     1,'' y_min_2='',g16.9,'' ny='',i10,'' dy='',g16.9)'
c     1,y_min,y_max,y_min_2
c     1,int((y_max-y_min)/max(1e-3,y_min_2-y_min))+1
c     1,y_min_2-y_min

c      print'('' z_min='',g16.9,'' z_max='',g16.9
c     1,'' z_min_2='',g16.9,'' nz='',i10,'' dz='',g16.9)'
c     1,z_min,z_max,z_min_2
c     1,int((z_max-z_min)/max(1e-3,z_min_2-z_min))+1
c     1,z_min_2-z_min

      return

  999 continue
      i_err = -1
      return

      end

c23456789012345678901234567890123456789012345678901234567890123456789012
      subroutine rmod_read_data_5(i_file,n_card,i_save
     1,v_min,v_max,v,i_err)

      implicit none
      integer  i_file,n_card,i_save

      real      v_min,v_max
      real      v(1)

      integer   i_err

      integer    util_len_r,lc
      integer    m,n
      parameter  (m=20)
      real       data(m)
      character   crd132*132
      integer    j,i_f,i_pound
      real       v1

      i_err = 0
      n_card = 0

    1 continue

        read(i_file,'(a)',err=999,end=2)crd132
        lc = util_len_r(crd132)

        call rmodfchr(i_pound,'#',1,util_len_r(crd132),crd132)
        if (i_pound .ne. 0) goto 1
        read(crd132,*,err=1,end=3)v1

    3    continue
         n_card = n_card + 1

        if (n_card .eq. 1) then

          v_min = v1
          v_max = v1

        else    ! if (n_card .eq. 1) then

          v_min = min(v_min,v1)
          v_max = max(v_max,v1)

        endif    ! if (n_card .eq. 1) then

        if (i_save .ne. 0) v(n_card) = v1

        goto 1

    2 continue

c      print'('' x_min='',g16.9,'' x_max='',g16.9
c     1,'' x_min_2='',g16.9,'' nx='',i10,'' dx='',g16.9)'
c     1,x_min,x_max,x_min_2
c     1,int((x_max-x_min)/max(1e-3,x_min_2-x_min))+1
c     1,x_min_2-x_min

c      print'('' y_min='',g16.9,'' y_max='',g16.9
c     1,'' y_min_2='',g16.9,'' ny='',i10,'' dy='',g16.9)'
c     1,y_min,y_max,y_min_2
c     1,int((y_max-y_min)/max(1e-3,y_min_2-y_min))+1
c     1,y_min_2-y_min

c      print'('' z_min='',g16.9,'' z_max='',g16.9
c     1,'' z_min_2='',g16.9,'' nz='',i10,'' dz='',g16.9)'
c     1,z_min,z_max,z_min_2
c     1,int((z_max-z_min)/max(1e-3,z_min_2-z_min))+1
c     1,z_min_2-z_min

      return

  999 continue
      i_err = -1
      return

      end

c23456789012345678901234567890123456789012345678901234567890123456789012
      subroutine rmod_get_columns_2(title,i_file,icolumn_1,icolumn_2
     1,i_flag)

      implicit  none
      integer   util_len_r

      character title*(*)
      integer   i_file,icolumn_1,icolumn_2,i_flag

      character crd132*132

      if (i_file .lt. 0) return

      rewind(i_file)
      read(i_file,'(a)',err=999,end=1)crd132
      rewind(i_file)
c      print'(/,'' enter the columns for '',a)',title(1:util_len_r(title))
      if (i_flag .ne. 0)
     1print'(
     1   ''          1         2         3         4''
     1  ,''         5         6         7         8''
     1,/,'' 1234567890123456789012345678901234567890''
     1  ,''1234567890123456789012345678901234567890''
     1,/,1x,a)',crd132(1:util_len_r(crd132))

c  get the two columns

    1 continue

      if (i_flag .lt. 0) then
        icolumn_1 = 1
        icolumn_2 = 10
      endif

      print'(/,'' enter the range of columns to use defaults=''
     1,i2,1x,i2,'' for '',a)'
     1,icolumn_1,icolumn_2,title(1:util_len_r(title))
      read(*,'(a)',err=2,end=2)crd132
      if (crd132 .ne. ' ') read(crd132,*,end=2)
     1icolumn_1,icolumn_2

c      icolumn_1 = max(1,min(icolumn_1,80))
c      icolumn_2 = max(1,min(icolumn_2,80))

    2 continue

      return
  999 continue
      print'(/,'' error could not read file'')'
      return

      end

c23456789012345678901234567890123456789012345678901234567890123456789012
      subroutine rmod_get_columns(i_file,n_column
     1,ix_column,iy_column,iz_column,iv_column)

      implicit  none
      integer   util_len_r
      integer   i_file,ix_column,iy_column,iz_column,iv_column
      integer   m_column,n_column
      character crd132*132
      parameter (m_column=20)
      integer   i,i_f,lc
      real      data(m_column)

      if (i_file .ge. 0) then
      rewind(i_file)
      read(i_file,'(a)',err=999,end=1)crd132
      rewind(i_file)
      print'('' first card is'',a)',crd132(1:util_len_r(crd132))

c  get the number of columns
      call util_setr(m_column,data,-999.)
      call rmodfchr(i_f,'F',1,util_len_r(crd132),crd132)
      lc = util_len_r(crd132)
      if (i_f .ne. 0) lc = i_f - 1
c      print*,' i_f=',i_f,' lc=',lc

      read(crd132(1:lc),*,err=1001,end=1001)(data(i),i=1,m_column)

 1001 continue
      n_column = 0
      do i = 1 , m_column
c      print*,' i=',i,' data=',data(i)
        if (abs(data(i)+999.) .lt. 1.e-3) goto 1002
        n_column = i
      enddo
 1002 continue
c      print*,' n_column=',n_column
      endif

    1 continue
      ix_column = 2
      iy_column = 1
      iz_column = 5
      iv_column = 5
      ix_column = 1
      iy_column = 2
      iz_column = 3
      iv_column = 4
      print'(/,'' enter the 4 columns to use for x,y,z,v defaults=''
     1,i2,1x,i2,1x,i2,1x,i2)',ix_column,iy_column,iz_column,iv_column
      read(*,'(a)',err=2,end=2)crd132
      if (crd132 .ne. ' ') read(crd132,*,end=2)
     1ix_column,iy_column,iz_column,iv_column
      ix_column = max(1,min(ix_column,n_column))
      iy_column = max(1,min(iy_column,n_column))
      iz_column = max(1,min(iz_column,n_column))
      iv_column = max(1,min(iv_column,n_column))

    2 continue
      print'('' using columns x='',i5,'' y='',i5,'' z='',i5,'' v='',i5)'
     1,ix_column,iy_column,iz_column,iv_column

      return
  999 continue
      print'(/,'' error could not read file'')'
      return

      end

c&&&
c23456789012345678901234567890123456789012345678901234567890123456789012
      subroutine recolumnate
c  this program will recolumnate an ascii file
      implicit  none
      integer   util_len_r
      character inp_file*80,out_file*80
      integer   i_inp_file,i_out_file
      integer   i_err

      print'(
     1 /,'' this program reads a columanted ascii file ''
     1  ,'' and reorders the columns.''
     1,/,'' ''
     1,/,'' You will be asked for the input and output file names,''
     1,/,'' the number of output columns,''
     1,/,'' and the input columns to write to the output columns.''
     1,/,'' ''
     1,/,'' To add new columns with some constant value in them,''
     1,/,'' enter a value outside the range 1-20 for the column number''
     1,/,'' and that value will be entered into the column''
     1)'
c23456789012345678901234567890123456789012345678901234567890123456789012

c  open the input data file
      inp_file = 'none'
      call util_get_file_name(' enter input file name',inp_file,'inp')
      call util_open_file(i_inp_file,inp_file,'old','formatted',0,i_err)
      if (i_err .ne. 0) goto 999

c  open the output data file
      out_file = inp_file
      call util_add_ext(out_file,'out')
      call util_get_file_name(' enter output file name',out_file,'out')
      call util_open_file(i_out_file,out_file,'new','formatted',0,i_err)
      if (i_err .ne. 0) goto 999

c  sort the columns
      call recolumnate_sort_columns(i_inp_file,i_out_file,i_err)
      if (i_err .ne. 0) goto 999

c  close the files
      close(i_inp_file)
      close(i_out_file)

      print'('' successful completion '')'
      return
c      stop

  999 continue
      print'(/,'' error in recolumnate'')'
      stop
      end

c23456789012345678901234567890123456789012345678901234567890123456789012
      subroutine recolumnate_sort_columns(i_inp_file,i_out_file,i_err)
c  sort i_inp_file to i_out_file
      implicit none

      integer   util_len_r

      integer   i_inp_file,i_out_file,i_err

      integer   mx_out
      parameter (mx_out=20)

      real      x_out(mx_out)
      real      x_inp(mx_out)
      real      fx_inp(mx_out)
      integer   nx_out
c     real      ix_inp
c     real      ix_out
      integer      ix_inp
      integer      ix_out
      real      c1(mx_out)
      real      s1(mx_out)
      real      e1(mx_out)
      real      f1

      character card_132*132

      integer   i_end
      integer   n_card

      i_err = 0

      call recolumnate_get_columns(i_inp_file
     1,mx_out,nx_out,fx_inp
     1,i_err)
      if (i_err .ne. 0) goto 999

      call recolumnate_read_col_scale(mx_out,nx_out,fx_inp
     1,c1,s1,e1)

      n_card = 0

c  read each card in succession and sort it before writing out
    2 continue

      call recolumnate_read_card(i_inp_file,i_err,i_end,card_132)
      if (i_err .ne. 0) goto 999
      if (i_end .ne. 0) goto 1

      n_card = n_card + 1

      if (n_card .le. 2) print'(a)',card_132(1:util_len_r(card_132))
      call util_setr(mx_out,x_inp,0.)
      call util_setr(mx_out,x_out,0.)

      read(card_132,*,err=999,end=3)
     1(x_inp(ix_out),ix_out=1,mx_out)

    3 continue

c  sort the columns
      do ix_out = 1 , nx_out

        ix_inp = nint(fx_inp(ix_out))

        if (ix_inp .gt. 0 .and. ix_inp .le. mx_out) then

          x_out(ix_out) = x_inp(ix_inp)

        else    ! if (ix_inp .gt. 0 .and. ix_inp .le. mx_out) then

          x_out(ix_out) = fx_inp(ix_out)

        endif    ! if (ix_inp .gt. 0 .and. ix_inp .le. mx_out) then

        if (e1(ix_out) .eq. 0.) then

          f1 = 1.

        elseif (e1(ix_out) .eq. 1.) then    ! if (e1(ix_out) .eq. 0.) th

          f1 = x_out(ix_out)

        else    ! if (e1(ix_out) .eq. 0.) then

          f1 = x_out(ix_out) ** e1(ix_out)

        endif    ! if (e1(ix_out) .eq. 0.) then

        x_out(ix_out) = c1(ix_out) + s1(ix_out) * f1

c      if(n_card.eq.1)print*,' ix_out=',ix_out
c     1,' fx_inp=',fx_inp(ix_out),' x_out=',x_out(ix_out)

      enddo    ! do ix_out = 1 , nx_out

c  write the card
      call util_write_card(i_out_file,nx_out,x_out)
      if (n_card .le. 2)
     1call util_write_card(6         ,nx_out,x_out)

      goto 2

    1 continue


      print'(/,'' number of cards sorted='',i8)',n_card

      return

  999 continue
      print'(/,'' error in recolumnate_sort_columns'')'
      i_err = -1
      return

      end

c23456789012345678901234567890123456789012345678901234567890123456789012
      subroutine recolumnate_get_columns(i_inp_file
     1,mx_out,nx_out,fx_inp
     1,i_err)
      implicit  none

      integer   util_len_r

      integer   i_inp_file
      integer   mx_out,nx_out
      real      fx_inp(1)
      integer   i_err

      character card_132*132
      character card_80*80
      integer   i_end
      integer   ix_out

      i_err = 0
c  read the first card and print it
      rewind (i_inp_file)
      call recolumnate_read_card(i_inp_file,i_err,i_end,card_132)
      if (i_err .ne. 0) goto 999
      if (i_end .ne. 0) goto 999
      rewind (i_inp_file)

      print'(a)',card_132(1:util_len_r(card_132))
      print'(/,'' enter the number of output columns'')'
      read(*,*)nx_out
      nx_out = min(mx_out,nx_out)

      print'(/,'' enter the input column for each output column''
     1,/,'' enter a value <=0 or >'',i5
     1,/,'' to fill in a new output column with that value''
     1)'
     1,mx_out

      do ix_out = 1 , nx_out

    1 continue

        fx_inp(ix_out) = ix_out
        card_80 = ' '
        print'(/,'' enter the column for output column='',i5
     1,'' default='',i5)'
     1,ix_out,nint(fx_inp(ix_out))
        read(*,'(a)',end=2,err=1)card_80
        if (card_80 .ne. ' ') read(card_80,*,end=2,err=1)fx_inp(ix_out)

    2   continue

      enddo    ! do ix_out = 1 , nx_out

      return

  999 continue
      print'(/,'' error in recolumnate_get_columns'')'
      i_err = -1
      return

      end

c23456789012345678901234567890123456789012345678901234567890123456789012
      subroutine recolumnate_read_card(i_file,i_err,i_end,card_132)
      implicit  none
      integer   i_file,i_err,i_end
      character card_132*(*)
      i_err = 0
      i_end = 0
      card_132 = ' '
      read(i_file,'(a)',err=999,end=1)card_132
      return
    1 continue
      i_end = 1
      return
  999 continue
      i_err = -1
      return
      end

c23456789012345678901234567890123456789012345678901234567890123456789012
      subroutine recolumnate_read_col_scale(mx_out,nx_out,fx_inp
     1,c1,s1,e1)

      implicit none
      integer  mx_out,nx_out
      real     fx_inp(1)
      real     c1(1),s1(1),e1(1)
      integer  ix_out,ix_inp
      character card_80*80

      print'(/,'' This option operates on the column values''
     1,'' using an equation of the form''
     1,/,'' new value = c1 + s1 * old value ** e1''
     1)'

      do ix_out = 1 , nx_out

    1 continue

      c1(ix_out) = 0.
      s1(ix_out) = 1.
      e1(ix_out) = 1.

        ix_inp = nint(fx_inp(ix_out))

        if (ix_inp .gt. 0 .and. ix_inp .le. mx_out) then

      card_80 = ' '
      print'(/,'' enter c1='',f10.2
     1,'' s1='',f10.2,'' e1='',f10.2
     1,'' for column='',i4)'
     1,c1(ix_out),s1(ix_out),e1(ix_out),ix_out
      read(*,'(a)',err=1,end=2)card_80
      if (card_80 .ne. ' ') read(card_80,*,err=1,end=2)
     1c1(ix_out),s1(ix_out),e1(ix_out)
    2 continue
        endif    ! if (ix_inp .gt. 0 .and. ix_inp .le. mx_out) then

      enddo

      return
      end

c23456789012345678901234567890123456789012345678901234567890123456789012
      subroutine rmod_3dl_to_2dl(mod_type
     1,x_min,x_max
     1,y_min,y_max
     1,z_min,z_max
     1,nx_vel,x0_vel,dx_vel
     1,ny_vel,y0_vel,dy_vel
     1,nz_vel,z0_vel,dz_vel
     1,v_grd
     1,mb,mxb,nb,imb,itb,ixb,nxb,xb,zb
     1,nv,imv,itv,ixv,nxv,xv,yv,zv,vel
     1,ncv,icv,xcv,ycv,zcv
     1,m_work,work)
c  convert a 3d layered model to a 2d layered model
      implicit  none

      character mod_type*(*)

      integer   nx_vel
      real      x0_vel,dx_vel

      integer   ny_vel
      real      y0_vel,dy_vel

      integer   nz_vel
      real      z0_vel,dz_vel

      real      v_grd(nx_vel,ny_vel,1)

      integer   mb,mxb,nb
      integer   imb(1),itb(1),ixb(1),nxb(1)
      real      xb(1),zb(1)

      integer   nv
      integer   imv(1),itv(1),ixv(1),nxv(1)
      real      xv(1),yv(1),zv(1),vel(1)

      integer   ncv
      integer   icv(1)
      real      xcv(1),ycv(1),zcv(1)

      real      x_min,x_max
      real      y_min,y_max
      real      z_min,z_max

      integer   m_work
      real      work(1)

      real      xl_end,xr_end,dx_end
      integer   i,i1,i2,ix1,ix2,ib,jb

      integer   is_hor

      integer   nx_hor
      real      x0_hor,dx_hor

      integer   ny_hor
      real      y0_hor,dy_hor

      integer   nz_hor
      real      z0_hor,dz_hor

      integer   ns_hor
      real      s0_hor,ds_hor

      integer   nx_lay
      real      x0_lay,dx_lay

      integer   ny_lay
      real      y0_lay,dy_lay

      integer   nz_lay
      real      z0_lay,dz_lay

      integer   ns_vel

      real      x_hor,y_hor
      character crd80*80

c23456789012345678901234567890123456789012345678901234567890123456789012
      print'(/,'' This option creates a 2D layered model by taking ''
     1,/,'' a slice through a 3D layered model.''
     1,/
     1,/,'' This slice may be in any direction but must be straight.''
     1,/,'' it is defined by the number of points along the slice,''
     1/,'' and the x,y (inline,crossline) endpoints along the 2D slice''
     1,/
     1,/,'' You will be asked for the first and last X,Y values of the''
     1,'' 2D slice,''
     1,/,'' the number of output points along the slice''
     1,'' and the output endpoint values.''
     1,/
     1,/,'' Since you are creating a 2D layered model from a 3D ''
     1,/,'' layered model you must redefine the vertical grid.''
     1)'

      if (mod_type .ne. 'G3DL') then
        print'('' Model mod_type must be G3DL for this option ''
     1,'' mod_type='',a16)',mod_type
        call rmod_pause(' ',' ')
        return
      endif

      crd80 = ' '
      write(crd80,'(a)')' RMOD createing 2d layers from 3D layers'
      call rmod_title_to_history(crd80)

c  get the 2d model position
      call rmod_3dl_to_2dl_limits(xl_end,xr_end
     1,x_min,x_max,y_min,y_max,z_min,z_max,nb
     1,nx_vel,x0_vel,dx_vel
     1,ny_vel,y0_vel,dy_vel
     1,nz_vel,z0_vel,dz_vel
     1,nx_lay,x0_lay,dx_lay
     1,ny_lay,y0_lay,dy_lay
     1,nz_lay,z0_lay,dz_lay
     1,nx_hor,x0_hor,dx_hor
     1,ny_hor,y0_hor,dy_hor
     1,ns_hor,s0_hor,ds_hor
     1,ns_vel)

c  fill horizon values
      dz_hor = max(.001,min(dz_vel,abs(x_max-x_min)/100.))
      jb = 1
      call util_setr(nb,ixb,0  )
      call util_setr(nb,nxb,ns_hor)

      dx_end = (xr_end - xl_end) / max(1,ns_hor-1)
      do ib = 1 , nb

        do is_hor = 1 , ns_hor
          x_hor = (is_hor - 1) * dx_hor + x0_hor
          y_hor = (is_hor - 1) * dy_hor + y0_hor
          call util_interpolate(
     1 nx_lay,x0_lay,dx_lay
     1,ny_lay,y0_lay,dy_lay
     1,  1,   0.,   1.
     1,v_grd(1,1,ib)
     1,  1,   x_hor,dx_hor
     1,  1,   y_hor,dy_hor
     1,  1,   0.,   1.
     1,zb(jb+is_hor-1))
        enddo    ! do is_hor = 1 , ns_hor

        call util_setr(ns_hor,itb(jb),ib)

c        call util_line(nxb(ib),xb(jb),s0_hor,ds_hor)
        call util_line(nxb(ib),xb(jb),xl_end,dx_end)

        if (ib .gt. 1) then

          ixb(ib) = ixb(ib-1) + nxb(ib-1)

          do i = 1 , nxb(ib)

            zb(ixb(ib)+i) = max(zb(ixb(ib)+i),zb(ixb(ib-1)+i)+dz_hor)

          enddo    ! do i = 1 , nxb(ib)

        endif

        jb = jb + nxb(ib)
        imb(ib) = ib*10

c      print'('' ib='',i5,'' imb='',i5,'' ixb='',i5,'' nxb='',i5
c     1,'' itb='',i5)',ib,imb(ib),ixb(ib),nxb(ib),itb(ixb(ib)+1)

      enddo    ! do ib = 1 , nb

      do i = 1 , ncv

        i1 = max(i-1 , 1)
        i2 = min(i1+1,nb)
        ix1 = ixb(i1) + nxb(i1) / 2 + 1
        ix2 = ixb(i2) + nxb(i2) / 2 + 1

        if (i .eq. 1) then

          z0_hor = (zb(ix1) + z_min) / 2.

        elseif (i .eq. nb+1) then

          z0_hor = (zb(ix2) + z_max) / 2.

        else

          z0_hor = (zb(ix1) + zb(ix2)) / 2

        endif

        xcv(i) = xb(ix1)
        zcv(i) = z0_hor

      enddo    ! do i = 1 , ncv

      call rmod_3dv_to_2dv(nv,imv,itv,ixv,nxv,xv,yv,zv,vel
     1,m_work,work
     1,nx_hor,x0_hor,dx_hor
     1,ny_hor,y0_hor,dy_hor
     1,ns_hor,s0_hor,ds_hor
     1,ns_vel
     1,xl_end,xr_end)

      mod_type = 'LAYER'

c      print*,' nb=',nb,' nv=',nv

      return

  999 continue
      call rmod_pause(' error in rmod_3dl_to_2dl work allocation',' ')
      return

      end

c23456789012345678901234567890123456789012345678901234567890123456789012
      subroutine rmod_3dl_to_2dl_limits(xl_end,xr_end
     1,x_min,x_max,y_min,y_max,z_min
     1,z_max,nb
     1,nx_vel,x0_vel,dx_vel
     1,ny_vel,y0_vel,dy_vel
     1,nz_vel,z0_vel,dz_vel
     1,nx_lay,x0_lay,dx_lay
     1,ny_lay,y0_lay,dy_lay
     1,nz_lay,z0_lay,dz_lay
     1,nx_hor,x0_hor,dx_hor
     1,ny_hor,y0_hor,dy_hor
     1,ns_hor,s0_hor,ds_hor
     1,ns_vel)
      character crd80*80
      real      xl_end,xr_end,y0_end
      nx_lay = nx_vel
      x0_lay = x0_vel
      dx_lay = dx_vel
      ny_lay = ny_vel
      y0_lay = y0_vel
      dy_lay = dy_vel
      nz_lay = nz_vel
      nb = nz_vel

      print'('' layered model limits''
     1,/,'' x_min='',f10.2,'' x_max='',f10.2
     1,/,'' y_min='',f10.2,'' y_max='',f10.2)'
     1,x_min,x_max,y_min,y_max

    1 continue

      x0_hor = x_min
      x1_hor = x_max
      y0_hor = (y_max + y_min) / 2.
      y1_hor = (y_max + y_min) / 2.

      print'(/,'' enter the first x and y values along the 2D slice''
     1,/,'' defaults x_first='',f10.2,'' y_first='',f10.2)'
     1,x0_hor,y0_hor
      crd80 = ' '
      read(*,'(a)',err=1,end=2)crd80
      if (crd80 .ne. ' ') read(crd80,*,err=1,end=2)x0_hor,y0_hor
    2 continue


      print'(/,'' enter the last  x and y values along the 2D slice''
     1,/,'' defaults x_last ='',f10.2,'' y_last ='',f10.2)'
     1,x1_hor,y1_hor
      crd80 = ' '
      read(*,'(a)',err=1,end=3)crd80
      if (crd80 .ne. ' ') read(crd80,*,err=1,end=3)x1_hor,y1_hor
    3 continue

      ns_hor = 101

      print'(/,'' enter the number of output points along the 2D slice''
     1,'' default='',i8)',ns_hor
      crd80 = ' '
      read(*,'(a)',err=1,end=4)crd80
      if (crd80 .ne. ' ') read(crd80,*,err=1,end=4)ns_hor
    4 continue

      if (x0_hor .eq. x1_hor) then
        xl_end = y0_hor
        xr_end = y1_hor
        y0_end  = (x0_hor + x1_hor) / 2.
      else
        xl_end = x0_hor
        xr_end = x0_hor
        y0_end  = (y0_hor + y1_hor) / 2.
      endif
      print'(/,'' enter the output 2D slice x endpoint values''
     1,'' defaults='',f10.2,1x,f10.2
     1,/,'' and the output line y value default='',f10.2)'
     1,xl_end,xr_end,y0_end
      crd80 = ' '
      read(*,'(a)',err=1,end=15)crd80
      if (crd80 .ne. ' ') read(crd80,*,err=1,end=5)
     1 xl_end,xr_end,y0_end
   15 continue

      ns_hor = max(2,ns_hor)

      dx_hor = (x1_hor - x0_hor) / (ns_hor - 1)
      dy_hor = (y1_hor - y0_hor) / (ns_hor - 1)

      s0_hor = sqrt(x0_hor**2+y0_hor**2)
      s1_hor = sqrt(x1_hor**2+y1_hor**2)
      ds_hor = (s1_hor - s0_hor) / (ns_hor - 1)

      ns_vel = ns_hor / 2 + 1

c  read new depth grid limits
    5 continue
      nz_vel = 101
      z0_vel= z_min
      dz_vel = (z_max - z_min) / (nz_vel - 1)
      print'('' depth range= '',f12.4,'' to '',f12.4
     1,/,'' Enter nz,z_min,zinc for grid defaults='',i5,f12.4,1x,f12.4)'
     1,z_min,z_max,nz_vel,z0_vel,dz_vel
      crd80 = ' '
      read(*,'(a)',err=5,end=6)crd80
      if (crd80 .ne. ' ')read(crd80,*,err=5,end=6)nz_vel,z0_vel,dz_vel
    6 continue

      s1_hor = (ns_hor - 1) * ds_hor + s0_hor
      x_min  = xl_end
      x_max  = xr_end
      y_min  = y0_end
      y_max  = y0_end

      nx_vel = ns_hor
      x0_vel = xl_end
      dx_vel = (xr_end - xl_end) / max(1,nx_vel-1)
c      x0_vel = s0_hor
c      dx_vel = ds_hor

      ny_vel = 1
      y0_vel = y0_end
      dy_vel = 1.

      z_max = max(z_max,(nz_vel-1)*dz_vel+z0_vel)

      return
      end

c23456789012345678901234567890123456789012345678901234567890123456789012
      subroutine rmod_3dg_to_2dg(mod_type
     1,x_min,x_max
     1,y_min,y_max
     1,m_vel
     1,nx_vel,x0_vel,dx_vel
     1,ny_vel,y0_vel,dy_vel
     1,nz_vel,z0_vel,dz_vel
     1,vel
     1,m_work,work)
c  interpolate a crooked line from a 3d gridded model
      implicit none

      real      util_invert_1

      character mod_type*(*)

      real      x_min,x_max
      real      y_min,y_max

      integer   m_vel

      integer   nx_vel
      real      x0_vel,dx_vel

      integer   ny_vel
      real      y0_vel,dy_vel

      integer   nz_vel
      real      z0_vel,dz_vel

      real      vel(1)

      integer   m_work
      real      work(1)

      integer   i_err

      integer   nx_tmp
      real      x0_tmp,dx_tmp

      integer   ny_tmp
      real      y0_tmp,dy_tmp

      integer   nz_tmp
      real      z0_tmp,dz_tmp

      integer   ns_xy
      real      ds_xy

      integer   i_vel,n_vel
      integer   i_work,n_work
      integer   i_work_i,i_work_n

      integer   ix_vel,iy_vel,iz_vel,is_xy
      character crd80*80

      print'(/,'' This option interpolates from a 3D gridded model ''
     1,''to a crooked line''
     1,/,'' You should make sure the X and Y dimensions are in ''
     1,/,'' similar units such as XBASEMENT and YBASEMENT.''
     1,/
     1,/,'' You will be asked to enter the spacing between ''
     1,'' output grid points, and the ''
     1,/,'' name of a file containing the X,Y locations ''
     1,''defining the output grid locations.''
     1,/
     1,/,'' If you enter NONE for this file name, you will be asked''
     1,/,'' for the number of points you want to enter by hand''
     1,/,'' and then the actual pairs of X,Y values.''
     1,/
     1,/,'' After the new grid values are computed  you will be asked''
     1,/,'' for the new values for the model X and Y limits''
     1,/
     1 /,'' The current X and Y model limits are''
     1,/
     1,/,'' xmin='',f10.2,'' xmax='',f10.2,'' nx='',i8,'' dx='',f10.4
     1,/,'' ymin='',f10.2,'' ymax='',f10.2,'' ny='',i8,'' dy='',f10.4
     1)'
     1,x0_vel,(nx_vel-1)*dx_vel+x0_vel,nx_vel,dx_vel
     1,y0_vel,(ny_vel-1)*dy_vel+y0_vel,ny_vel,dy_vel

      if (mod_type(1:4) .ne. 'GRID') then
        print'('' Model mod_type must be GRID mod_type='',a16)',mod_type
        call rmod_pause(' ',' ')
        return
      endif

      ds_xy = dx_vel
      call rmod_3dg_to_2dg_input(nx_vel,x0_vel,dx_vel,y0_vel
     1,ds_xy,ns_xy,m_work,work,i_err)
      if (i_err .ne. 0) goto 999

      n_vel = nx_vel * ny_vel * nz_vel
      n_work = 4 * (nx_vel + ny_vel + nz_vel)

      call util_wors(i_work_i,i_work_n,m_work)
      call util_work(i_work_i,i_work_n,is_xy,ns_xy*2)
      call util_work(i_work_i,i_work_n,i_vel,n_vel)
      call util_work(i_work_i,i_work_n,i_work,n_work)
      call util_worc(i_work_i,i_work_n,i_err)

      if (i_err .ne. 0) then
        print'('' insufficient work memory''
     1,/,'' m_vel='',i8,'' nx_vel*ny_vel*nz_vel='',i8,'' i_err='',i8)'
     1,m_vel,nx_vel*ny_vel*nz_vel,i_err
        goto 999
      endif

      call util_copy(n_vel,vel,work(i_vel))

      print'(/,'' number of X,Y pairs='',i8
     1,/,'' output grid spacing='',f10.4
     1)',ns_xy,ds_xy

      nx_tmp = nx_vel
      x0_tmp = x0_vel
      dx_tmp = dx_vel

      ny_tmp = ny_vel
      y0_tmp = y0_vel
      dy_tmp = dy_vel

      call rmod_3dg_to_2dg_compute_v(ds_xy,ns_xy,work(is_xy)
     1,nx_vel,x0_vel,dx_vel
     1,ny_vel,y0_vel,dy_vel
     1,nz_vel,z0_vel,dz_vel
     1,work(i_vel)
     1,m_vel/nz_vel
     1,vel
     1,n_work,work(i_work)
     1,i_err)
      if (i_err .ne. 0) goto 999

      print'(
     1 /,'' The current X and Y model limits are''
     1,/
     1,/,'' xmin='',f10.2,'' xmax='',f10.2,'' nx='',i8,'' dx='',f10.4
     1,/,'' ymin='',f10.2,'' ymax='',f10.2,'' ny='',i8,'' dy='',f10.4
     1)'
     1,x0_tmp,(nx_tmp-1)*dx_tmp+x0_tmp,nx_tmp,dx_tmp
     1,y0_tmp,(ny_tmp-1)*dy_tmp+y0_tmp,ny_tmp,dy_tmp

      call rmod_get_real_2('the new X model limits'
     1,x0_vel,x_min
     1,(nx_vel-1)*dx_vel+x0_vel,x_max)

      call rmod_get_real('the new Y model location'
     1,y0_vel,y_min)

      x0_vel = x_min
      dx_vel = (x_max - x_min) / max(nx_vel-1,1)

      ny_vel = 1
      y0_vel = y_min
      dy_vel = 1.
      y_max  = y_min

      crd80 = ' '
      write(crd80,'('' RMOD interpolating crooked gridded model'')')
      call rmod_title_to_history(crd80)

      crd80 = ' '
      write(crd80,'('' ds_xy:'',f10.2)')ds_xy
      call rmod_card_to_history(crd80)

      crd80 = ' '
      write(crd80,'(''  input nx_vel:'',i8,'' x0_vel:'',f10.2
     1,'' dx_vel:'',f10.2)')nx_tmp,x0_tmp,dx_tmp
      call rmod_card_to_history(crd80)

      crd80 = ' '
      write(crd80,'('' output nx_vel:'',i8,'' x0_vel:'',f10.2
     1,'' dx_vel:'',f10.2)')nx_vel,x0_vel,dx_vel
      call rmod_card_to_history(crd80)

      crd80 = ' '
      write(crd80,'(''  input ny_vel:'',i8,'' y0_vel:'',f10.2
     1,'' dy_vel:'',f10.2)')ny_tmp,y0_tmp,dy_tmp
      call rmod_card_to_history(crd80)

      crd80 = ' '
      write(crd80,'('' output ny_vel:'',i8,'' y0_vel:'',f10.2
     1,'' dy_vel:'',f10.2)')ny_vel,y0_vel,dy_vel
      call rmod_card_to_history(crd80)

c      crd80 = ' '
c      write(crd80,'(''  input nz_vel:'',i8,'' z0_vel:'',f10.2
c     1,'' dz_vel:'',f10.2)')nz_tmp,z0_tmp,dz_tmp
c      call rmod_card_to_history(crd80)

c      crd80 = ' '
c      write(crd80,'('' output nz_vel:'',i8,'' z0_vel:'',f10.2
c     1,'' dz_vel:'',f10.2)')nz_vel,z0_vel,dz_vel
c      call rmod_card_to_history(crd80)

      return

  999 continue
      call rmod_pause(' error in rmod_3dg_to_2dg',' ')
      return

      end

c23456789012345678901234567890123456789012345678901234567890123456789012
      subroutine rmod_3dg_to_2dg_compute_v(ds_xy,ns_xy,s_xy
     1,nx_vel,x0_vel,dx_vel
     1,ny_vel,y0_vel,dy_vel
     1,nz_vel,z0_vel,dz_vel
     1,v_inp
     1,mv_out
     1,v_out
     1,m_work,work
     1,i_err)
      implicit none

      real     util_invert_1

      integer  ns_xy
      real     s_xy(2,ns_xy)

      integer  nx_vel
      real     x0_vel,dx_vel

      integer  ny_vel
      real     y0_vel,dy_vel

      integer  nz_vel
      real     z0_vel,dz_vel

      real     v_inp(nz_vel,nx_vel,ny_vel)

      integer  mv_out
      real     v_out(nz_vel,1)

      integer  m_work
      real     work(1)

      integer  i_err

      integer   is,js
      real      ds_xy,ds_12,ds_01
      real      s0,s1,s2
      real      x0,y0,x1,y1,x2,y2
      real      dx_ds,dy_ds

c  add the first point

      i_err = 0
      s0 = 0.    ! total distance
      js = 0     ! output counter
      is = 1     ! input counter

c  get the characteristics of the first pair of points
      call rmod_3dg_to_2dg_ds(is,s_xy
     1,s1,s2,x1,y1,x2,y2,dx_ds,dy_ds,ds_12)

      x0 = x1
      y0 = y1

c  cycle over the X,Y points
    1 continue

c  add the point x0,y0
      js = js + 1

      call util_interpolate_1(
     1 nz_vel,z0_vel,dz_vel
     1,nx_vel,x0_vel,dx_vel
     1,ny_vel,y0_vel,dy_vel
     1,v_inp
     1,nz_vel,z0_vel,dz_vel
     1,     1,x0    ,dx_vel
     1,     1,y0    ,dy_vel
     1,v_out(1,js)
     1,m_work,work
     1,i_err)
      if (i_err .ne. 0) goto 999

      if (js .le. 100 .or. mod(js,100) .eq. 0)
     1print'('' j='',i5,'' s='',f8.0
     1,'' x='',f8.0,'' y='',f8.0,'' v='',f8.0,1x,f8.0)'
     1,js,s0,x0,y0
     1,util_invert_1(v_out(     1,js))
     1,util_invert_1(v_out(nz_vel,js))

c  if the next output point is within s2 or we have more input points
c  proceed to the next output point
      if (is .lt. ns_xy-1 .or. s0+ds_xy .le. s2) then

c  increment the distance counter for next output point
        s0 = s0 + ds_xy

c  if the next output point is still within this pair of input points
c  increment x0,y0 and get the next output point
        if (s0 .le. s2) then

          x0 = x0 + dx_ds * ds_xy
          y0 = y0 + dy_ds * ds_xy

        else    ! if (s0 .le. s2) then
c  otherwise we need to increment is etc and compute new values for s0,x
c  taking in to account that s0 may be slightly past x1,y1

          is  = is + 1

          call rmod_3dg_to_2dg_ds(is,work
     1,s1,s2,x1,y1,x2,y2,dx_ds,dy_ds,ds_12)

          ds_01 = s0 - s1
          x0    = x1 + dx_ds * ds_01
          y0    = y1 + dy_ds * ds_01

        print'('' i='',i5,'' j='',i5
     1,'' x='',f8.0,'' y='',f8.0,'' s1='',f8.2,'' s2='',f8.2
     1,'' ds_12='',f8.2,'' ds_01='',f8.2)'
     1,is,js,x2,y2,s1,s2,ds_12,ds_01

        endif    ! if (s0 .le. s2) then

c        print'('' i='',i5,'' j='',i5
c     1,'' x='',f8.0,'' y='',f8.0,'' s1='',f8.2,'' s2='',f8.2
c     1,'' ds_12='',f8.2,'' ds_01='',f8.2)'
c     1,is,js,x2,y2,s1,s2,ds_12,ds_01

c  go back for the next output point
        goto 1

      endif    ! if (is .lt. ns_xy-1 .or. s0+ds_xy .le. s2) then

      nx_vel = js

      print'(
     1 /,'' The total number of output points='',i5
     1,/,'' with a total distance of        s='',f10.2)'
     1,nx_vel,s0

      return

  999 continue
      call rmod_pause(' error in rmod_3dg_to_2dg_compute_v',' ')
      i_err = -1
      return

      end

c23456789012345678901234567890123456789012345678901234567890123456789012
      subroutine rmod_3dg_to_2dg_input(nx_vel,x0_vel,dx_vel,y0_vel
     1,ds_xy,ns_xy,ms_xy,s_xy,i_err)
c  read in the x,y points
      implicit  none

      integer   util_len_r

      integer   nx_vel
      real      x0_vel,dx_vel,y0_vel

      real      ds_xy
      integer   ns_xy

      integer   ms_xy
      real      s_xy(2,ms_xy/2)

      integer   i_err

      character inp_file*80
      integer   i_file
      integer   is,l_xy
      real      x0,y0,x1,y1
      logical   rmod_char_end
      character crd80*80

  101 continue

      call rmod_get_real(
     1'the distance between output points',ds_xy,ds_xy)

      print'(/,'' enter the name of the X,Y control point file''
     1,/,'' enter NONE to enter pairs of X,Y values by hand''
     1)'
      inp_file = 'NONE'
      call util_get_file_name(' Enter file name',inp_file,'xy')

      if (inp_file(1:4) .eq. 'NONE') then

        i_file = 6
        print'(/,'' enter the X,Y control points one pair at a time''
     1,/,'' Enter END or QUIT when done'')'

      else    ! if (inp_file(1:4) .eq. 'NONE') then

          call rmod_inquire_exist(inp_file,*101)
          call rmodopen(i_file,inp_file,'FORMATTED','OLD'
     1,'SEQUENTIAL','IEEE',0,i_err)
          if (i_err .ne. 0) goto 999

      endif    ! if (inp_file(1:4) .eq. 'NONE') then

      l_xy = ms_xy / 2

      ns_xy = 0

      do is = 1 , l_xy

        if (is .eq. 1) then

          x0 = x0_vel
          y0 = y0_vel

        elseif (is .eq. 2) then

          x0 = (nx_vel - 1) * dx_vel + x0_vel
          y0 = y0

        else    ! if (is .eq. 1) then

          x0 = x0 + (x1 - x0)
          y0 = y0 + (y1 - y0)

        endif    ! if (is .eq. 1) then

        if (inp_file(1:4) .eq. 'NONE')
     1print'(/,'' enter the X,Y values for node='',i8
     1,'' defaults='',f10.2,1x,f10.2)',is,x0,y0

        x1 = x0
        y1 = x0

  301   continue
        crd80 = ' '

        if (inp_file(1:4) .eq. 'NONE') then

          read(i_file,'(a)',err=301,end=303)crd80

        else    ! if (inp_file(1:4) .eq. 'NONE') then

          read(i_file,'(a)',err=301,end=302)crd80

        endif    ! if (inp_file(1:4) .eq. 'NONE') then

        if (rmod_char_end(crd80)) goto 302
        if (crd80 .ne. ' ') read(crd80,*,err=301,end=303)x0,y0

  303   continue

        ns_xy = ns_xy + 1
        s_xy(1,ns_xy) = x0
        s_xy(2,ns_xy) = y0

      enddo    ! do is = 1 , ms_xy / 2

  302 continue
      if (inp_file(1:4) .ne. 'NONE') call rmodclof(inp_file)

      return
  999 continue
      i_err = -1
      return

      end

c23456789012345678901234567890123456789012345678901234567890123456789012
      subroutine rmod_3dg_to_2dg_ds(is,s_xy
     1,s1,s2,x1,y1,x2,y2,dx_ds,dy_ds,ds_12)
      implicit none

      integer  is
      real     s_xy(2,is+1)

      real     s1,s2,x1,y1,x2,y2,dx_ds,dy_ds,ds_12

      if (is .eq. 1) then
        s1 = 0
      else
        s1 = s2
      endif

      x1    = s_xy(1,is)
      y1    = s_xy(2,is)
      x2    = s_xy(1,is+1)
      y2    = s_xy(2,is+1)

      ds_12 = sqrt((x2-x1)**2+(y2-y1)**2)
      s2 = s1 + ds_12

      dx_ds = (x2 - x1) / ds_12
      dy_ds = (y2 - y1) / ds_12

      return
      end

c23456789012345678901234567890123456789012345678901234567890123456789012
      logical function rmod_char_end(card)
c  set logical variable to .true.
c  if character string starts with E,e,Q,q
      implicit  none

      character card*(*)

      if (card(1:1) .eq. 'E'
     1.or. card(1:1) .eq. 'e'
     1.or. card(1:1) .eq. 'Q'
     1.or. card(1:1) .eq. 'q'
     1) then

        rmod_char_end = .true.

      else

        rmod_char_end = .false.

      endif

      return
      end
c23456789012345678901234567890123456789012345678901234567890123456789012
      subroutine rmod_condition_boundaries(mod_type
     1,x_min,x_max
     1,z_min,z_max
     1,m_bnd,mx_bnd,n_bnd,im_bnd,it_bnd,ix_bnd,nx_bnd,x_bnd,z_bnd
     1,m_work,work
     1,i_err)
c  compare every pair of boundaries for those which may overlap
      implicit  none

      integer   util_len_r

      character mod_type*(*)

      real      x_min,x_max
      real      z_min,z_max

      integer   m_bnd,mx_bnd
      integer   n_bnd
      integer   im_bnd(n_bnd),it_bnd(n_bnd)
      integer   ix_bnd(n_bnd),nx_bnd(n_bnd)
      real      x_bnd(1),z_bnd(1)
      integer   m_work
      real      work(m_work)
      integer   i_err

      integer   i_work_i,i_work_n
      integer   i_work  ,n_work  ,u_work

      integer   lx_bnd
      integer   i_im_bnd,n_im_bnd
      integer   i_it_bnd,n_it_bnd
      integer   i_ix_bnd,n_ix_bnd
      integer   i_nx_bnd,n_nx_bnd
      integer   i_x_bnd ,n_x_bnd
      integer   i_z_bnd ,n_z_bnd

      real      z_tol

      i_err = 0

      print'(/,'' this conditions 2D boundaries to remove ''
     1,''overlapping portions.''
     1,/,'' you will be asked for a vertical distance for''
     1,/,'' deciding when points overlap''
     1,//,'' current model limits are:''
     1,/,'' x_min='',f10.2,'' x_max='',f10.2
     1,/,'' z_min='',f10.2,'' z_max='',f10.2
     1)'
     1,x_min,x_max
     1,z_min,z_max

      if (mod_type(1:5) .ne. 'LAYER') then
        print'('' mode mod_type must be LAYER mod_type='',a)'
     1,mod_type(1:util_len_r(mod_type))
        return
      endif

c  copy the input boundaries to work space
      lx_bnd = ix_bnd(n_bnd) + nx_bnd(n_bnd)

      n_im_bnd = n_bnd
      n_it_bnd = lx_bnd
      n_ix_bnd = n_bnd
      n_nx_bnd = n_bnd
      n_x_bnd  = lx_bnd
      n_z_bnd  = lx_bnd

      call util_wors(i_work_i,i_work_n,m_work)
      call util_work(i_work_i,i_work_n,i_im_bnd,n_im_bnd)
      call util_work(i_work_i,i_work_n,i_it_bnd,n_it_bnd)
      call util_work(i_work_i,i_work_n,i_ix_bnd,n_ix_bnd)
      call util_work(i_work_i,i_work_n,i_nx_bnd,n_nx_bnd)
      call util_work(i_work_i,i_work_n,i_x_bnd ,n_x_bnd )
      call util_work(i_work_i,i_work_n,i_z_bnd ,n_z_bnd )
      call util_woru(i_work_i,i_work_n,u_work)
      call util_worc(i_work_i,i_work_n,i_err)
      call util_worl(i_work_i,i_work_n,n_work)
      call util_work(i_work_i,i_work_n,i_work,n_work)

      if (i_err .ne. 0) then
        print'('' need more work space have='',i6,'' and need='',i6)'
     1,m_work,u_work
        goto 999
      endif    ! if (i_err .ne. 0) then

c scale x's from x_min,x_max to 0.,1.
c      call rmod_scale_2(lx_bnd,x_bnd,x_min,x_max,0.,1.)

c scale z's from z_min,z_max to 0.,1.
c      call rmod_scale_2(lx_bnd,z_bnd,z_min,z_max,0.,1.)

      call rmod_compute_tolerance(lx_bnd,z_bnd,z_tol)
      call rmod_get_real(
     1'the vertical tolerance for deleteing points'
     1,z_tol,z_tol)

      call rmod_condition_boundaries_n(z_tol
     1,m_bnd,mx_bnd,n_bnd,im_bnd,it_bnd,ix_bnd,nx_bnd,x_bnd,z_bnd
     1,work(i_im_bnd),work(i_it_bnd),work(i_ix_bnd),work(i_nx_bnd)
     1,work(i_x_bnd),work(i_z_bnd)
     1,n_work,work(i_work)
     1,i_err)
      if (i_err .ne. 0) goto 999

c scale x's from 0.,1. to x_min,x_max
c      call rmod_scale_2(lx_bnd,x_bnd,0.,1.,x_min,x_max)

c scale z's from 0.,1. to z_min,z_max
c      call rmod_scale_2(lx_bnd,z_bnd,0.,1.,z_min,z_max)

      return

  999 continue
      print'(/,'' error in rmod_condition_boundaries'')'
      i_err = -1
      return

      end

c23456789012345678901234567890123456789012345678901234567890123456789012
      subroutine rmod_condition_boundaries_n(z_tol
     1,m_bnd,mx_bnd,n_bnd,im_bnd,it_bnd,ix_bnd,nx_bnd,x_bnd,z_bnd
     1,                   im_tmp,it_tmp,ix_tmp,nx_tmp,x_tmp,z_tmp
     1,m_work,work
     1,i_err)
c  compare every pair of boundaries for those which may overlap
      implicit  none

      integer   util_len_r

      real      z_tol

      integer   m_bnd,mx_bnd

      integer   n_bnd
      integer   im_bnd(n_bnd),it_bnd(1)
      integer   ix_bnd(n_bnd),nx_bnd(n_bnd)
      real      x_bnd(1),z_bnd(1)

      integer   im_tmp(n_bnd),it_tmp(1)
      integer   ix_tmp(n_bnd),nx_tmp(n_bnd)
      real      x_tmp(1),z_tmp(1)

      integer   m_work
      real      work(m_work)

      integer   i_err

      integer   n_tmp,lx_tmp,i_tmp,ix_tmp_0
      integer   n_bnd_0,ix_bnd_0

      i_err = 0

c  copy the input boundaries to work space
      n_tmp  = n_bnd
      lx_tmp = ix_bnd(n_bnd) + nx_bnd(n_bnd)

c  copy input info to tmp arrays
      call util_copy(n_tmp ,im_bnd,im_tmp)
      call util_copy(lx_tmp,it_bnd,it_tmp)
      call util_copy(n_tmp ,ix_bnd,ix_tmp)
      call util_copy(n_tmp ,nx_bnd,nx_tmp)
      call util_copy(lx_tmp, x_bnd, x_tmp)
      call util_copy(lx_tmp, z_bnd, z_tmp)

c  keep the first boundary as is
      n_bnd = min(n_bnd,1)

c      print'(
c     1 /,'' total number of input boundaries='',i8
c     1,/,'' total number of input points    ='',i8
c     1)'
c     1,n_tmp,lx_tmp

      print'(
     1 /,'' orignal  boundary original  number    final''
     1,/,'' boundary flag     number    of        number of''
     1,/,''                   of points divisions points''
     1)'
c  check each tmp boundary against all previous values
      n_bnd_0 = 0
      do i_tmp = 1 , n_tmp

        ix_bnd_0 = ix_bnd(n_bnd) + 1
        ix_tmp_0 = ix_tmp(i_tmp) + 1
        if (i_tmp .gt. 0) then
          call rmod_condition_boundaries_1(z_tol
     1,m_bnd,mx_bnd,n_bnd,im_bnd,it_bnd,ix_bnd,nx_bnd,x_bnd,z_bnd
     1,im_tmp(i_tmp),it_tmp(ix_tmp_0),ix_tmp(i_tmp),nx_tmp(i_tmp)
     1,x_tmp(ix_tmp_0),z_tmp(ix_tmp_0)
     1,m_work,work
     1,i_err)
        if (i_err .ne. 0) goto 999
        endif

        print'(1x,i8,1x,i8,1x,i8,1x,i8,1x,i8)'
     1,i_tmp,im_tmp(i_tmp),nx_tmp(i_tmp),n_bnd-n_bnd_0
     1,ix_bnd(n_bnd)+nx_bnd(n_bnd)-ix_bnd_0
        n_bnd_0 = n_bnd

      enddo    ! do i_tmp = 2 , n_tmp

      print'(
     1 /,'' total number of input  boundaries='',i8
     1 /,'' total number of output boundaries='',i8
     1,/,'' total number of input  points    ='',i8
     1,/,'' total number of output points    ='',i8
     1)'
     1,n_tmp,lx_tmp
     1,n_bnd,ix_bnd(n_bnd)+nx_bnd(n_bnd)

      return

  999 continue
      print'(/,'' error in rmod_condition_boundaries_n'')'
      i_err = -1
      return

      end

c23456789012345678901234567890123456789012345678901234567890123456789012
      subroutine rmod_condition_boundaries_1(z_tol
     1,m_bnd,mx_bnd,n_bnd,im_bnd,it_bnd,ix_bnd,nx_bnd,x_bnd,z_bnd
     1,                   im_tmp,it_tmp,ix_tmp,nx_tmp,x_tmp,z_tmp
     1,m_work,work
     1,i_err)
c  compare every pair of boundaries for those which may overlap
      implicit  none

      integer   util_len_r

      real      z_tol

      integer   m_bnd,mx_bnd

      integer   n_bnd
      integer   im_bnd(n_bnd),it_bnd(n_bnd)
      integer   ix_bnd(n_bnd),nx_bnd(n_bnd)
      real      x_bnd(1),z_bnd(1)

      integer   im_tmp,it_tmp(1)
      integer   ix_tmp,nx_tmp
      real      x_tmp(1),z_tmp(1)

      integer   m_work
      real      work(m_work)

      integer   i_err

      integer   new_bnd
      integer   n_tmp,i_tmp,jx_tmp,jt_tmp,i_bad,n_bad,n_del
      integer   i_bnd,jx_bnd,n_bnd_0,jx_bnd_0
      real      x0,z0,z1
      real      z_dis

      i_err = 0

c  there are curently n_bnd boundaries in x_bnd,z_bnd
c  check the points in x_tmp,z_tmp against each of these
c  if there is an overlap then separate them

      jt_tmp  = it_tmp(1)
      n_bnd_0 = n_bnd
      jx_bnd  = ix_bnd(n_bnd) + nx_bnd(n_bnd)
      new_bnd = 1
      n_bad   = 0
      n_del   = 0

c  check each point against the other boundaries

      do jx_tmp = 1 , nx_tmp

        x0 = x_tmp(jx_tmp)
        z0 = z_tmp(jx_tmp)
        z_dis = 1.e6

        do i_bnd = 1 , n_bnd_0

c  interpolate the depths from z_bnd at the x_tmp
c  put them into work
          jx_bnd_0 = ix_bnd(i_bnd) + 1
          call rmod_interpolate_points_n(
     1 nx_bnd(i_bnd),x_bnd(jx_bnd_0),z_bnd(jx_bnd_0)
     1,1,x0,z1)

          if (abs(z0-z1) .lt. z_tol) i_bad = i_bnd
          z_dis = min(z_dis,abs(z0-z1))

        enddo    ! do i_bnd = 1 , n_bnd_0

c  if the vertical distance between these two points is greater than z_t
c  add the point
c        if (z_dis .gt. z_tol .or. n_bad .eq. 0) then
        if (z_dis .gt. z_tol) then

          if (z_dis .gt. z_tol) then

            n_bad = 0

          else

            n_bad = n_bad + 1

          endif

          if (new_bnd .ne. 0) then

c  if this is a new boundary set the pointer
c            if (nx_bnd(n_bnd) .eq. 1)
c     1print'('' removeing boundary with only 1 point''
c     1,'' n_bnd='',i6)',n_bnd

            if (nx_bnd(n_bnd) .eq. 1) then

              n_bnd = n_bnd - 1
              jx_bnd = ix_bnd(n_bnd) + nx_bnd(n_bnd)

            endif    ! if (nx_bnd(n_bnd) .eq. 1) then

            n_bnd         = n_bnd + 1
            im_bnd(n_bnd) = im_tmp
            ix_bnd(n_bnd) = jx_bnd
            nx_bnd(n_bnd) = 0
            jt_tmp        = jt_tmp + 1

c      print'('' new boundary n_bnd='',i6
c     1,'' im='',i6,'' it='',i6,'' ix='',i6)'
c     1,n_bnd,im_bnd(n_bnd),it_bnd(n_bnd),ix_bnd(n_bnd)

          endif    ! if (new_bnd .ne. 0) then

c  add this point
          new_bnd        = 0
          jx_bnd         = jx_bnd + 1
          nx_bnd(n_bnd)  = nx_bnd(n_bnd) + 1
          x_bnd(jx_bnd)  = x_tmp(jx_tmp)
          z_bnd(jx_bnd)  = z_tmp(jx_tmp)
          it_bnd(jx_bnd) = jt_tmp

c      print'('' good tx='',i6,'' bx='',i6
c     1,'' x0='',f10.2,'' z0='',f10.2,'' z1='',f10.2
c     1,'' ib='',i2)'
c     1,jx_tmp,jx_bnd-ix_bnd(n_bnd),x0,z0,z1,i_bad

        else    ! if (z_dis .gt. z_tol) then

c      print'('' bad  tx='',i6,'' bx='',i6
c     1,'' x0='',f10.2,'' z0='',f10.2,'' z1='',f10.2
c     1,'' ib='',i2)'
c     1,jx_tmp,jx_bnd-ix_bnd(n_bnd),x0,z0,z1,i_bad
          n_del = n_del + 1
          new_bnd = 1

        endif    ! if (z_dis .gt. z_tol) then

      enddo    ! do jx_tmp = 1 , nx_tmp

c  check the last boundary to make sure it has more than 1 point
      if (nx_bnd(n_bnd) .eq. 1) n_bnd = n_bnd - 1

c      if (n_del .ne. 0)
c     1print'('' number of orignal points='',i6
c     1,/,'' number of points deleted='',i6)'
c     1,nx_tmp,n_del

      return

  999 continue
      print'(/,'' error in rmod_condition_boundaries_1'')'
      i_err = -1
      return

      end

c23456789012345678901234567890123456789012345678901234567890123456789012
      subroutine rmod_scale_2(nx,x,x_inp_1,x_inp_2,x_out_1,x_out_2)
      implicit none
      integer  nx
      real     x(nx)
      real     x_inp_1,x_inp_2
      real     x_out_1,x_out_2

      integer ix
      real    d_out_d_inp

        print'(/,'' rmod_scale_2''
     1,/,'' x_inp_1='',f10.2,'' x_inp_2='',f10.2
     1,/,'' x_out_1='',f10.2,'' x_out_2='',f10.2
     1,/,'' nx='',i6,'' x1='',f10.2,'' xn='',f10.2
     1)'
     1,x_inp_1,x_inp_2
     1,x_out_1,x_out_2
     1,nx,x(1),x(nx)

      if (x_out_1 .eq. x_out_2 .or. x_inp_1 .eq. x_out_2) then

        print'(/,'' error in rmod_scale_2'')'

      else    ! if (x_out_1 .eq. x_out_2 .or. x_inp_1 .eq. x_out_2) then

        d_out_d_inp = (x_out_2 - x_out_1) / (x_inp_2 - x_inp_1)

        do ix = 1 , nx

          x(ix) = x_out_1 + d_out_d_inp * (x(ix) - x_inp_1)

        enddo    ! do ix = 1 , nx

      endif    ! if (x_out_1 .eq. x_out_2 .or. x_inp_1 .eq. x_out_2) the

        print'(/,'' after scaling''
     1,/,'' nx='',i6,'' x1='',f10.2,'' xn='',f10.2
     1)'
     1,nx,x(1),x(nx)

      return
      end


c23456789012345678901234567890123456789012345678901234567890123456789012
      subroutine rmod_interpolate_points_n(
     1 nx_inp,x_inp,z_inp
     1,nx_out,x_out,z_out)
      implicit none
      integer nx_inp
      real    x_inp(nx_inp),z_inp(nx_inp)
      integer nx_out
      real    x_out(nx_out),z_out(nx_out)

      integer ix_out,x_eps
      integer il,ir
      real    wl,wr

      x_eps = 1e-6

      do ix_out = 1 , nx_out

        call rmod_interpolate_coefficients(
     1x_out,x_eps,nx_inp,x_inp,il,ir,wl,wr)
        z_out(ix_out) = z_inp(il) * wl + z_inp(ir) * wr

      enddo    ! do ix_out = 1 , nx_out

      return
      end

c23456789012345678901234567890123456789012345678901234567890123456789012
      subroutine rmod_interpolate_coefficients(
     1x_out,x_eps,nx_inp,x_inp,il,ir,wl,wr)
c get nearest index to x_out from x_inp to left and right.
c  if x_inp is outvide range of x_out use nearest end member for both il
c  x_eps = min resolution in model
c  nx_inp = # of elements in x_inp to convider
c  il = nearest point to left (x_inp<x_out)
c  ir = nearest point to right (x_inp>x_out)
c  wl = weight to left vide
c  wr = weight to right vide
c  this code is stripped from bill harlan's fitcell1
      implicit none
      integer nx_inp,il,ir
      real x_out,x_inp(nx_inp),x_eps,wl,wr,dx
      integer i,nrepeat
      real x_min,x_max,eps
      x_min = x_out
      x_max = x_out
      il = 0
      ir = 0
      nrepeat = 0
      do 1 i = 1 , nx_inp
        x_min = min(x_min,x_inp(i))
        x_max = max(x_max,x_inp(i))
        if (x_inp(i) .lt. x_out) then
          if (il .eq. 0) then
            il = i
          else
            if (x_inp(i) .gt. x_inp(il)) then
              il = i
            elseif (x_inp(i) .eq. x_inp(il)) then
c              goto 999
              nrepeat = nrepeat + 1
            endif
          endif
        elseif (x_inp(i) .gt. x_out) then
          if (ir .eq. 0) then
            ir = i
          else
            if (x_inp(i) .lt. x_inp(ir)) then
              ir = i
            elseif (x_inp(i) .eq. x_inp(ir)) then
c              goto 999
              nrepeat = nrepeat + 1
            endif
          endif
        else
          il = i
          ir = i
        endif
    1 continue
c see if output point is off end of input points.
      if (il .eq. 0) il = ir
      if (ir .eq. 0) ir = il
      if (il*ir .eq. 0) then
        print'('' impossible: code bug in fitcell1'')'
        stop
      endif
c now have two points spannng output point
      dx = x_inp(ir) - x_inp(il)
      eps = max(x_eps,(1.e-8)*abs(x_max-x_min))
      if (dx .gt. eps) then
        wr = (x_out - x_inp(il)) / dx
      else
c...   same point on left and right
        wr = 0.5
      endif
      wl = 1. - wr

      return
      end

c23456789012345678901234567890123456789012345678901234567890123456789012
      subroutine rmod_compute_tolerance(nx,x,x_tol)
c  compute a tolerance distance
      implicit none

      integer  nx
      real     x(nx),x_tol
      real     x_min,x_max

      call util_min_max(x_min,x_max,nx,x)

      if (x_min .eq. x_max) then

        x_tol = 1 e-5

      else    ! if (x_min .eq. x_max) then

        x_tol = abs(x_max-x_min) / 1e5

      endif    ! if (x_min .eq. x_max) then

c      print'(/,'' rmod_compute_tolerance''
c     1,'' nx='',i6,'' x_min='',f10.2,'' x_max='',f10.2
c     1,'' x_tol='',f10.2)'
c     1,nx,x_min,x_max,x_tol

      return
      end


c23456789012345678901234567890123456789012345678901234567890123456789012
      subroutine rmod_decimate_2d_layer(mod_type
     1,x_min,x_max
     1,z_min,z_max
     1,n_bnd,im_bnd,it_bnd,ix_bnd,nx_bnd,x_bnd,z_bnd
     1,n_vel,im_vel,it_vel,ix_vel,nx_vel,x_vel,y_vel,z_vel,vel)

c  remove every n'th point from boundaries
      implicit  none

      integer   util_len_r

      character mod_type*(*)

      real      x_min,x_max
      real      z_min,z_max

      integer   n_bnd
      integer   im_bnd(1),it_bnd(1)
      integer   ix_bnd(1),nx_bnd(1)
      real      x_bnd(1),z_bnd(1)

      integer   n_vel
      integer   im_vel(1),it_vel(1)
      integer   ix_vel(1),nx_vel(1)
      real      x_vel(1),y_vel(1),z_vel(1),vel(1)

      integer   i

c23456789012345678901234567890123456789012345678901234567890123456789012
      print'(
     1 /,'' This option decimates 2D layered models by skipping points''
     1,/,'' or by keeping a minimum distance between points''
     1,/,'' you will be asked for the skip number and ''
     1,'' the tolerance distance''
     1,/,'' Using tolerance   = 0 will keep all points''
     1,/,'' Using skip number = 0 will keep all points''
     1,/,'' To avoid using tolerance set it  to a large number''
     1,/,'' To avoid using the skip number set it  to a large number''
     1,/,'' If you use the tolerance criteria, you may want ''
     1,'' to put the x and y ''
     1,/,'' coordinates in the same units such as XBASEMENT and DEPTH''
     1,//,'' current model limits are:''
     1,/,'' x_min='',f10.2,'' x_max='',f10.2
     1,/,'' z_min='',f10.2,'' z_max='',f10.2
     1)'
     1,x_min,x_max
     1,z_min,z_max

      if (mod_type(1:5) .ne. 'LAYER') then
        print'('' mode mod_type must be LAYER mod_type='',a)'
     1,mod_type(1:util_len_r(mod_type))
        return
      endif

c23456789012345678901234567890123456789012345678901234567890123456789012
      call rmod_decimate_boundaries(
     1 n_bnd,im_bnd,it_bnd,ix_bnd,nx_bnd,x_bnd,z_bnd)

      call rmod_decimate_velocities(
     1 n_vel,im_vel,it_vel,ix_vel,nx_vel,x_vel,z_vel,vel)

      return

      end

c23456789012345678901234567890123456789012345678901234567890123456789012
      subroutine rmod_decimate_boundaries(
     1 n_bnd,im_bnd,it_bnd,ix_bnd,nx_bnd,x_bnd,z_bnd)

c  remove every n'th point from boundaries
      implicit  none

      integer   n_bnd
      integer   im_bnd(1),it_bnd(1)
      integer   ix_bnd(1),nx_bnd(1)
      real      x_bnd(1),z_bnd(1)

      integer   lx_bnd
      integer   n_tol
      real      r_tol

      integer   i_bnd,jx_inp_1,jx_inp_2,jx_inp,jx_out
      integer   jx_last,nx_last
      real      x_last,z_last,r_dis

c23456789012345678901234567890123456789012345678901234567890123456789012
      print'(/,'' decimating boundaries number of boundaries='',i8)'
     1,n_bnd

      lx_bnd = ix_bnd(n_bnd) + nx_bnd(n_bnd)
      jx_out = 0

      n_tol = 0
      r_tol = 0.

      do i_bnd = 1 , n_bnd

        jx_inp_1      = ix_bnd(i_bnd) + 1
        jx_inp_2      = ix_bnd(i_bnd) + nx_bnd(i_bnd)
        x_last        = x_bnd(jx_inp_1)
        z_last        = z_bnd(jx_inp_1)
        jx_last       = jx_inp_1
        nx_last       = nx_bnd(i_bnd)
        ix_bnd(i_bnd) = jx_out
        nx_bnd(i_bnd) = 0

        print'(
     1 /,'' boundary='',i8,'' flag1='',i5,'' flag2='',i5
     1,'' n_inp='',i5
     1,/,'' first i='',i10  ,'' last i='',i10
     1,/,'' first x='',f10.2,'' last x='',f10.2
     1,/,'' first z='',f10.2,'' last z='',f10.2
     1)'
     1,i_bnd,im_bnd(i_bnd),it_bnd(ix_bnd(i_bnd)+1),nx_last
     1,it_bnd(jx_inp_1),it_bnd(jx_inp_2)
     1,x_bnd(jx_inp_1),x_bnd(jx_inp_2)
     1,z_bnd(jx_inp_1),z_bnd(jx_inp_2)

        call rmod_get_int_real(
     1'the skip number and the tolerance'
     1,n_tol,n_tol
     1,r_tol,r_tol)

        n_tol = max(0 ,n_tol)
        r_tol = max(0.,r_tol)

        do jx_inp = jx_inp_1 , jx_inp_2

          r_dis =
     1sqrt((x_bnd(jx_inp)-x_last)**2+(z_bnd(jx_inp)-z_last)**2)

          if (
     1      r_dis .gt. r_tol
     1 .or. jx_inp-jx_last .gt. n_tol
     1 .or. jx_inp .eq. jx_inp_1
     1 .or. jx_inp .eq. jx_inp_2
     1) then

            jx_out         = jx_out + 1
            nx_bnd(i_bnd)  = nx_bnd(i_bnd) + 1
            it_bnd(jx_out) = it_bnd(jx_inp)
            x_bnd(jx_out)  = x_bnd(jx_inp)
            z_bnd(jx_out)  = z_bnd(jx_inp)
            x_last         = x_bnd(jx_out)
            z_last         = z_bnd(jx_out)
            jx_last        = jx_inp

          endif    ! if (

        enddo    ! do jx_inp = jx_inp_1 , jx_inp_2

        print'('' boundary='',i8,'' flag1='',i5,'' flag2='',i5
     1,'' n_inp='',i8,'' n_out='',i8)'
     1,i_bnd,im_bnd(i_bnd),it_bnd(ix_bnd(i_bnd)+1)
     1,nx_last,nx_bnd(i_bnd)

      enddo    ! do i_bnd = 1 , n_bnd

      print'(
     1 /,'' total number of input  points    ='',i8
     1,/,'' total number of output points    ='',i8
     1)'
     1,lx_bnd,ix_bnd(n_bnd)+nx_bnd(n_bnd)

      return

      end

c23456789012345678901234567890123456789012345678901234567890123456789012
      subroutine rmod_decimate_velocities(
     1 n_vel,im_vel,it_vel,ix_vel,nx_vel,x_vel,z_vel,vel)

c  remove every n'th point from boundaries
      implicit  none

      integer   n_vel
      integer   im_vel(1),it_vel(1)
      integer   ix_vel(1),nx_vel(1)
      real      x_vel(1),z_vel(1),vel(1)

      integer   lx_vel
      integer   n_tol
      real      r_tol

      integer   i_vel,jx_inp_1,jx_inp_2,jx_inp,jx_out
      integer   jx_last,nx_last,i
      real      x_last,z_last,r_dis

c23456789012345678901234567890123456789012345678901234567890123456789012
      print'(///,'' decimating velocities number of velocities='',i8)'
     1,n_vel

      lx_vel = ix_vel(n_vel) + nx_vel(n_vel)
      jx_out = 0

      n_tol = 0
      r_tol = 0.

      do i_vel = 1 , n_vel

        jx_inp_1      = ix_vel(i_vel) + 1
        jx_inp_2      = ix_vel(i_vel) + nx_vel(i_vel)
        x_last        = x_vel(jx_inp_1)
        z_last        = z_vel(jx_inp_1)
        jx_last       = jx_inp_1
        nx_last       = nx_vel(i_vel)
        ix_vel(i_vel) = jx_out
        nx_vel(i_vel) = 0

        print'(
     1 /,'' velocity='',i8,'' flag1='',i5,'' flag2='',i5
     1,'' n_inp='',i8
     1,/,'' first i='',i10  ,'' last i='',i10
     1,/,'' first x='',f10.2,'' last x='',f10.2
     1,/,'' first z='',f10.2,'' last z='',f10.2
     1,/,'' first v='',f10.2,'' last v='',f10.2
     1)'
     1,i_vel,im_vel(i_vel),it_vel(ix_vel(i_vel)+1),nx_last
     1,it_vel(jx_inp_1),it_vel(jx_inp_2)
     1,x_vel(jx_inp_1),x_vel(jx_inp_2)
     1,z_vel(jx_inp_1),z_vel(jx_inp_2)
     1,vel(jx_inp_1),vel(jx_inp_2)

        call rmod_get_int_real(
     1'the skip number and the tolerance'
     1,n_tol,n_tol
     1,r_tol,r_tol)

        n_tol = max(0 ,n_tol)
        r_tol = max(0.,r_tol)

        do jx_inp = jx_inp_1 , jx_inp_2

          r_dis =
     1sqrt((x_vel(jx_inp)-x_last)**2+(z_vel(jx_inp)-z_last)**2)

          if (
     1      r_dis .gt. r_tol
     1 .or. jx_inp-jx_last .gt. n_tol
     1 .or. jx_inp .eq. jx_inp_1
     1 .or. jx_inp .eq. jx_inp_2
     1) then

            jx_out         = jx_out + 1
            nx_vel(i_vel)  = nx_vel(i_vel) + 1
            it_vel(jx_out) = it_vel(jx_inp)
            x_vel(jx_out)  = x_vel(jx_inp)
            z_vel(jx_out)  = z_vel(jx_inp)
            vel(jx_out)    = vel(jx_inp)
            x_last         = x_vel(jx_out)
            z_last         = z_vel(jx_out)
            jx_last        = jx_inp

          endif    ! if (

        enddo    ! do jx_inp = jx_inp_1 , jx_inp_2

        print'('' velocity='',i8,'' flag1='',i5,'' flag2='',i5
     1,'' n_inp='',i8,'' n_out='',i8)'
     1,i_vel,im_vel(i_vel),it_vel(ix_vel(i_vel)+1)
     1,nx_last,nx_vel(i_vel)

      enddo    ! do i_vel = 1 , n_vel

      print'(
     1 /,'' total number of input  points    ='',i8
     1,/,'' total number of output points    ='',i8
     1)'
     1,lx_vel,ix_vel(n_vel)+nx_vel(n_vel)

      return

      end

c23456789012345678901234567890123456789012345678901234567890123456789012
      subroutine rmod_convert_v_stack_to_nmo_t_0(mod_type,czi
     1,nx_vel,nt_vel,t0_vel,dt_vel,vel)
c  convert between stacking velocity and nmo velocity in time domain
      implicit none

      character mod_type*(*),czi*(*)
      real     eta,near_mute,far_mute,max_off
      integer  nx_vel,nt_vel
      real     t0_vel,dt_vel
      real     vel(nt_vel,nx_vel,2)

      print'(/,'' this option convert between stacking velocity''
     1,'' and nmo velocity in the time domain''
     1,/,'' using a constant eta value''
     1,/,'' you will be asked for the eta value,''
     1,'' the near and far mute times''
     1,/,'' and the maximum offset used in determining the ''
     1,''stacking velocities.''
     1)'

      if (mod_type(1:4) .ne. 'GRID') then
        print'('' Model mod_type must be GRID for this option''
     1,'' mod_type= '',a16)',mod_type
        call rmod_pause(' ',' ')
        return
      endif

      if (czi(1:4) .ne. 'TIME') then
        print'('' Model Z coordinate must be TIME for this option''
     1,'' czi= '',a16)',czi
        return
      endif

      eta = 0.
      near_mute = 0.
      far_mute = (nt_vel-1)*dt_vel+t0_vel
      max_off = 1000.

      call rmod_get_real('the constant eta value'
     1,eta,eta)
      call rmod_get_real('the near mute time'
     1,near_mute,near_mute)
      call rmod_get_real('the far mute time'
     1,far_mute,far_mute)
      call rmod_get_real('the maximum offset'
     1,max_off,max_off)

      call rmod_convert_v_stack_to_nmo_t(
     1eta,near_mute,far_mute,max_off
     1,nx_vel,nt_vel,t0_vel,dt_vel,vel)

      return

      end

c23456789012345678901234567890123456789012345678901234567890123456789012
      subroutine rmod_convert_v_stack_to_nmo_t(
     1eta,near_mute,far_mute,max_off
     1,nx_vel,nt_vel,t0_vel,dt_vel,vel)
c  convert between stacking velocity and nmo velocity in time domain
      implicit none

      real     eta,near_mute,far_mute,max_off
      integer  nx_vel,nt_vel
      real     t0_vel,dt_vel
      real     vel(nt_vel,nx_vel,2)

      integer  ix_vel,it_vel
      real     a,b,c,q,x_sq,v_sq,t_vel,t_sq

      print'(/,'' rmod_convert_v_stack_to_nmo_t''
     1,/,'' eta='',f10.4,'' near_mute='',f10.4
     1,'' far_mute='',f10.4,'' max_off='',f10.2
     1,/,'' nx_vel='',i8,'' nz_vel='',i8
     1,'' t0_vel='',f10.4,'' dt_vel='',f10.4))'
     1,eta,near_mute,far_mute,max_off
     1,nx_vel,nt_vel,t0_vel,dt_vel

c  convert from slowness to velocity
      call util_invert(nx_vel*nt_vel,vel)

      do ix_vel = 1 , nx_vel

        do it_vel = 1 , nt_vel

          t_vel = (it_vel - 1) * dt_vel + t0_vel
          t_sq= (t_vel / 2.)**2
          v_sq = vel(it_vel,ix_vel,1)**2
          x_sq = (min(1.,max(0.
     1,t_vel-near_mute)/(far_mute-near_mute))*max_off/2.)**2
c          q = 1. / (1. + 2. * vel(it_vel,ix_vel,2))
          q = 1. / (1. + 2. * eta)
          a = t_sq
          b = x_sq - t_sq * v_sq
          c = -x_sq * v_sq * q
          if (a .ne. 0.)
     1vel(it_vel,ix_vel,1) = sqrt((-b+sqrt(b**2-4.*a*c))/(2.*a))

        enddo    ! do it_vel = 1 , nt_vel

      enddo    ! do ix_vel = 1 , nx_vel

c  convert from velocity to slowness
      call util_invert(nx_vel*nt_vel,vel)

      return
      end

c23456789012345678901234567890123456789012345678901234567890123456789012
      subroutine rmod_set_surfaces(mod_type
     1,nx_grd,ny_grd,nz_grd,z_grd)

      implicit none

      integer   util_len_r
      character mod_type*(*)
      integer   nx_grd,ny_grd,nz_grd
      real      z_grd(nx_grd,ny_grd,nz_grd)

      integer   ix_grd,iy_grd,iz_grd,jz_grd,n_move

      print'(/,'' this option conditions surface depths''
     1,/,'' to ensure each subsequent surface lies below ''
     1,''the previous surface''
     1)'

      if (mod_type(1:4) .ne. 'G3DL') then

        print'(/,'' this option only uses G3DL models mod_type='',a)'
     1,mod_type(1:util_len_r(mod_type))
        return

      endif    ! if (mod_type(1:4) .ne. 'G3DL') then

      call rmod_print_z_min_max(nx_grd,ny_grd,nz_grd,z_grd)

      do iz_grd = 2 , nz_grd

        jz_grd = iz_grd - 1
        n_move = 0

        do iy_grd = 1 , ny_grd

          do ix_grd = 1 , nx_grd

            if (z_grd(ix_grd,iy_grd,iz_grd) 
     1     .lt. z_grd(ix_grd,iy_grd,jz_grd)) then

              n_move = n_move + 1
              z_grd(ix_grd,iy_grd,iz_grd) = z_grd(ix_grd,iy_grd,jz_grd) 

            endif    ! if (z_grd(ix_grd,iy_grd,iz_grd) 

          enddo    ! do ix_grd = 1 , nx_grd

        enddo    ! do iy_grd = 1 , ny_grd

        print'('' surface='',i8,'' number of points moved='',i8)'
     1,iz_grd,n_move

      enddo    ! do iz_grd = 2 , nz_grd

      call rmod_print_z_min_max(nx_grd,ny_grd,nz_grd,z_grd)

      return
      end

c23456789012345678901234567890123456789012345678901234567890123456789012
      subroutine rmod_3dl_to_3dg_new(mod_type,z0_lay,z1_lay
     1,nxv,iv1,iv2,iv3,xv,yv,zv,vel
     1,ncv,icv,xcv,ycv,zcv
     1,mv_grd
     1,nx_grd,x0_grd,dx_grd
     1,ny_grd,y0_grd,dy_grd
     1,nz_grd,z0_grd,dz_grd
     1,v_grd,z_lay
     1,m_work,work)
      character mod_type*(*),crd80*80
      dimension work(1)
      real z0_lay,z1_lay
      integer  nx_sum,ny_sum,nz_sum

      if (mod_type .ne. 'G3DL') then
        print'('' Model mod_type must be G3DL mod_type='',a16)',mod_type
        call rmod_pause(' ',' ')
        return
      endif

      call util_min_max(z_min,z_max,nx_grd*ny_grd*nz_grd,v_grd)
      print'(/,'' This option converts a 3d layered model into a ''
     1,''3d gridded model.''
     1,/,'' z_min='',f12.4,'' z_max='',f12.4
     1,/,'' Enter new grid limits'')'
     1,z_min,z_max

      nz_lay = nz_grd

      nx_lay = nx_grd
      x0_lay = x0_grd
      dx_lay = dx_grd

      ny_lay = ny_grd
      y0_lay = y0_grd
      dy_lay = dy_grd

      nz_grd = 101
      z0_grd = z0_lay

      nz_grd = 180
      z0_grd = z0_lay
      dz_grd = .03

      nz_grd = 180
      z0_grd = z0_lay
      dz_grd = .03

      nx_grd = 217
      x0_grd = 580
      dx_grd = 8

      ny_grd = 152
      y0_grd = 1842
      dy_grd = 8

      call rmod_set_grid_size('x'
     1,nx_grd,x0_grd,dx_grd
     1,nx_grd,x0_grd,dx_grd)

      call rmod_new_grid_size('y'
     1,ny_grd,y0_grd,dy_grd
     1,ny_grd,y0_grd,dy_grd)

      call rmod_new_grid_size('z'
     1,nz_grd,z0_grd,dz_grd
     1,nz_grd,z0_grd,dz_grd)

      if (nx_grd*ny_grd*nz_grd .gt. mv_grd) then
        print'('' insufficient memory for new grid''
     1,/,'' mv_grd='',i8,''
     1 nx_grd*ny_grd*nz_grd='',i8)',mv_grd,nx_grd*ny_grd*nz_grd
        nz_grd = nz_lay
        nx_grd = nx_lay
        x0_grd = x0_lay
        dx_grd = dx_lay
        ny_grd = ny_lay
        y0_grd = y0_lay
        dy_grd = dy_lay
        return
      endif

      call rmod_read_nx_sum(nx_sum,ny_sum,nz_sum)

      print'(/,'' layered grid characteristics''
     1,/,'' nz_lay='',i5,'' z0_lay='',f10.2,'' z1_lay='',f10.4
     1,/,'' nx_lay='',i5,'' x0_lay='',f10.2,'' dx_lay='',f10.4
     1,/,'' ny_lay='',i5,'' y0_lay='',f10.2,'' dy_lay='',f10.4)'
     1,nz_lay,z_min,z_max
     1,nx_lay,x0_lay,dx_lay
     1,ny_lay,y0_lay,dy_lay
      print'(/,'' 3d grid characteristics''
     1,/,'' nx_grd='',i5,'' x0_grd='',f10.2,'' dx_grd='',f10.4
     1,/,'' ny_grd='',i5,'' y0_grd='',f10.2,'' dy_grd='',f10.4
     1,/,'' nz_grd='',i5,'' z0_grd='',f10.2,'' dz_grd='',f10.4)'
     1,nx_grd,x0_grd,dx_grd
     1,ny_grd,y0_grd,dy_grd
     1,nz_grd,z0_grd,dz_grd

      crd80 = ' '
      write(crd80,'(a)')' RMOD creating 3D grid from 3D layer'
      call rmod_title_to_history(crd80)
      crd80 = ' '
      write(crd80,'('' nz_lay:'',i5)')nz_lay
      call rmod_card_to_history(crd80)
      crd80 = ' '
      write(crd80,'('' nx_lay:'',i5,'' x0_lay:'',f10.2
     1,'' dx_lay:'',f10.4)')
     1nx_lay,x0_lay,dx_lay
      call rmod_card_to_history(crd80)
      crd80 = ' '
      write(crd80,'('' ny_lay:'',i5,'' y0_lay:'',f10.2
     1,'' dy_lay:'',f10.4)')
     1ny_lay,y0_lay,dy_lay
      call rmod_card_to_history(crd80)
      crd80 = ' '
      write(crd80,'('' nx_grd:'',i5,'' x0_grd:'',f10.2
     1,'' dx_grd:'',f10.4,'' nx_sum:'',i5)')
     1nx_grd,x0_grd,dx_grd,nx_sum
      call rmod_card_to_history(crd80)
      crd80 = ' '
      write(crd80,'('' ny_grd:'',i5,'' y0_grd:'',f10.2
     1,'' dy_grd:'',f10.4,'' ny_sum:'',i5)')
     1ny_grd,y0_grd,dy_grd,ny_sum
      call rmod_card_to_history(crd80)
      crd80 = ' '
      write(crd80,'('' nz_grd:'',i5,'' z0_grd:'',f10.2
     1,'' dz_grd:'',f10.4,'' nz_sum:'',i5)')
     1nz_grd,z0_grd,dz_grd,nz_sum
      call rmod_card_to_history(crd80)

      call util_invert(nxv,vel)

      call util_copy(nx_lay*ny_lay*nz_lay,v_grd,z_lay)

      call gtol_3dl_to_3dg_new(
     1 nxv,iv1,iv2,iv3,xv,yv,zv,vel
     1,ncv,icv,xcv,ycv,zcv
     1,nx_sum,ny_sum,nz_sum
     1,nx_lay,x0_lay,dx_lay
     1,ny_lay,y0_lay,dy_lay
     1,nz_lay,z_lay
     1,nx_grd,x0_grd,dx_grd
     1,ny_grd,y0_grd,dy_grd
     1,nz_grd,z0_grd,dz_grd
     1,v_grd,m_work,work
     1,i_err)
      if (i_err .ne. 0) goto 999

      call util_invert(nxv,vel)

      mod_type ='GRID'

      return
  999 continue
      print'(''errror in rmod_3dl_to_3dg'')'
      return
      end

c23456789012345678901234567890123456789012345678901234567890123456789012
      subroutine rmod_read_nx_sum(nx_sum,ny_sum,nz_sum)
      implicit none
      integer  nx_sum,ny_sum,nz_sum
      character crd80*80

    1 continue

      nx_sum = 1
      ny_sum = 1
      nz_sum = 5

      print'(
     1 /,'' enter the number of points to ''
     1,/,'' smooth into the velocity in each direction''
     1,/,'' the number of points to one side will be (nx_sum-1)/2''
     1,/,'' defaults='',i5,1x,i5,1x,i5)'
     1,nx_sum,ny_sum,nz_sum

      read(*,'(a)',err=1,end=2)crd80

      if (crd80 .ne. ' ') read(crd80,*,err=1,end=2)nx_sum,ny_sum,nz_sum

    2 continue

      print'(/,'' nx_sum='',i5,'' ny_sum='',i5,'' nz_sum='',i5)'
     1,nx_sum,ny_sum,nz_sum

      return
      end

c23456789012345678901234567890123456789012345678901234567890123456789012
        subroutine rmod_ext (fname,ext)
        character*(*) fname,ext
        ientry = 1
        go to 1
        entry rmod_ext_replace (fname,ext)
        ientry = 2
   1    continue
        iclose = index(fname,']')
        isem2 = index(fname,';;')

        if (isem2 .gt. 0) iclose = max(iclose,isem2+1)
        if (iclose.eq.0)  iclose = index (fname,'>')
        idot = index (fname(iclose+1:),'.')
        if (idot.gt.0 .and. ientry.eq.1)  return
        isem = index (fname(iclose+1:),';')
        if (isem .gt. 0)  then
          if (idot.eq.0)  idot = isem
          fname = fname(:iclose+idot-1)//'.'//ext//fname(iclose+isem:)
        else
          if (idot.gt.0)  then
            fname = fname(:iclose+idot)//ext
          else
            ilast = index (fname(iclose+1:),' ')
            if (ilast.eq.0)  return
            fname = fname(:iclose+ilast-1)//'.'//ext
          end if
        end if
        return
        end

c23456789012345678901234567890123456789012345678901234567890123456789012
      subroutine rmod_comp(a)
c  remove spaces from a character string
      character a*(*)
      integer   rmod_len_r
      n = rmod_len_r(a)
      j = 0
      do 1 i = 1 , n
        if (a(i:i) .ne. ' ') then
          j = j + 1
          a(j:j) = a(i:i)
        endif
    1 continue
      a(j+1:n) = ' '
      return
      end

c23456789012345678901234567890123456789012345678901234567890123456789012
      subroutine rmod_efil(file1,file2)
      character *(*) file1,file2
      file2 = file1
      call rmod_cprs(lfile,file2)
      i1 = index(file2,']') + 1
      do 1 i = i1 , len(file2)
        if (file2(i:i) .eq. '.') then
          do 2 j = i , len(file2)
            file2(j:j) = ' '
    2     continue
          goto 3
        endif
    1 continue
    3 continue
      return
      end

c23456789012345678901234567890123456789012345678901234567890123456789012
      subroutine rmod_cprs(nstr,str)
      character str*(*)
      nstr = 0
      do 1 i = 1 , len(str)
        if (str(i:i) .ne. ' ')  then
          nstr = nstr + 1
          str(nstr:nstr) = str(i:i)
        endif
    1 continue
      return
      end

c  top of unix change
c  for unix delete the following routines to the next czzz
czzz top of plotting subroutine routines
czzz end of plotting subroutine routines
c  end of unix change
c23456789012345678901234567890123456789012345678901234567890123456789012
      subroutine rmod_print_2d_velocity_id(title,lu_out
     1,n_vel,im_vel,it_vel,ix_vel,nx_vel,x_vel,z_vel,vel)
c  print velocity info
      implicit none

      character title*(*)
      integer   lu_out
      integer   n_vel
      integer   im_vel(1),it_vel(1)
      integer   ix_vel(1),nx_vel(1)
      real      x_vel(1),z_vel(1),vel(1)

      integer   i_vel,j_vel

      if (lu_out .lt. 0) return

      write(lu_out,'(/,'' rmod_print_2d_layer_velocity''
     1,/,a
     1,/,'' number of velocities='',i8
     1,/,'' number of points    ='',i8
     1,/,''       vel      im_vel   ix_vel   nx_vel '')')
     1title,n_vel,ix_vel(n_vel)+nx_vel(i_vel)


      do i_vel = 1 , n_vel

        write(lu_out,'(1x,i8,1x,i8,1x,i8,1x,i8)')
     1i_vel,im_vel(i_vel),ix_vel(i_vel),nx_vel(i_vel)

      enddo    !    ! do i_vel = 1 , n_vel

      return
      end

c23456789012345678901234567890123456789012345678901234567890123456789012
      subroutine rmod_print_2d_velocity(title,lu_out
     1,n_vel,im_vel,it_vel,ix_vel,nx_vel,x_vel,z_vel,vel)
c  print velocity info
      implicit none

      character title*(*)
      integer   lu_out
      integer   n_vel
      integer   im_vel(1),it_vel(1)
      integer   ix_vel(1),nx_vel(1)
      real      x_vel(1),z_vel(1),vel(1)

      integer   i_vel,j_vel

      if (lu_out .lt. 0) return

      write(lu_out,'(/,'' rmod_print_2d_layer_velocity''
     1,/,a
     1,/,'' number of velocities='',i8
     1,/,'' number of points    ='',i8)')
     1title,n_vel,ix_vel(n_vel)+nx_vel(i_vel)

      do i_vel = 1 , n_vel

        write(lu_out,'(/,'' i_vel='',i3
     1,'' ix_vel='',i8,'' nx_vel='',i8
     1,/,''     x_vel      z_vel     vel    im_vel   it_vel  i_vel'')')
     1i_vel,ix_vel(i_vel),nx_vel(i_vel)

        write(lu_out,'(1x,f10.2,1x,f10.2,1x,f10.2,1x,i8,1x,i8,1x,i8)')
     1(x_vel(j_vel),z_vel(j_vel),vel(j_vel)
     1,im_vel(i_vel),it_vel(j_vel),j_vel
     1,j_vel=ix_vel(i_vel)+1,ix_vel(i_vel)+nx_vel(i_vel))

      enddo    !    ! do i_vel = 1 , n_vel

      return
      end

c23456789012345678901234567890123456789012345678901234567890123456789012
      subroutine rmod_print_2d_cell_id(title,lu_out
     1,n_cell,im_cell,ix_cell,nx_cell,x_cell,z_cell)
c  print 2d cell information
      implicit  none

      character title*(*)
      integer   lu_out
      integer   n_cell
      integer   im_cell(1)
      integer   ix_cell(1),nx_cell(1)
      real      x_cell(1),z_cell(1)

      integer   i_cell,j_cell

      if (lu_out .lt. 0) return

      write(lu_out,'(/,''rmod_print_2d_cell''
     1,/,a
     1,/,'' number of cells ='',i8
     1,/,'' number of points='',i8
     1,/,''       cell     im_cell  ix_cell  nx_cell'')')
     1title,n_cell,ix_cell(n_cell)+nx_cell(n_cell)

      do i_cell = 1 , n_cell

        write(lu_out,'(1x,i8,1x,i8,1x,i8,1x,i8)')
     1i_cell,im_cell(i_cell),ix_cell(i_cell),nx_cell(i_cell)

      enddo    ! do i_cell = 1 , n_cell

      return
      end

c23456789012345678901234567890123456789012345678901234567890123456789012
      subroutine rmod_print_2d_cell(title,lu_out
     1,n_cell,im_cell,ix_cell,nx_cell,x_cell,z_cell)
c  print 2d cell information
      implicit  none

      character title*(*)
      integer   lu_out
      integer   n_cell
      integer   im_cell(1)
      integer   ix_cell(1),nx_cell(1)
      real      x_cell(1),z_cell(1)

      integer   i_cell,j_cell

      if (lu_out .lt. 0) return

      write(lu_out,'(/,''rmod_print_2d_cell''
     1,/,a
     1,/,'' number of cells ='',i8
     1,/,'' number of points='',i8)')
     1title,n_cell,ix_cell(n_cell)+nx_cell(n_cell)

      do i_cell = 1 , n_cell

        write(lu_out,'(/,'' cell='',i8,'' im_cell='',i8
     1,'' ix_cell='',i8,'' nx_cell='',i8
     1,/,''  x_cell      z_cell        i_cell    j_cell'')')
     1i_cell,im_cell(i_cell),ix_cell(i_cell),nx_cell(i_cell)

        write(lu_out,'(1x,f10.2,1x,f10.2,1x,i8,1x,i8)')
     1(x_cell(j_cell),z_cell(j_cell),i_cell,j_cell-ix_cell(i_cell)
     1,j_cell=ix_cell(i_cell)+1,ix_cell(i_cell)+nx_cell(i_cell))

      enddo    ! do i_cell = 1 , n_cell

      return
      end

c23456789012345678901234567890123456789012345678901234567890123456789012
      subroutine rmod_cell_to_geodepth(mod_type
     1,x_min,x_max
     1,z_min,z_max
     1,nv,imv,itv,ixv,nxv,xv,zv,vel
     1,nc,imc,ixc,nxc,xc,zc
     1,nx_vel,x0_vel,dx_vel
     1,nz_vel,z0_vel,dz_vel
     1,v_grd
     1,m_work,work
     1,i_err)
      print'(/,'' this option is not available yet'')'
      return
      end
c23456789012345678901234567890123456789012345678901234567890123456789012
      subroutine rmod_scale_cps_v(
     1 m_work,work)

      implicit  none

      integer   m_work
      real      work(m_work)

      integer   i_err

      integer   nt_vel
      integer   l_vel,n_vel,nv_vel
      integer   iv_work,it_work,iw_work
      integer   ix_vel,iy_vel,n_work
      integer   i_vel,ihx,ihy

      logical   i_exist

      character project*10,line_name*10,rec_date*5,proj_date*5
      character userid*3,remark*15,v_name*8,v_type*4

      integer   i_inp_file
      character inp_file*80

      integer   i_out_file
      character out_file*80
      character crd80*80

      real      v_scale

      print'(
     1 /,'' This option scales a CPS velocity file''
     1)'

      i_err    = 0

c  get the input name and its unit number
    1 continue
      inp_file = 'NONE'
      call util_get_file_name(' Enter the input CPS velocity file name'
     1,inp_file,'vel')
      call rmod_inquire_exist(inp_file,*1)

      call getlun(inp_file,*1099)
      goto 1098
 1099 continue
      print'(/,'' error in getlun'')'
      goto 999
 1098 continue

c  open the input velocity function file
      call open_cps_velfile2(i_inp_file,inp_file,'READ',l_vel,n_vel
     1,*999,*999,ihx,ihy)

c  get the output name and its unit number
    2 continue
      out_file = inp_file
      call util_get_file_name(' Enter the output CPS velocity file name'
     1,out_file,'vel')

      call getlun(i_out_file,*2099)
      goto 2098
 2099 continue
      print'(/,'' error in getlun'')'
      goto 999
 2098 continue

c  open the output velocity function file
      call open_cps_velfile2(i_out_file,out_file,'WRITE',l_vel,n_vel
     1,*999,*999,ihx,ihy)

c  get the scale factor
 3001 continue
      v_scale = 1.
      print'(/,'' enter the velocity scale default='',f10.4
     1,/,'' new velocity = scale * old velocity'')'
     1,v_scale
      crd80 = ' '
      read(*,'(a)',err=3001,end=3002)crd80
      if (crd80(1:1) .eq. ' ') goto 3002
      read(crd80,*,err=3001,end=3002) v_scale
 3002 continue

      print'(
     1 /,'' Total number of velocity functions ='',i8
     1,/,'' Velocity scale ='',f10.4
     1)'
     1,n_vel,v_scale

      nt_vel = 1000
      it_work = 1
      iv_work = it_work + nt_vel
      iw_work = iv_work + nt_vel
      n_work  = iw_work + nt_vel

      if (n_work .gt. m_work) then
      print'('' need more memory in rmod_scale_cps_v have='',i10
     1,'' and need='',i10)',m_work,n_work
        i_err = -1
        goto 999
      endif    ! if (n_work .gt. m_work) then

c read each velocity function in the file
      do i_vel = 1 , n_vel

c  read the output scaled velocity file
        call read_cps_velfile(i_inp_file,l_vel,v_name
     1,nv_vel,ix_vel,iy_vel,v_type,work(it_work),work(iv_work),*999,*999
     1,project,line_name,rec_date,proj_date,userid,remark,work(iw_work))

c  check the memory size
      if (nv_vel .gt. nt_vel) then
        print'('' need more memory for velocty read n_vel='',i8)',nv_vel
        goto 999
      endif

c  scale the velocity values
        call util_scale(nv_vel,work(iv_work),v_scale)

c  write the output scaled velocity file
c          call write_cps_velfile (i_file,nvpp,v_name,nz,x,y,vmod_type
c     1,work,v(1,nx+1,ny),project,line,r_date,p_date,id,comment,work)
        call write_cps_velfile(i_out_file,l_vel,v_name
     1,nv_vel,ix_vel,iy_vel,v_type,work(it_work),work(iv_work)
     1,project,line_name,rec_date,proj_date,userid,remark,work(iw_work))

      enddo    ! do i_vel = 1 , n_vel

c  close the input file
      rewind(i_inp_file)
      call close_cps_velfile(i_inp_file,'READ')

c  close the output file
      rewind(i_out_file)
      call close_cps_velfile(i_out_file,'WRITE')

      return

  999 continue
      call rmod_pause(' error reading cps file',inp_file)
      return

      end
      subroutine date(bufd)
      implicit none
      character (len=9), intent(out) :: bufd
      character (len=8)              :: dat
      call date_and_time(dat)
      bufd(1:2)=dat(5:6)
      bufd(3:3)="-"
      bufd(4:5)=dat(7:8)
      bufd(6:9)=dat(1:4)
      end subroutine date
