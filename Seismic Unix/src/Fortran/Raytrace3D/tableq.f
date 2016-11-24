* Copyright (c) Colorado School of Mines, 2004.
* All rights reserved.

      program table
c  test program to compute a single travel/amplitude time table
c  you should be able to compile and run this as a stand alone program
c
c
c test normal exit
c stopping i_flag=          0
c variables:
c mx_tab		maximum grids in X on the output tables
c my_tab		maximum grids in Y on the output tables
c mz_tab		maximum grids in Z on the output tables
c dx_tab		X spacing on the output tables
c dy_tab		Y spacing on the output tables
c dz_tab		Z spacing on the output tables
c x_src			source location
c y_src			source location
c z_src			source location
c nx_vel		number of grids in X of the velocity file
c ny_vel		number of grids in Y of the velocity file
c nz_vel		number of grids in Z of the velocity file
c dx_vel		X spacing on velocity file
c dy_vel		Y spacing on velocity file
c dz_vel		Z spacing on velocity file
c x0_vel		initial position of velocity distribution 
c y0_vel		initial position of velocity distribution 
c z0_vel		initial position of velocity distribution 
c 

      implicit  none

      integer   util_len_r

      integer    mx_tab
      integer    my_tab
      integer    mz_tab

      real       dx_tab, dy_tab, dz_tab

      real      x_src,y_src,z_src

      integer  nx_vel
      real     x0_vel,dx_vel
      integer  ny_vel
      real     y0_vel,dy_vel
      integer  nz_vel
      real     z0_vel,dz_vel

      integer   na,nb
      real      a0,a1,b0,b1

c velocity space pointer
      integer  i_s_vel, n_s_vel

      character inp_file*80
      character outt_file*80
      character outamp_file*80
      character outpha_file*80
      character outq11_file*80
      character outq12_file*80
      character outq21_file*80
      character outq22_file*80
      character outa0_file*80
      character outb0_file*80
      character outa1_file*80
      character outb1_file*80
      character outdet21_file*80
      character outdet22_file*80
      character outdet23_file*80
      character outdet31_file*80
      character outdet32_file*80
      character outdet33_file*80
      character outlog_file*80
      character vx_file*80

      integer  t_scale,num_add,inter_type,m_ray
      real     t0_ray,t1_ray,dt_ray,dr_ray,maxangle
    
      integer   m_work
      parameter (m_work=35000000)
      real      work(m_work)
      integer   i_work,n_work
      integer   i_work_i,i_work_n

      real  v0,vx,vy,vz

      integer   i_err

      character tim_type*8

      real stepsize
      real dtaccuracy
      real maxTime
      real anglex1
      real anglex2
      real angley1
      real angley2
      integer angleDown
      real nullTTvalue
      real maxangle_p

      stepsize=.02
      dtaccuracy=.001
      maxTime=8.0
      anglex1=-89.
      anglex2=89.
      angley1=-89.
      angley2=89.
      angleDown=1
      maxangle=89.
      nullTTvalue=0.

c read input parameters

      call read_input(
     1 outt_file,outamp_file
     1,outpha_file,outq11_file,outq12_file
     1,outq21_file,outq22_file
     1,outa0_file,outb0_file,outa1_file,outb1_file
     1,outdet21_file,outdet22_file,outdet23_file
     1,outdet31_file,outdet32_file,outdet33_file
     1,outlog_file,inp_file
     1,vx_file
     1,tim_type
     1,nx_vel,x0_vel,dx_vel
     1,ny_vel,y0_vel,dy_vel
     1,nz_vel,z0_vel,dz_vel
     1,mx_tab,       dx_tab
     1,my_tab,       dy_tab
     1,mz_tab,       dz_tab
     1,x_src,y_src,z_src
     1,na,a0,a1
     1,nb,b0,b1
     1,t_scale,dt_ray
     1,t0_ray,t1_ray
     1,dr_ray,inter_type,num_add
     1,maxangle,m_ray
     1,v0,vx,vy,vz
     1,i_err)

c write out the readin parameters into log file

      OPEN(15,FILE=outlog_file,STATUS='unknown')
      CLOSE(15)
      OPEN(15,FILE=outlog_file,STATUS='OLD')

      write(15,'('' ******Start of '',a,/,'' '')')
     1 outlog_file(1:util_len_r(outlog_file))
 
      write(15, '(/,'' input parameters:''
     1,/,'' tim_type='',a
     1,/,'' outt_file='',a
     1,/,'' outamp_file='',a
     1,/,'' outpha_file='',a
     1,/,'' outdet21_file='',a
     1,/,'' outdet22_file='',a
     1,/,'' outdet23_file='',a
     1,/,'' outdet31_file='',a
     1,/,'' outdet32_file='',a
     1,/,'' outdet33_file='',a
     1,/,'' outa1_file='',a
     1,/,'' outb1_file='',a
     1,/,'' outinform='',a
     1,/,'' vx_file='',a
     1,/,'' v0='',f10.2
     1,/,'' x0='',f10.2,'' vx='',f10.4
     1,/,'' y0='',f10.2,'' vy='',f10.4
     1,/,'' z0='',f10.2,'' vz='',f10.4
     1,/,'' xs='',f10.2,'' ys='',f10.2,'' zs='',f10.2
     1,/,'' nx_tab='',i8,'' x0_tab='',f10.2,'' dx_tab='',f10.2
     1,'' x1_tab='',f10.2
     1,/,'' ny_tab='',i8,'' y0_tab='',f10.2,'' dy_tab='',f10.2
     1,'' y1_tab='',f10.2
     1,/,'' nz_tab='',i8,'' z0_tab='',f10.2,'' dz_tab='',f10.2
     1,'' z1_tab='',f10.2
     1,/,'' nx_vel='',i8,'' x0_vel='',f10.2,'' dx_vel='',f10.2
     1,'' x1_vel='',f10.2
     1,/,'' ny_vel='',i8,'' y0_vel='',f10.2,'' dy_vel='',f10.2
     1,'' y1_vel='',f10.2
     1,/,'' nz_vel='',i8,'' z0_vel='',f10.2,'' dz_vel='',f10.2
     1,'' z1_vel='',f10.2
     1,/,'' na='',i8,'' a0='',f10.2,'' a1='',f10.2
     1,/,'' nb='',i8,'' b0='',f10.2,'' b1='',f10.2
     1,/,'' m_ray='',i10,'' t_scale='',i10,'' inter_type='',i10
     1,''  num_add='',i8
     1,/,'' t0_ray='',f10.6,'' t1_ray='',f10.2,''     dt_ray='',f10.2
     1,''   dr_ray='',f10.2
     1,/,'' maxangle='',f10.2
     1)')
     1 tim_type(1:util_len_r(tim_type))
     1,outt_file(1:util_len_r(outt_file))
     1,outamp_file(1:util_len_r(outamp_file))
     1,outpha_file(1:util_len_r(outpha_file))
     1,outdet21_file(1:util_len_r(outdet21_file))
     1,outdet22_file(1:util_len_r(outdet22_file))
     1,outdet23_file(1:util_len_r(outdet23_file))
     1,outdet31_file(1:util_len_r(outdet31_file))
     1,outdet32_file(1:util_len_r(outdet32_file))
     1,outdet33_file(1:util_len_r(outdet33_file))
     1,outa1_file(1:util_len_r(outa1_file))
     1,outb1_file(1:util_len_r(outb1_file))
     1,outlog_file(1:util_len_r(outlog_file))
     1,vx_file(1:util_len_r(vx_file))
     1,v0
     1,x0_vel,vx
     1,y0_vel,vy
     1,z0_vel,vz
     1,x_src,y_src,z_src
     1,mx_tab,x_src-(mx_tab-1)*dx_tab/2.0,dx_tab,x_src+(mx_tab-1)*dx_tab/2.0
     1,my_tab,x_src-(my_tab-1)*dy_tab/2.0,dy_tab,y_src+(my_tab-1)*dy_tab/2.0
     1,mz_tab,z_src,dz_tab,z_src+(mz_tab-1)*dz_tab
     1,nx_vel,x0_vel,dx_vel,(nx_vel-1)*dx_vel+x0_vel
     1,ny_vel,y0_vel,dy_vel,(ny_vel-1)*dy_vel+y0_vel
     1,nz_vel,z0_vel,dz_vel,(nz_vel-1)*dz_vel+z0_vel
     1,na,a0,a1
     1,nb,b0,b1
     1,m_ray,t_scale,inter_type,num_add
     1,t0_ray,t1_ray,dt_ray,dr_ray
     1,maxangle

      close(15)

c allocate space for velocity array

      n_s_vel = nx_vel * ny_vel * nz_vel
      call util_wors(i_work_i,i_work_n,m_work)
      call util_work(i_work_i,i_work_n,i_s_vel ,n_s_vel )
      call util_worc(i_work_i,i_work_n,i_err)
      if (i_err .ne. 0) goto 998
		  
      call util_worl(i_work_i,i_work_n,n_work)
      call util_work(i_work_i,i_work_n,i_work,n_work)

c compute travel time tables

      call ktable_compute(
     1 vx_file
     1,outt_file,outamp_file
     1,outpha_file,outq11_file,outq12_file
     1,outq21_file,outq22_file
     1,outa0_file,outb0_file,outa1_file,outb1_file
     1,outdet21_file,outdet22_file,outdet23_file
     1,outdet31_file,outdet32_file,outdet33_file
     1,outlog_file
     1,tim_type
     1,nx_vel,x0_vel,dx_vel
     1,ny_vel,y0_vel,dy_vel
     1,nz_vel,z0_vel,dz_vel
     1,work(i_s_vel)
     1,mx_tab,       dx_tab
     1,my_tab,       dy_tab
     1,mz_tab,       dz_tab
     1,x_src,y_src,z_src
     1,na,a0,a1
     1,nb,b0,b1
     1,t_scale,dt_ray
     1,t0_ray,t1_ray
     1,dr_ray,inter_type,num_add
     1,maxangle,m_ray
     1,v0,vx,vy,vz
     1,stepsize
     1,dtaccuracy
     1,maxTime
     1,anglex1
     1,anglex2
     1,angley1
     1,angley2
     1,angleDown
     1,nullTTvalue
     1,maxangle_p
     1,n_work,work(i_work)
     1,i_err)

  998 continue
      print'('' error in table'')'
      i_err = -1

      end

c23456789012345678901234567890123456789012345678901234567890123456789012
      subroutine ktable_compute(
     1 vx_file
     1,outt_file,outamp_file
     1,outpha_file,outq11_file,outq12_file
     1,outq21_file,outq22_file
     1,outa0_file,outb0_file,outa1_file,outb1_file
     1,oute1x_file,oute1y_file,oute1z_file
     1,oute2x_file,oute2y_file,oute2z_file
     1,outlog_file
     1,tim_type
     1,nx_vel,x0_vel,dx_vel
     1,ny_vel,y0_vel,dy_vel
     1,nz_vel,z0_vel,dz_vel
     1,s_vel
     1,mx_tab,       dx_tab
     1,my_tab,       dy_tab
     1,mz_tab,       dz_tab
     1,x_src,y_src,z_src
     1,na,a0,a1
     1,nb,b0,b1
     1,t_scale,dt_ray
     1,t0_ray,t1_ray
     1,dr_ray,inter_type,num_add
     1,maxangle,m_ray
     1,v0,vx,vy,vz
     1,stepsize
     1,dtaccuracy
     1,maxTime
     1,anglex1
     1,anglex2
     1,angley1
     1,angley2
     1,angleDown
     1,nullTTvalue
     1,maxangle_p
     1,m_work,work
     1,i_err)
c  allocate working spaces
c  read in velocities or compute linearly distributed velocities from 
c  parameters v0,vx,vy,vz
c  call function to compute a single travel time and amplitude table
c  this can be modified to compute multiple tables

      implicit  none

      integer   util_len_r

      integer    npx_grid     ! x size of paraxial travel time grid
      integer    npy_grid     ! y size of paraxial travel time grid
      integer    npz_grid     ! z size of paraxial travel time grid

      integer    mx_tab
      integer    my_tab
      integer    mz_tab

      integer  nx_tab_0
      real     x0_tab_0

      integer  nz_tab_0
      real     z0_tab_0

      integer   n_dim

      real      x_src,y_src,z_src

      real      dr_tab

c work space pointers
      integer  i_ix_tab,n_ix_tab
      integer  i_nx_tab,n_nx_tab
      integer  i_x0_tab,n_x0_tab
      integer  i_iz_tab,n_iz_tab
      integer  i_nz_tab,n_nz_tab
      integer  i_z0_tab,n_z0_tab
      integer  i_t_tab,n_t_tab
      integer  i_amp_tab,n_amp_tab
      integer  i_phase_tab,n_phase_tab
      integer  i_q11_tab,n_q11_tab
      integer  i_q12_tab,n_q12_tab
      integer  i_q21_tab,n_q21_tab
      integer  i_q22_tab,n_q22_tab
      integer  i_p3_tab,n_p3_tab

      real     dx_tab,dz_tab
      integer  ny_tab
      real     y0_tab,dy_tab

      integer   n_xyz_tab

      integer  n_xyz_vel
      integer  nx_vel
      real     x0_vel,dx_vel
      integer  ny_vel
      real     y0_vel,dy_vel
      integer  nz_vel
      real     z0_vel,dz_vel

      integer   na,nb
      real      a0,a1,b0,b1

      real     s_vel(nx_vel*ny_vel*nz_vel)
      real     s_min, s_max

      character outt_file*80
      character outamp_file*80
      character outpha_file*80
      character outq11_file*80
      character outq12_file*80
      character outq21_file*80
      character outq22_file*80
      character outa0_file*80
      character outb0_file*80
      character outa1_file*80
      character outb1_file*80
      character oute1x_file*80
      character oute1y_file*80
      character oute1z_file*80
      character oute2x_file*80
      character oute2y_file*80
      character oute2z_file*80
      character outlog_file*80
      character vx_file*80

      integer  t_scale,num_add,inter_type,m_ray
      real     t0_ray,t1_ray,dt_ray,dr_ray,maxangle

      real     dx, dy, dz
      real     vx, vy, vz, v0
      real     x_vel, y_vel, z_vel
      integer  i_xy0, ix, iy, iz
      integer  ix_vel, iy_vel, iz_vel, i_xyz
    
      real     tmp
 
      integer   m_work
      real      work(m_work)
      integer   i_work,n_work
      integer   i_work_i,i_work_n

      integer   i_err

      character tim_type*8

      real stepsize
      real dtaccuracy
      real maxTime
      real anglex1
      real anglex2
      real angley1
      real angley2
      integer angleDown
      real nullTTvalue
      real maxangle_p

      stepsize=.02
      dtaccuracy=.001
      maxTime=8.0
      n_dim  = 3               ! dimensionality 2, 3
      dr_tab = 25.             ! computation grid size

c  define the travel time table grid
      nx_tab_0 = mx_tab
      x0_tab_0 = -((nx_tab_0 - 1) / 2) * dx_tab 
 
      ny_tab   = my_tab
      y0_tab   = -((ny_tab   - 1) / 2) * dy_tab 
 
      nz_tab_0 = mz_tab
      z0_tab_0 = 0.

      n_xyz_tab = nx_tab_0 * ny_tab * nz_tab_0

c define full 3D grid
 
      npx_grid = nx_tab_0
      npy_grid = ny_tab
      npz_grid = nz_tab_0
 
c  read velocity from an input file
      if (vx_file(1:4) .ne. 'NONE') then
 
        n_xyz_vel = nx_vel*ny_vel*nz_vel
 
        OPEN(13,FILE=vx_file,STATUS='old',ACCESS='sequential')

        do i_xyz= 1,n_xyz_vel
 
           iz = mod(i_xyz, nz_vel)
           i_xy0 = i_xyz/nz_vel
           if (iz .eq. 0) then
               iz = nz_vel
               i_xy0= i_xy0 -1
           endif
           ix = mod(i_xy0,nx_vel) + 1
           iy = i_xy0/nx_vel + 1
 
           read (13,*) tmp
           s_vel(i_xyz)=tmp
        enddo
        CLOSE(13)

        call util_min_max(s_min,s_max,n_xyz_vel,s_vel)

	print'('' Velocity has been read in from '',a)'
     1,vx_file(1:util_len_r(vx_file))

        print*,'min,max velocity:',s_min,s_max

c  compute velocity from v0,vx,vy,vz,x0,y0,z0
      else    ! if (vx_file(1:4) .ne. 'NONE') then
 
       do iy_vel = 1 , ny_vel
 
        do ix_vel = 1 , nx_vel
 
          do iz_vel = 1 , nz_vel
 
            dx = (ix_vel - 1) * dx_vel
            dy = (iy_vel - 1) * dy_vel
            dz = (iz_vel - 1) * dz_vel
 
            x_vel =  dx + x0_vel
            y_vel =  dy + y0_vel
            z_vel =  dz + z0_vel

            i_xyz = iz_vel + nz_vel*(ix_vel-1) +
     1              (iy_vel -1)*nx_vel*nz_vel 
            s_vel(i_xyz) =  v0
     1              + vx * dx + vy * dy + vz * dz
 
          enddo    ! do iz_vel = 1 , nz_vel
 
        enddo    ! do ix_vel = 1 , nx_vel
 
       enddo    ! do iy_vel = 1 , ny_vel

       print'('' The velocity model has been created.'')'
 
      endif    ! if (vx_file(1:4) .ne. 'NONE)' then

 
c  convert to slowness
      if (tim_type(1:1) .eq. 'E' .or.
     1  tim_type(1:1) .eq. 'B' .or. 
     1             tim_type(1:4) .eq. 'DRAY') then
         call util_invert(nx_vel*ny_vel*nz_vel,s_vel)

      endif ! if (tim_type(1:1) .eq. 'E' .or.

c  allocate working spaces
      call util_wors(i_work_i,i_work_n,m_work)
      n_xyz_tab = nx_tab_0 * ny_tab * nz_tab_0

      n_t_tab = n_xyz_tab      ! traveltime table location
      call util_work(i_work_i,i_work_n,i_t_tab ,n_t_tab )

      n_amp_tab = n_xyz_tab     ! amplitude table location
      call util_work(i_work_i,i_work_n,i_amp_tab ,n_amp_tab )

      n_q11_tab = n_xyz_tab     ! amplitude table location
      call util_work(i_work_i,i_work_n,i_q11_tab ,n_q11_tab )

      n_q12_tab = n_xyz_tab     ! amplitude table location
      call util_work(i_work_i,i_work_n,i_q12_tab ,n_q12_tab )

      n_q21_tab = n_xyz_tab     ! amplitude table location
      call util_work(i_work_i,i_work_n,i_q21_tab ,n_q21_tab )

      n_q22_tab = n_xyz_tab     ! amplitude table location
      call util_work(i_work_i,i_work_n,i_q22_tab ,n_q22_tab )

      n_p3_tab = n_xyz_tab     ! amplitude table location
      call util_work(i_work_i,i_work_n,i_p3_tab ,n_p3_tab )

      n_phase_tab = n_xyz_tab     ! phase table location
      call util_work(i_work_i,i_work_n,i_phase_tab ,n_phase_tab )

      n_ix_tab = ny_tab
      call util_work(i_work_i,i_work_n,i_ix_tab,n_ix_tab)

      n_nx_tab = ny_tab
      call util_work(i_work_i,i_work_n,i_nx_tab,n_nx_tab)

      n_x0_tab = ny_tab
      call util_work(i_work_i,i_work_n,i_x0_tab,n_x0_tab)

      n_iz_tab = ny_tab * nx_tab_0
      call util_work(i_work_i,i_work_n,i_iz_tab,n_iz_tab)

      n_nz_tab = ny_tab * nx_tab_0
      call util_work(i_work_i,i_work_n,i_nz_tab,n_nz_tab)

      n_z0_tab = ny_tab * nx_tab_0
      call util_work(i_work_i,i_work_n,i_z0_tab,n_z0_tab)

      call util_worc(i_work_i,i_work_n,i_err)
      if (i_err .ne. 0) goto 999

      call util_worl(i_work_i,i_work_n,n_work)
      call util_work(i_work_i,i_work_n,i_work,n_work)

c  initialize the travel time table pointers
c  these will match a rectangular grid

      call util_lini(         ny_tab,work(i_ix_tab),0,nx_tab_0) 
      call util_seti(         ny_tab,work(i_nx_tab)  ,nx_tab_0) 
      call util_setr(         ny_tab,work(i_x0_tab)  ,x0_tab_0) 
 
      call util_lini(nx_tab_0*ny_tab,work(i_iz_tab),0,nz_tab_0) 
      call util_seti(nx_tab_0*ny_tab,work(i_nz_tab)  ,nz_tab_0) 
      call util_setr(nx_tab_0*ny_tab,work(i_z0_tab)  ,z0_tab_0) 

c  compute a single travel time and amplitude table
      call ktable_compute_1(x_src,y_src,z_src
     1,n_dim,tim_type,dr_tab
     1,work(i_ix_tab),work(i_nx_tab),work(i_x0_tab),dx_tab
     1       ,ny_tab,y0_tab,dy_tab
     1,work(i_iz_tab),work(i_nz_tab),work(i_z0_tab),dz_tab
     1,n_xyz_tab
     1,work(i_t_tab)
     1,na,a0,a1
     1,nb,b0,b1
     1,t_scale,dt_ray
     1,t0_ray,t1_ray
     1,dr_ray,inter_type,num_add
     1,maxangle,m_ray
     1,nx_vel,x0_vel,dx_vel
     1,ny_vel,y0_vel,dy_vel
     1,nz_vel,z0_vel,dz_vel
     1,s_vel
     1,npx_grid,npy_grid,npz_grid
     1,n_work,work(i_work)
     1,stepsize
     1,dtaccuracy
     1,maxTime
     1,anglex1
     1,anglex2
     1,angley1
     1,angley2
     1,angleDown
     1,nullTTvalue
     1,maxangle_p
     1,work(i_amp_tab)
     1,work(i_phase_tab)
     1,work(i_q11_tab),work(i_q12_tab)
     1,work(i_q21_tab),work(i_q22_tab)
     1,work(i_p3_tab)
     1,outlog_file
     1,outt_file,outamp_file
     1,outpha_file,outq11_file,outq12_file
     1,outq21_file,outq22_file
     1,outa0_file,outb0_file,outa1_file,outb1_file
     1,oute1x_file,oute1y_file,oute1z_file
     1,oute2x_file,oute2y_file,oute2z_file
     1,i_err)

      return
  999 continue
      print'('' error in ktable_compute_1'')'
      i_err = -1

      end

c23456789012345678901234567890123456789012345678901234567890123456789012
      subroutine ktable_compute_1(x_src,y_src,z_src
     1,n_dim,tim_type,dr_tab
     1,ix_tab,nx_tab,x0_tab,dx_tab
     1       ,ny_tab,y0_tab,dy_tab
     1,iz_tab,nz_tab,z0_tab,dz_tab
     1,m_xyz_tab
     1,t_tab
     1,na,a0,a1
     1,nb,b0,b1
     1,t_scale,dt_ray
     1,t0_ray,t1_ray
     1,dr_ray,inter_type,num_add
     1,maxangle,m_ray
     1,nx_vel,x0_vel,dx_vel
     1,ny_vel,y0_vel,dy_vel
     1,nz_vel,z0_vel,dz_vel
     1,s_vel
     1,npx_grid,npy_grid,npz_grid
     1,m_work,work
     1,stepsize
     1,dtaccuracy
     1,maxTime
     1,anglex1
     1,anglex2
     1,angley1
     1,angley2
     1,angleDown
     1,nullTTvalue
     1,maxangle_p
     1,amp
     1,phase
     1,q11_tab,q12_tab,q21_tab,q22_tab,p3_tab
     1,outlog_file
     1,outt_file,outamp_file
     1,outpha_file,outq11_file,outq12_file
     1,outq21_file,outq22_file
     1,outa0_file,outb0_file,outa1_file,outb1_file
     1,oute1x_file,oute1y_file,oute1z_file
     1,oute2x_file,oute2y_file,oute2z_file
     1,i_err)
c  compute a single travel time and amplitude table
c
c  i = input o = output b = both
c
c i x_src = source x position in distance units
c i y_src = source y position in distance units
c i z_src = source z position in distance units
c
c i n_dim = dimensionality 2,3 are valid
c i tim_type = traveltime caluclation method STRAIGHT, PRAY (character*8)
c i dr_tab = raytracing spatial increment
c
c i ix_tab - travel time table x column start  array
c i nx_tab - travel time table x column length array
c i x0_tab - travel time table x column origin array
c i dx_tab - travel time table x column increment
c
c i ny_tab - travel time table y column length
c i y0_tab - travel time table y column origin
c i dy_tab - travel time table y column increment
c
c i iz_tab - travel time table z column start  array
c i nz_tab - travel time table z column length array
c i z0_tab - travel time table z column origin array
c i dz_tab - travel time table z column increment
c
c o t_tab - travel times in seconds
c o amp   - amplitude at output grids
c o phase - phase shift index at output grids
c o q11_tab - Q component at output grids  
c o q12_tab - Q component at output grids
c o q21_tab - Q component at output grids
c o q22_tab - Q component at output grids
c o p3_tab  - slowness component at output grids
c
c i nx_vel = number of x nodes in slowness array
c i x0_vel = x origin for slowness array
c i dx_vel = x increment for slowness array
c
c i ny_vel = number of y nodes in slowness array
c i y0_vel = y origin for slowness array
c i dy_vel = y increment for slowness array
c
c i nz_vel = number of z nodes in slowness array
c i z0_vel = z origin for slowness array
c i dz_vel = z increment for slowness array
c
c i s_vel  = slowness array dimensioned real s_vel(nz_vel,nx_vel,ny_vel)
c
c i m_work = number of words in work array work
c i work   = array dimensioned real work(n_work)
c
c o i_err  = error flag 0 = normal exit -1 = error exit
c
c  The travel time table is not necessarily rectangular.  Its shape
c  is defined by
c     1,ix_tab,nx_tab,x0_tab,dx_tab
c     1       ,ny_tab,y0_tab,dy_tab
c     1,iz_tab,nz_tab,z0_tab,dz_tab
c
c  travel time tables will consist of ny_tab y slices
c  starting at y0_tab and incrementing by dy_tab
c  each y slice may have a different number of x columns in it
c  for the iy th y slice
c  the first x column will be the ix_tab(iy) + 1 column
c  the number of x columns will be nx_tab(iy)  and the first
c  x value of that slice will be x0_tab(iy)
c  the x incrment is constant for all y slices and is dx_tab
c  the times for the ix th column of slice iy
c  will start at sample iz_tab(ix_tab(iy)+ix) + 1 within the travel time
c  that column will have nz_tab(ix_tab(iy)+ix)) depth points in it
c  and start at depth z0_tab(ix_tab(iy)+ix)
c  each x column may start at a different depth
c  note x0_tab and y0_tab are measured from the source location
c  z0_tab is measured from the global origin

      implicit  none

      integer   util_len_r

      real      util_invert_1

      integer   n_dim

      real      x_src,y_src,z_src

      real      dr_tab

      integer   m_xyz_tab

      integer  ix_tab(1),nx_tab(1)
      real     x0_tab(1),dx_tab

      integer  ny_tab
      real     y0_tab,dy_tab

      integer  iz_tab(1),nz_tab(1)
      real     z0_tab(1),dz_tab

      real     t_tab(m_xyz_tab)

      integer  nx_vel
      real     x0_vel,dx_vel
      integer  ny_vel
      real     y0_vel,dy_vel
      integer  nz_vel
      real     z0_vel,dz_vel
      real     s_vel(nz_vel,nx_vel,ny_vel)

      integer  na,nb
      real     a0,a1,b0,b1

      integer  t_scale,num_add,inter_type,m_ray
      real     t0_ray,t1_ray,dt_ray,dr_ray,maxangle

      integer npx_grid,npy_grid,npz_grid
      integer   m_work
      real      work(m_work)

      real stepsize
      real dtaccuracy
      real maxTime
      real anglex1
      real anglex2
      real angley1
      real angley2
      integer angleDown
      real nullTTvalue
      real maxangle_p
      real phase(1)
      real amp(1)
      real q11_tab(1),q12_tab(1),q21_tab(1),q22_tab(1)
      real p3_tab(1)

      character outt_file*(*)
      character outamp_file*(*)
      character outpha_file*(*)
      character outq11_file*(*)
      character outq12_file*(*)
      character outq21_file*(*)
      character outq22_file*(*)
      character outa0_file*(*)
      character outb0_file*(*)
      character outa1_file*(*)
      character outb1_file*(*)
      character oute1x_file*(*)
      character oute1y_file*(*)
      character oute1z_file*(*)
      character oute2x_file*(*)
      character oute2y_file*(*)
      character oute2z_file*(*)
      character outlog_file*(*)
      integer   i_err

      character tim_type*8

      integer   n_xyz_tab
      real      r_max,q_max
      real      t_min,t_max
      real      s_min,s_max
      real      second
      real      t_cpu_0, t_cpu_1

c  initialize the error flag
      i_err = 0

c      t_cpu_0 = second()
c 2d, 3d travel time tables
      if (tim_type(1:1) .eq. 'E') then

c  eikonal solver

c       call ktime_3d_eikonal(x_src,y_src,z_src
c    1,dr_tab
c    1,ix_tab,nx_tab,x0_tab,dx_tab
c    1       ,ny_tab,y0_tab,dy_tab
c    1,iz_tab,nz_tab,z0_tab,dz_tab
c    1,t_tab
c    1,nx_vel,x0_vel,dx_vel
c    1,ny_vel,y0_vel,dy_vel
c    1,nz_vel,z0_vel,dz_vel
c    1,s_vel
c    1,m_work,work
c    1,i_err)
c       if (i_err .ne. 0) goto 999

c  bisect traveltime solver 

      elseif (tim_type(1:1) .eq. 'B') then ! Bisect
 
c       call ktime_3d_bisect(x_src,y_src,z_src
c    1,dr_tab
c    1,ix_tab,nx_tab,x0_tab,dx_tab
c    1       ,ny_tab,y0_tab,dy_tab
c    1,iz_tab,nz_tab,z0_tab,dz_tab
c    1,t_tab
c    1,nx_vel,x0_vel,dx_vel
c    1,ny_vel,y0_vel,dy_vel
c    1,nz_vel,z0_vel,dz_vel
c    1,s_vel
c    1,m_work,work
c    1,i_err)
c       if (i_err .ne. 0) goto 999

c  shell for Stork's code
c     elseif (tim_type(1:4) .eq. 'PRAY') then ! Paraxial raytrace

c       call ktime_3d_pray_cs(x_src,y_src,z_src
c    1,dr_tab
c    1,ix_tab,nx_tab,x0_tab,dx_tab
c    1       ,ny_tab,y0_tab,dy_tab
c    1,iz_tab,nz_tab,z0_tab,dz_tab
c    1,t_tab
c    1,nx_vel,x0_vel,dx_vel
c    1,ny_vel,y0_vel,dy_vel
c    1,nz_vel,z0_vel,dz_vel
c    1,s_vel
c    1,npx_grid,npy_grid,npz_grid
c    1,m_work
c    1,work
c    1,stepsize
c    1,dtaccuracy
c    1,maxTime
c    1,anglex1
c    1,anglex2
c    1,angley1
c    1,angley2
c    1,angleDown
c    1,nullTTvalue
c    1,maxangle_p
c    1,phase
c    1,amp
c    1,phase_tmp
c    1,out3_file,out4_file
c    1,i_err)
c       if (i_err .ne. 0) goto 999

      elseif (tim_type(1:4) .eq. 'DRAY') then ! Dynamic raytrace

        call ktime_3d_raytrace_2(x_src,y_src,z_src
     1,dr_tab
     1,ix_tab,nx_tab,x0_tab,dx_tab
     1       ,ny_tab,y0_tab,dy_tab
     1,iz_tab,nz_tab,z0_tab,dz_tab
     1,t_tab,amp,phase
     1,q11_tab,q12_tab,q21_tab,q22_tab,p3_tab
     1,nx_vel,x0_vel,dx_vel
     1,ny_vel,y0_vel,dy_vel
     1,nz_vel,z0_vel,dz_vel
     1,s_vel
     1,na,a0,a1
     1,nb,b0,b1
     1,t_scale,dt_ray
     1,t0_ray,t1_ray
     1,dr_ray,inter_type,num_add
     1,maxangle,m_ray
     1,m_work
     1,work
     1,outt_file,outamp_file
     1,outpha_file,outq11_file,outq12_file
     1,outq21_file,outq22_file
     1,outa0_file,outb0_file,outa1_file,outb1_file
     1,oute1x_file,oute1y_file,oute1z_file
     1,oute2x_file,oute2y_file,oute2z_file
     1,i_err)
        if (i_err .ne. 0) goto 999

      else    ! if (tim_type(1:1) .eq. 'E') then

        print'('' error in ktable_compute_1 tim_type= '',a8)'
     1,tim_type
        goto 999

      endif    ! if (tim_type(1:1) .eq. 'S') then

c      t_cpu_1 = second()
c convert slowness to velocity
c     if (tim_type(1:1) .eq. 'E' .or.
c    1 tim_type(1:1) .eq. 'B' .or. 
c    1 tim_type(1:3) .eq. 'DRAY')  then
c        call util_invert(nx_vel*ny_vel*nz_vel,s_vel)
c     endif !if (tim_type(1:1) .eq. 'E')  then
c
c  max distance from source
c     r_max    = sqrt(
c    1                                      x0_tab(1)       **2
c    1              +                       y0_tab          **2
c    1              + ((nz_tab(1)-1)*dz_tab+z0_tab(1)-z_src)**2
c    1               )

c  max travel time
c     q_max = r_max / s_vel(1,1,1)

c  get min, max velocity(slowness) s_min here is v_min
c     call util_min_max(s_min,s_max,nx_vel*ny_vel*nz_vel,s_vel)

c  get min, max travel time
c     n_xyz_tab = iz_tab(ix_tab(ny_tab)+nx_tab(ny_tab))
c    1          + nz_tab(ix_tab(ny_tab)+nx_tab(ny_tab))
c     call util_min_max(t_min,t_max,n_xyz_tab           ,t_tab)

      print*,' computational cost in building the table:'
     1,t_cpu_1-t_cpu_0

      return

  999 continue
      print'('' error in ktable_compute_1'')'
      i_err = -1
      return

      end

c23456789012345678901234567890123456789012345678901234567890123456789012
      subroutine read_input(
     1 outt_file,outamp_file
     1,outpha_file,outq11_file,outq12_file
     1,outq21_file,outq22_file
     1,outa0_file,outb0_file,outa1_file,outb1_file
     1,outdet21_file,outdet22_file,outdet23_file
     1,outdet31_file,outdet32_file,outdet33_file
     1,outlog_file,inp_file
     1,vx_file
     1,tim_type
     1,nx_vel,x0_vel,dx_vel
     1,ny_vel,y0_vel,dy_vel
     1,nz_vel,z0_vel,dz_vel
     1,mx_tab,       dx_tab
     1,my_tab,       dy_tab
     1,mz_tab,       dz_tab
     1,xs,ys,zs
     1,na,a0,a1
     1,nb,b0,b1
     1,t_scale,dt_ray
     1,t0_ray,t1_ray
     1,dr_ray,inter_type,num_add
     1,maxangle,m_ray
     1,v0,vx,vy,vz
     1,i_err)
c  read in parameters for ray tracing in this routine.
c  o v0,vx,vy,vz ---- linear velocity coefficients
c  o a0,a1,b0,b1,na,nb ---- angle ranges
c  o t_scale ---- number of micro-steps in each macro-step
c  o inter_type ---- type of arrivals stored in the output grids
c       1--most energetic    2--first arrival
c       3--shortest raypath  4--smallest maximum velocity
c       5--longest time      6--longest raypath
c  o num_add ---- # of macro-steps that each adding rays function is called
c  o t0_ray ---- initial ray tracing time
c  o t1_ray ---- maximum ray tracing time
c  o dt_ray ---- macro trace step in time
c  o dr_ray ---- maximum distance between rays that adding rays is applied
c  o maxangle ---- maximum angle to stop adding new rays and checking boundary
c  o m_ray ----  maximum number of rays inserted
c  o outt_file ---- name of output travel time 
c  o outamp_file ---- name of output amplitude
c  o outpha_file ---- name of output phase shift index
c  o outdet21_file ---- name of output Beylkin determinant component
c  o outdet22_file ---- name of output Beylkin determinant component
c  o outdet23_file ---- name of output Beylkin determinant component
c  o outdet31_file ---- name of output Beylkin determinant component
c  o outdet32_file ---- name of output Beylkin determinant component
c  o outdet33_file ---- name of output Beylkin determinant component
c  o outa1_file ---- name of output polar angle
c  o outb1_file ---- name of output azimuth angle
c  o outlog_file ---- output file of ray tracing information
c  o vx_file ---- name of input velocity file
c  o tim_type ---- traveltime computation type
c      EIK ---- eikonal solver
c      B ------ Bisect raytracing
c      PRAY --- paraxial raytracing
c      DRAY --- dynamic raytracing
c  o nx_vel, x0_vel, dx_vel, ny_vel, y0_vel, dy_vel, nz_vel, z0_vel, dz_vel
c     ---- velocity array parameters
c  o mx_tab, dx_tab, my_tab, dy_tab, mz_tab, dz_tab ---- table parameters
c  o xs, xy, xz ---- source position
      implicit none
 
c     real      util_invert_1
c     integer   util_len_r
      integer   util_fetch_i
      integer   util_fetch_r
      integer   util_fetch_c
 
      character inp_file*(*),vx_file*(*)
      character outt_file*(*),outamp_file*(*)
      character outpha_file*(*),outq11_file*(*)
      character outq12_file*(*),outq21_file*(*)
      character outq22_file*(*)
      character outa0_file*(*),outb0_file*(*)
      character outa1_file*(*),outb1_file*(*)
      character outdet21_file*(*),outdet22_file*(*),outdet23_file*(*)
      character outdet31_file*(*),outdet32_file*(*),outdet33_file*(*)
      character outlog_file*(*)
      character tim_type*(*)
 
      integer   nx_vel
      real      x0_vel,dx_vel
 
      integer   ny_vel
      real      y0_vel,dy_vel
 
      integer   nz_vel
      real      z0_vel,dz_vel

      integer   mx_tab
      real      dx_tab
 
      integer   my_tab
      real      dy_tab
 
      integer   mz_tab
      real      dz_tab
 
      real      xs,ys,zs

      integer   na,nb
      real      a0,a1,b0,b1

      integer  t_scale,num_add,inter_type,m_ray
      real     t0_ray,t1_ray,dt_ray,dr_ray,maxangle

      integer   i_inp_file

      real      v0,vx,vy,vz

      integer   i_err

c  get the input data file name
      inp_file = 'drt.inp'
      call util_open_file(i_inp_file,inp_file,'old','formatted',0,i_err)
      if (i_err .ne. 0) goto 999

c set the logical unit number for read input cards
      call util_put_device(i_inp_file)

c  read linear velocity coefficients
c  note x0,y0,z0 - vx,vy,vz are in input coordinates
      if (util_fetch_r('v0',v0) .eq. 0) v0 = 2000.
      if (util_fetch_r('vx',vx) .eq. 0) vx = 0.
      if (util_fetch_r('vy',vy) .eq. 0) vy = 0.
      if (util_fetch_r('vz',vz) .eq. 0) vz = 0.

c  read angle ranges
      if (util_fetch_r('a0',a0) .eq. 0) a0 = 0.
      if (util_fetch_r('a1',a1) .eq. 0) a1 = 90.
      if (util_fetch_r('b0',b0) .eq. 0) b0 = 0.
      if (util_fetch_r('b1',b1) .eq. 0) b1 = 360.
      if (util_fetch_i('na',na) .eq. 0) na = 10
      if (util_fetch_i('nb',nb) .eq. 0) nb = 30

c  read raytracing parameters
      if (util_fetch_i('t_scale',t_scale) .eq. 0) t_scale = 5
      if (util_fetch_i('inter_type',inter_type) .eq. 0) inter_type = 1
      if (util_fetch_i('num_add',num_add) .eq. 0) num_add = 1
      if (util_fetch_r('t0_ray',t0_ray) .eq. 0) t0_ray = 0.0005
      if (util_fetch_r('t1_ray',t1_ray) .eq. 0) t1_ray = 10.
      if (util_fetch_r('dt_ray',dt_ray) .eq. 0) dt_ray = 0.02
      if (util_fetch_r('dr_ray',dr_ray) .eq. 0) dr_ray = 250.
      if (util_fetch_r('maxangle',maxangle) .eq. 0) maxangle = 180
      if (util_fetch_i('m_ray',m_ray) .eq. 0) m_ray = 10001

c  outt file 
      if (util_fetch_c('outt_file',outt_file) .eq. 0)
     1 outt_file = 'NONE'
 
c  outamp file
      if (util_fetch_c('outamp_file',outamp_file) .eq. 0)
     1 outamp_file = 'NONE'

c  outpha file
      if (util_fetch_c('outpha_file',outpha_file) .eq. 0)
     1 outpha_file = 'NONE'
 
c  outq11 file
      if (util_fetch_c('outq11_file',outq11_file) .eq. 0)
     1 outq11_file = 'NONE'

c  outq12 file
      if (util_fetch_c('outq12_file',outq12_file) .eq. 0)
     1 outq12_file = 'NONE'

c  outq21 file
      if (util_fetch_c('outq21_file',outq21_file) .eq. 0)
     1 outq21_file = 'NONE'

c  outq22 file
      if (util_fetch_c('outq22_file',outq22_file) .eq. 0)
     1 outq22_file = 'NONE'

c  outa0 file
      if (util_fetch_c('outa0_file',outa0_file) .eq. 0)
     1 outa0_file = 'NONE'

c  outb0 file
      if (util_fetch_c('outb0_file',outb0_file) .eq. 0)
     1 outb0_file = 'NONE'

c  outa1 file
      if (util_fetch_c('outa1_file',outa1_file) .eq. 0)
     1 outa1_file = 'NONE'

c  outb1 file
      if (util_fetch_c('outb1_file',outb1_file) .eq. 0)
     1 outb1_file = 'NONE'

c  outdet21 file
      if (util_fetch_c('outdet21_file',outdet21_file) .eq. 0)
     1 outdet21_file = 'NONE'

c  outdet22 file
      if (util_fetch_c('outdet22_file',outdet22_file) .eq. 0)
     1 outdet22_file = 'NONE'

c  outdet23 file
      if (util_fetch_c('outdet23_file',outdet23_file) .eq. 0)
     1 outdet23_file = 'NONE'

c  outdet31 file
      if (util_fetch_c('outdet31_file',outdet31_file) .eq. 0)
     1 outdet31_file = 'NONE'

c  outdet32 file
      if (util_fetch_c('outdet32_file',outdet32_file) .eq. 0)
     1 outdet32_file = 'NONE'

c  outdet33 file
      if (util_fetch_c('outdet33_file',outdet33_file) .eq. 0)
     1 outdet33_file = 'NONE'

c  outlog file
      if (util_fetch_c('outlog_file',outlog_file) .eq. 0)
     1 outlog_file = 'NONE'

c  vx file
      if (util_fetch_c('vx_file',vx_file) .eq. 0)
     1 vx_file = 'NONE'

c time compute type
      if (util_fetch_c('tim_type',tim_type) .eq. 0)
     1 tim_type = 'EIKONAL'
 
c velocity grid information about velocity model
      if (util_fetch_i('nx_vel',nx_vel) .eq. 0) nx_vel = 1
      if (util_fetch_r('x0_vel',x0_vel) .eq. 0) x0_vel = 0.
      if (util_fetch_r('dx_vel',dx_vel) .eq. 0) dx_vel = 1.
 
      if (util_fetch_i('ny_vel',ny_vel) .eq. 0) ny_vel = 1
      if (util_fetch_r('y0_vel',y0_vel) .eq. 0) y0_vel = 0.
      if (util_fetch_r('dy_vel',dy_vel) .eq. 0) dy_vel = 1.
 
      if (util_fetch_i('nz_vel',nz_vel) .eq. 0) nz_vel = 1
      if (util_fetch_r('z0_vel',z0_vel) .eq. 0) z0_vel = 0.
      if (util_fetch_r('dz_vel',dz_vel) .eq. 0) dz_vel = 1.
 
c  travel time table grid information
      if (util_fetch_i('mx_tab',mx_tab) .eq. 0) mx_tab = 1
      if (util_fetch_r('dx_tab',dx_tab) .eq. 0) dx_tab = 1.
 
      if (util_fetch_i('my_tab',my_tab) .eq. 0) my_tab = 1
      if (util_fetch_r('dy_tab',dy_tab) .eq. 0) dy_tab = 1.
 
      if (util_fetch_i('mz_tab',mz_tab) .eq. 0) mz_tab = 1
      if (util_fetch_r('dz_tab',dz_tab) .eq. 0) dz_tab = 1.
 
c  source location information
      if (util_fetch_r('xs',xs) .eq. 0) xs = 0.
      if (util_fetch_r('ys',ys) .eq. 0) ys = 0.
      if (util_fetch_r('zs',zs) .eq. 0) zs = 0.

      return
  999 continue
      print'('' table_read_input error'')'
      i_err = -1
      return
 
      end
c23456789012345678901234567890123456789012345678901234567890123456789012
      subroutine write_table_slice(outt_file,outamp_file
     1,outpha_file,outq11_file,outq12_file
     1,outq21_file,outq22_file,outp3_file
     1,ix_tab,nx_tab,x0_tab,dx_tab
     1       ,ny_tab,y0_tab,dy_tab
     1,iz_tab,nz_tab,z0_tab,dz_tab
     1,n_xyz_tab
     1,t_tab,amp,phase
     1,q11_tab,q12_tab,q21_tab,q22_tab,p3_tab
     1,i_err
     1)
      implicit  none

      character outt_file*(*)
      character outamp_file*(*)
      character outpha_file*(*)
      character outq11_file*(*)
      character outq12_file*(*)
      character outq21_file*(*)
      character outq22_file*(*)
      character outp3_file*(*)

      integer   n_xyz_tab
 
      integer  ix_tab(1),nx_tab(1)
      real     x0_tab(1),dx_tab
 
      integer  ny_tab
      real     y0_tab,dy_tab
 
      integer  iz_tab(1),nz_tab(1)
      real     z0_tab(1),dz_tab
 
      real     t_tab(n_xyz_tab)
      real     amp(n_xyz_tab)
      real     phase(n_xyz_tab)
      real     q11_tab(n_xyz_tab),q12_tab(n_xyz_tab)
      real     q21_tab(n_xyz_tab),q22_tab(n_xyz_tab)
      real     p3_tab(n_xyz_tab)

      real     xs,ys
      real     x, y, z
      integer  i_xy, i_xy0, ix, iy, iz
      integer  i_xyz
 
      integer  ix1,iy1

      integer  i_out1,i_out2,i_out3,i_out4
      integer  i_out5,i_out6,i_out7,i_out8

      integer  i_err

      ix1 = nx_tab(1)/2 + 1
      iy1 = ny_tab/2 +1

      if (outt_file(1:4) .ne. 'NONE') then
         OPEN(11,FILE=outt_file,STATUS='unknown')
         CLOSE(11)
         OPEN(11,FILE=outt_file,STATUS='OLD')
         i_out1=1
      else
	 i_out1=0
      endif

      if (outamp_file(1:4) .ne. 'NONE') then
         OPEN(12,FILE=outamp_file,STATUS='unknown')
         CLOSE(12)
         OPEN(12,FILE=outamp_file,STATUS='OLD')
         i_out2=1
      else
         i_out2=0
      endif

      if (outpha_file(1:4) .ne. 'NONE') then
         OPEN(13,FILE=outpha_file,STATUS='unknown')
         CLOSE(13)
         OPEN(13,FILE=outpha_file,STATUS='OLD')
         i_out3=1
      else
         i_out3=0
      endif

      if (outq11_file(1:4) .ne. 'NONE') then
         OPEN(14,FILE=outq11_file,STATUS='unknown')
         CLOSE(14)
         OPEN(14,FILE=outq11_file,STATUS='OLD')
         i_out4=1
      else
         i_out4=0
      endif

      if (outq12_file(1:4) .ne. 'NONE') then
         OPEN(15,FILE=outq12_file,STATUS='unknown')
         CLOSE(15)
         OPEN(15,FILE=outq12_file,STATUS='OLD')
         i_out5=1
      else
         i_out5=0
      endif


      if (outq21_file(1:4) .ne. 'NONE') then
         OPEN(16,FILE=outq21_file,STATUS='unknown')
         CLOSE(16)
         OPEN(16,FILE=outq21_file,STATUS='OLD')
         i_out6=1
      else
         i_out6=0
      endif

      if (outq22_file(1:4) .ne. 'NONE') then
         OPEN(17,FILE=outq22_file,STATUS='unknown')
         CLOSE(17)
         OPEN(17,FILE=outq22_file,STATUS='OLD')
         i_out7=1
      else
         i_out7=0
      endif

      if (outp3_file(1:4) .ne. 'NONE') then
         OPEN(18,FILE=outp3_file,STATUS='unknown')
         CLOSE(18)
         OPEN(18,FILE=outp3_file,STATUS='OLD')
         i_out8=1
      else
         i_out8=0
      endif

      xs = x0_tab(1) + (nz_tab(1)-1)*dx_tab/2.0	
      ys = y0_tab + (ny_tab-1)*dy_tab/2.0

      do i_xyz=1, n_xyz_tab

         iz = mod(i_xyz, nz_tab(1))
         i_xy0 = i_xyz/nz_tab(1)
         if (iz .eq. 0) then
             iz = nz_tab(1)
             i_xy0= i_xy0 -1
         endif
         ix = mod(i_xy0,nx_tab(1)) + 1
         iy = i_xy0/nx_tab(1) + 1

         y = y0_tab + (iy - 1) * dy_tab
         x = x0_tab(iy) + (ix - 1) * dx_tab
         i_xy = ix_tab(iy) + ix
         z = z0_tab(i_xy) + (iz - 1) * dz_tab

         if (i_out1 .eq. 1 )
     1    write(11,*) t_tab(i_xyz)

         if (i_out2 .eq. 1 )
     1    write(12,*) amp(i_xyz)

         if (i_out3 .eq. 1 )
     1    write(13,*) phase(i_xyz)

	 if (i_out4 .eq. 1)
     1    write(14,*) q11_tab(i_xyz)

         if (i_out5 .eq. 1)
     1    write(15,*) q12_tab(i_xyz)

         if (i_out6 .eq. 1)
     1    write(16,*) q21_tab(i_xyz)

         if (i_out7 .eq. 1)
     1    write(17,*) q22_tab(i_xyz)

         if (i_out8 .eq. 1)
     1    write(18,*) p3_tab(i_xyz)

      enddo !  do i_xyz=1, mx_tab*my_tab*mz_tab


      if (i_out1 .eq. 1) close(11)
      if (i_out2 .eq. 1) close(12)
      if (i_out3 .eq. 1) close(13)
      if (i_out4 .eq. 1) close(14)
      if (i_out5 .eq. 1) close(15)
      if (i_out6 .eq. 1) close(16)
      if (i_out7 .eq. 1) close(17)
      if (i_out8 .eq. 1) close(18)

      return

      end

c23456789012345678901234567890123456789012345678901234567890123456789012
      subroutine write_tables(outt_file,outamp_file
     1,outpha_file,outq11_file,outq12_file
     1,outq21_file,outq22_file
     1,outa0_file,outb0_file,outa1_file,outb1_file
     1,outdet21_file,outdet22_file,outdet23_file
     1,outdet31_file,outdet32_file,outdet33_file
     1,ix_tab,nx_tab,x0_tab,dx_tab
     1       ,ny_tab,y0_tab,dy_tab
     1,iz_tab,nz_tab,z0_tab,dz_tab
     1,n_xyz_tab
     1,t_tab,amp,phase
     1,q11_tab,q12_tab,q21_tab,q22_tab
     1,a0_tab,b0_tab,a1_tab,b1_tab
     1,det21_tab,det22_tab,det23_tab
     1,det31_tab,det32_tab,det33_tab
     1,i_err
     1)
c  write computed tables into files

      implicit  none

      character outt_file*(*)
      character outamp_file*(*)
      character outpha_file*(*)
      character outq11_file*(*)
      character outq12_file*(*)
      character outq21_file*(*)
      character outq22_file*(*)
      character outa0_file*(*)
      character outb0_file*(*)
      character outa1_file*(*)
      character outb1_file*(*)
      character outdet21_file*(*)
      character outdet22_file*(*)
      character outdet23_file*(*)
      character outdet31_file*(*)
      character outdet32_file*(*)
      character outdet33_file*(*)

      integer   n_xyz_tab
 
      integer  ix_tab(1),nx_tab(1)
      real     x0_tab(1),dx_tab
 
      integer  ny_tab
      real     y0_tab,dy_tab
 
      integer  iz_tab(1),nz_tab(1)
      real     z0_tab(1),dz_tab
 
      real     t_tab(n_xyz_tab)
      real     amp(n_xyz_tab)
      real     phase(n_xyz_tab)
      real     q11_tab(n_xyz_tab),q12_tab(n_xyz_tab)
      real     q21_tab(n_xyz_tab),q22_tab(n_xyz_tab)
      real     a0_tab(n_xyz_tab),b0_tab(n_xyz_tab)
      real     a1_tab(n_xyz_tab),b1_tab(n_xyz_tab)
      real     det21_tab(n_xyz_tab),det22_tab(n_xyz_tab)
      real     det23_tab(n_xyz_tab)
      real     det31_tab(n_xyz_tab),det32_tab(n_xyz_tab)
      real     det33_tab(n_xyz_tab)

      real     x, y, z
      integer  i_xy, i_xy0, ix, iy, iz
      integer  i_xyz
 
      integer  ix1,iy1

      integer  i_out1,i_out2,i_out3,i_out4
      integer  i_out5,i_out6,i_out7,i_out8
      integer  i_out9,i_out10,i_out11,i_out12
      integer  i_out13,i_out14,i_out15,i_out16
      integer  i_out17

      integer  i_err

      ix1 = nx_tab(1)/2 + 1
      iy1 = ny_tab/2 +1

      if (outt_file(1:4) .ne. 'NONE') then
         OPEN(11,FILE=outt_file,STATUS='unknown')
         CLOSE(11)
         OPEN(11,FILE=outt_file,STATUS='OLD')
         i_out1=1
      else
	 i_out1=0
      endif

      if (outamp_file(1:4) .ne. 'NONE') then
         OPEN(12,FILE=outamp_file,STATUS='unknown')
         CLOSE(12)
         OPEN(12,FILE=outamp_file,STATUS='OLD')
         i_out2=1
      else
         i_out2=0
      endif

      if (outpha_file(1:4) .ne. 'NONE') then
         OPEN(13,FILE=outpha_file,STATUS='unknown')
         CLOSE(13)
         OPEN(13,FILE=outpha_file,STATUS='OLD')
         i_out3=1
      else
         i_out3=0
      endif

      if (outq11_file(1:4) .ne. 'NONE') then
         OPEN(14,FILE=outq11_file,STATUS='unknown')
         CLOSE(14)
         OPEN(14,FILE=outq11_file,STATUS='OLD')
         i_out4=1
      else
         i_out4=0
      endif

      if (outq12_file(1:4) .ne. 'NONE') then
         OPEN(15,FILE=outq12_file,STATUS='unknown')
         CLOSE(15)
         OPEN(15,FILE=outq12_file,STATUS='OLD')
         i_out5=1
      else
         i_out5=0
      endif


      if (outq21_file(1:4) .ne. 'NONE') then
         OPEN(16,FILE=outq21_file,STATUS='unknown')
         CLOSE(16)
         OPEN(16,FILE=outq21_file,STATUS='OLD')
         i_out6=1
      else
         i_out6=0
      endif

      if (outq22_file(1:4) .ne. 'NONE') then
         OPEN(17,FILE=outq22_file,STATUS='unknown')
         CLOSE(17)
         OPEN(17,FILE=outq22_file,STATUS='OLD')
         i_out7=1
      else
         i_out7=0
      endif

      if (outa0_file(1:4) .ne. 'NONE') then
         OPEN(18,FILE=outa0_file,STATUS='unknown')
         CLOSE(18)
         OPEN(18,FILE=outa0_file,STATUS='OLD')
         i_out8=1
      else
         i_out8=0
      endif

      if (outb0_file(1:4) .ne. 'NONE') then
         OPEN(19,FILE=outb0_file,STATUS='unknown')
         CLOSE(19)
         OPEN(19,FILE=outb0_file,STATUS='OLD')
         i_out9=1
      else
         i_out9=0
      endif

      if (outa1_file(1:4) .ne. 'NONE') then
         OPEN(20,FILE=outa1_file,STATUS='unknown')
         CLOSE(20)
         OPEN(20,FILE=outa1_file,STATUS='OLD')
         i_out10=1
      else
         i_out10=0
      endif

      if (outb1_file(1:4) .ne. 'NONE') then
         OPEN(21,FILE=outb1_file,STATUS='unknown')
         CLOSE(21)
         OPEN(21,FILE=outb1_file,STATUS='OLD')
         i_out11=1
      else
         i_out11=0
      endif

      if (outdet21_file(1:4) .ne. 'NONE') then
         OPEN(22,FILE=outdet21_file,STATUS='unknown')
         CLOSE(22)
         OPEN(22,FILE=outdet21_file,STATUS='OLD')
         i_out12=1
      else
         i_out12=0
      endif

      if (outdet22_file(1:4) .ne. 'NONE') then
         OPEN(23,FILE=outdet22_file,STATUS='unknown')
         CLOSE(23)
         OPEN(23,FILE=outdet22_file,STATUS='OLD')
         i_out13=1
      else
         i_out13=0
      endif

      if (outdet23_file(1:4) .ne. 'NONE') then
         OPEN(24,FILE=outdet23_file,STATUS='unknown')
         CLOSE(24)
         OPEN(24,FILE=outdet23_file,STATUS='OLD')
         i_out14=1
      else
         i_out14=0
      endif

      if (outdet31_file(1:4) .ne. 'NONE') then
         OPEN(25,FILE=outdet31_file,STATUS='unknown')
         CLOSE(25)
         OPEN(25,FILE=outdet31_file,STATUS='OLD')
         i_out15=1
      else
         i_out15=0
      endif

      if (outdet32_file(1:4) .ne. 'NONE') then
         OPEN(26,FILE=outdet32_file,STATUS='unknown')
         CLOSE(26)
         OPEN(26,FILE=outdet32_file,STATUS='OLD')
         i_out16=1
      else
         i_out16=0
      endif

      if (outdet33_file(1:4) .ne. 'NONE') then
         OPEN(27,FILE=outdet33_file,STATUS='unknown')
         CLOSE(27)
         OPEN(27,FILE=outdet33_file,STATUS='OLD')
         i_out17=1
      else
         i_out17=0
      endif

      do i_xyz=1, n_xyz_tab

         iz = mod(i_xyz, nz_tab(1))
         i_xy0 = i_xyz/nz_tab(1)
         if (iz .eq. 0) then
             iz = nz_tab(1)
             i_xy0= i_xy0 -1
         endif
         ix = mod(i_xy0,nx_tab(1)) + 1
         iy = i_xy0/nx_tab(1) + 1

         y = y0_tab + (iy - 1) * dy_tab
         x = x0_tab(iy) + (ix - 1) * dx_tab
         i_xy = ix_tab(iy) + ix
         z = z0_tab(i_xy) + (iz - 1) * dz_tab

         if (i_out1 .eq. 1 )
     1    write(11,*) t_tab(i_xyz)

         if (i_out2 .eq. 1 )
     1    write(12,*) amp(i_xyz)

         if (i_out3 .eq. 1 )
     1    write(13,*) phase(i_xyz)

	 if (i_out4 .eq. 1)
     1    write(14,*) q11_tab(i_xyz)

         if (i_out5 .eq. 1)
     1    write(15,*) q12_tab(i_xyz)

         if (i_out6 .eq. 1)
     1    write(16,*) q21_tab(i_xyz)

         if (i_out7 .eq. 1)
     1    write(17,*) q22_tab(i_xyz)

         if (i_out8 .eq. 1)
     1    write(18,*) a0_tab(i_xyz)

         if (i_out9 .eq. 1)
     1    write(19,*) b0_tab(i_xyz)

         if (i_out10 .eq. 1)
     1    write(20,*) a1_tab(i_xyz)

         if (i_out11 .eq. 1)
     1    write(21,*) b1_tab(i_xyz)

         if (i_out12 .eq. 1)
     1    write(22,*) det21_tab(i_xyz)

         if (i_out13 .eq. 1)
     1    write(23,*) det22_tab(i_xyz)

         if (i_out14 .eq. 1)
     1    write(24,*) det23_tab(i_xyz)

         if (i_out15 .eq. 1)
     1    write(25,*) det31_tab(i_xyz)

         if (i_out16 .eq. 1)
     1    write(26,*) det32_tab(i_xyz)

         if (i_out17 .eq. 1)
     1    write(27,*) det33_tab(i_xyz)

      enddo !  do i_xyz=1, mx_tab*my_tab*mz_tab


      if (i_out1 .eq. 1) close(11)
      if (i_out2 .eq. 1) close(12)
      if (i_out3 .eq. 1) close(13)
      if (i_out4 .eq. 1) close(14)
      if (i_out5 .eq. 1) close(15)
      if (i_out6 .eq. 1) close(16)
      if (i_out7 .eq. 1) close(17)
      if (i_out8 .eq. 1) close(18)
      if (i_out9 .eq. 1) close(19)
      if (i_out10 .eq. 1) close(20)
      if (i_out11 .eq. 1) close(21)
      if (i_out12 .eq. 1) close(22)
      if (i_out13 .eq. 1) close(23)
      if (i_out14 .eq. 1) close(24)
      if (i_out15 .eq. 1) close(25)
      if (i_out16 .eq. 1) close(26)
      if (i_out17 .eq. 1) close(27)

      return

      end
