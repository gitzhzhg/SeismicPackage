* Copyright (c) Colorado School of Mines, 2004.
* All rights reserved.

c23456789012345678901234567890123456789012345678901234567890123456789012
      subroutine ktime_3d_raytrace_2(xs,ys,zs
     1,dr_tab
     1,ix_tab,nx_tab,x0_tab,dx_tab
     1       ,ny_tab,y0_tab,dy_tab
     1,iz_tab,nz_tab,z0_tab,dz_tab
     1,t_tab,amp_tab,phase_tab
     1,q11_tab,q12_tab,q21_tab,q22_tab,p3_tab
     1,nx_vel,x0_vel,dx_vel
     1,ny_vel,y0_vel,dy_vel
     1,nz_vel,z0_vel,dz_vel
     1,vel
     1,na_ray,a0,a1
     1,nb_ray,b0,b1
     1,t_scale,dt_ray
     1,t0_ray,t1_ray
     1,dr_ray,inter_type,num_add
     1,maxangle,m_ray
     1,m_work,work
     1,outt_file,outamp_file
     1,outpha_file,outq11_file,outq12_file
     1,outq21_file,outq22_file
     1,outa0_file,outb0_file,outa1_file,outb1_file
     1,oute1x_file,oute1y_file,oute1z_file
     1,oute2x_file,oute2y_file,oute2z_file
     1,i_err)
c  compute a signle travel time/amplitude table
c  compute velocity derivatives

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
      character oute1x_file*(*)
      character oute1y_file*(*)
      character oute1z_file*(*)
      character oute2x_file*(*)
      character oute2y_file*(*)
      character oute2z_file*(*)

      real     xs,ys,zs
 
      integer  ix_tab(1),nx_tab(1)
      real     x0_tab(1),dx_tab
 
      integer  ny_tab
      real     y0_tab,dy_tab
 
      integer  iz_tab(1),nz_tab(1)
      real     z0_tab(1),dz_tab
 
      real     t_tab(1),amp_tab(1)
      real  phase_tab(1)
      real     q11_tab(1),q12_tab(1),q21_tab(1),q22_tab(1)
      real     p3_tab(1)
 
      integer  nx_vel
      real     x0_vel,dx_vel
 
      integer  ny_vel
      real     y0_vel,dy_vel
      integer  nz_vel
      real     z0_vel,dz_vel
 
      real     vel(nz_vel,nx_vel,ny_vel)

      integer  nx_vel_rt
      real     x0_vel_rt,dx_vel_rt,x1_vel_rt
 
      integer  ny_vel_rt
      real     y0_vel_rt,dy_vel_rt,y1_vel_rt
 
      integer  nz_vel_rt
      real     z0_vel_rt,dz_vel_rt
 
      real     dr_tab
 
      integer  na_ray, nb_ray
      real     a0, a1, b0, b1

      integer  t_scale,num_add,inter_type,m_ray
      real     t0_ray,t1_ray,dt_ray,dr_ray,maxangle

      integer  m_work
      real     work(m_work)
 
      real nullTTvalue
      parameter (nullTTvalue=0.0)
      integer  n_xyz_tab
 
      integer i_work_i,i_work_n
 
      integer   nx_tab_min,nx_tab_max
      integer   nz_tab_min,nz_tab_max
 
      real    x0_tab_min, x0_tab_max
      real    z0_tab_min, z0_tab_max
 
      integer i_err,i,j,k
 
      real    second,t_cpu_1, t_cpu_2
c  work space pointers
      integer  i_work,n_work
      integer  j_vel, n_j_vel
 
      integer  ix, iy
 
c     t_cpu_1 = second()
 
      call util_min_max_i(nx_tab_min,nx_tab_max,ny_tab,nx_tab)
      call util_min_max_i(nz_tab_min,nz_tab_max
     1,ix_tab(ny_tab)+nx_tab(ny_tab),nz_tab)
 
      call util_min_max(x0_tab_min,x0_tab_max,ny_tab,x0_tab)
      call util_min_max(z0_tab_min,z0_tab_max
     1,ix_tab(ny_tab)+nx_tab(ny_tab),z0_tab)
 
c allocate extra space for the first and second derivatives of velocity field
c all together work(j_vel) has 10 times space of the original velocity array
c (nz_vel,nx_vel,ny_vel,10)
c 1=velocity, 2=dv/dx, 3=dv/dy, 4=dv/dz,
c 5=ddv2/dxdy, 6=ddv2/dxdz, 7=ddv2/dydz,
c 8=ddv2/dxdx, 9=ddv2/dydy, 10=ddv2/dzdz

      ix = (x0_tab_min +xs - x0_vel)/dx_vel
      x0_vel_rt = x0_vel + (ix -1 ) * dx_vel
      x0_vel_rt = max(x0_vel_rt,x0_vel)
      ix = (x0_tab_min+dx_tab*(nx_tab_max-1) + xs - x0_vel_rt )/dx_vel
      x1_vel_rt = x0_vel_rt + ix * dx_vel
      x1_vel_rt = min(x1_vel_rt, x0_vel+(nx_vel-1)*dx_vel)
      nx_vel_rt = (x1_vel_rt - x0_vel_rt)/dx_vel + 1
      if (nx_vel_rt .le. 0 ) then
          x0_vel_rt = x0_vel
          nx_vel_rt = nx_vel
      endif !if (nx_vel_rt .le. 0 ) then
      dx_vel_rt = dx_vel

      iy = (y0_tab +ys - y0_vel)/dy_vel
      y0_vel_rt = y0_vel + (iy -1 ) * dy_vel
      y0_vel_rt = max(y0_vel_rt,y0_vel)
      iy = (y0_tab+dy_tab*(ny_tab-1) + ys - y0_vel_rt )/dy_vel
      y1_vel_rt = y0_vel_rt + iy * dy_vel
      y1_vel_rt = min(y1_vel_rt, y0_vel+(ny_vel-1)*dy_vel)
      ny_vel_rt = (y1_vel_rt - y0_vel_rt)/dy_vel + 1
      if (ny_vel_rt .le. 0 ) then
          y0_vel_rt = y0_vel
          ny_vel_rt = ny_vel
      endif !if (nx_vel_rt .le. 0 ) then
      dy_vel_rt = dy_vel
 
      z0_vel_rt = z0_vel
      nz_vel_rt = nz_vel
      dz_vel_rt = dz_vel

      x0_vel_rt = x0_vel
      nx_vel_rt = nx_vel
      dx_vel_rt = dx_vel
      
      y0_vel_rt = y0_vel
      ny_vel_rt = ny_vel
      dy_vel_rt = dy_vel
      
      n_j_vel = nx_vel_rt * ny_vel_rt * nz_vel_rt * 10
      call util_wors(i_work_i,i_work_n,m_work)
      call util_work(i_work_i,i_work_n,j_vel ,n_j_vel )
      call util_worc(i_work_i,i_work_n,i_err)
      if (i_err .ne. 0) goto 999

      call util_worl(i_work_i,i_work_n,n_work)
      call util_work(i_work_i,i_work_n,i_work,n_work)

c  compute velocity derivatives
 
          call util_invert(nx_vel*ny_vel*nz_vel,vel) !convert slowness to v*
 
          call ktime_3d_compute_v_grad_coef(
     1 nz_vel,z0_vel,dz_vel
     1,nx_vel,x0_vel,dx_vel
     1,ny_vel,y0_vel,dy_vel
     1,vel
     1,nz_vel_rt,z0_vel_rt,dz_vel_rt
     1,nx_vel_rt,x0_vel_rt,dx_vel_rt
     1,ny_vel_rt,y0_vel_rt,dy_vel_rt
     1,work(j_vel)
     1)
          call util_invert(nx_vel*ny_vel*nz_vel,vel) !convert slowness to v*
 
c  initialize the travel time table array to a large number (100.)
      n_xyz_tab = iz_tab(ix_tab(ny_tab)+nx_tab(ny_tab))
     1          + nz_tab(ix_tab(ny_tab)+nx_tab(ny_tab))
      call util_setr(n_xyz_tab,t_tab,0.)
      call util_setr(n_xyz_tab,amp_tab,100.)
      call util_setr(n_xyz_tab,phase_tab,0.0)
      call util_setr(n_xyz_tab,q11_tab,0.)
      call util_setr(n_xyz_tab,q12_tab,0.)
      call util_setr(n_xyz_tab,q21_tab,0.)
      call util_setr(n_xyz_tab,q22_tab,0.)
      call util_setr(n_xyz_tab,p3_tab,1.)
 
c compute the ray data through wavefront construction dynamic ray tracing

      print*,' max memory in ktime_3d_raytrace:',m_work
      print*,' max memory pass into the next  :',n_work

         call ktime_3d_raytrace_1(xs,ys,zs
     1,dr_tab
     1,ix_tab,nx_tab,x0_tab,dx_tab
     1       ,ny_tab,y0_tab,dy_tab
     1,iz_tab,nz_tab,z0_tab,dz_tab
     1,t_tab,amp_tab,phase_tab
     1,q11_tab,q12_tab,q21_tab,q22_tab,p3_tab
     1,nx_vel_rt,x0_vel_rt,dx_vel_rt
     1,ny_vel_rt,y0_vel_rt,dy_vel_rt
     1,nz_vel_rt,z0_vel_rt,dz_vel_rt
     1,work(j_vel)
     1,na_ray,nb_ray
     1,a0,a1,b0,b1
     1,t_scale,dt_ray
     1,t0_ray,t1_ray
     1,dr_ray,inter_type,num_add
     1,maxangle,m_ray
     1,n_work,work(i_work)
     1,outt_file,outamp_file
     1,outpha_file,outq11_file,outq12_file
     1,outq21_file,outq22_file
     1,outa0_file,outb0_file,outa1_file,outb1_file
     1,oute1x_file,oute1y_file,oute1z_file
     1,oute2x_file,oute2y_file,oute2z_file
     1,i_err)
        if (i_err .ne. 0) goto 999

c     t_cpu_2 = second()

c     print*,'time cost in ktime_3d_raytrace:',t_cpu_2-t_cpu_1
c     print*,' max memory in ktime_3d_raytrace:',m_work
c     print*,' max memory pass into the next  :',n_work

      
      return
  999 continue
      print'('' error in ktime_3d_raytrace'')'
      i_err = -1
      return
 
      end

c23456789012345678901234567890123456789012345678901234567890123456789012
      subroutine ktime_3d_raytrace_memory(n_work)
c  initalize the number of word savaliable to n_work
c  and the pointer to 1
      implicit none
      integer n_work

      n_work = 8000000

      print'('' in ktime_3d_raytrace_memory: n_work='',i10)',n_work 
      return
      end
c&&&
c12345678901234567890123456789012345678901234567890123456789012345678901_
      subroutine ktime_3d_raytrace_1(xs,ys,zs
     1,dr_tab
     1,ix_tab,nx_tab,x0_tab,dx_tab
     1       ,ny_tab,y0_tab,dy_tab
     1,iz_tab,nz_tab,z0_tab,dz_tab
     1,t_tab,amp_tab,phase_tab
     1,q11_tab,q12_tab,q21_tab,q22_tab,p3_tab
     1,nx_vel,x0_vel,dx_vel
     1,ny_vel,y0_vel,dy_vel
     1,nz_vel,z0_vel,dz_vel
     1,vel
     1,na_ray,nb_ray
     1,a0_ray,a1_ray
     1,b0_ray,b1_ray
     1,t_scale,dt_ray
     1,t0_ray,t1_ray
     1,dr_ray,inter_type,num_add
     1,maxangle,m_ray
     1,m_work,work
     1,outt_file,outamp_file
     1,outpha_file,outq11_file,outq12_file
     1,outq21_file,outq22_file
     1,outa0_file,outb0_file,outa1_file,outb1_file
     1,oute1x_file,oute1y_file,oute1z_file
     1,oute2x_file,oute2y_file,oute2z_file
     1,i_err)
c  compute a signle travel time/amplitude table
c  allocate various work memory units

      implicit none

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

      real     xs,ys,zs,v0,vx,vy,vz

      integer  ix_tab(1),nx_tab(1)
      real     x0_tab(1),dx_tab

      integer            ny_tab
      real     y0_tab,dy_tab

      integer  iz_tab(1),nz_tab(1)
      real     z0_tab(1),dz_tab

      real     t_tab(1), amp_tab(1)
      real     phase_tab(1)
      real     q11_tab(1),q12_tab(1),q21_tab(1),q22_tab(1)
      real     p3_tab(1)

      integer  nx_vel
      real     x0_vel,dx_vel

      integer  ny_vel
      real     y0_vel,dy_vel

      integer  nz_vel
      real     z0_vel,dz_vel

      real     vel(nz_vel,nx_vel,ny_vel,10)

      integer  na_ray,nb_ray
      real     dr_ray

      real     dr_tab

      integer  m_work
      real     work(m_work)

      integer  i_err

      real     a0_ray,da_ray,a1_ray

      real     b0_ray,db_ray,b1_ray

      integer  mt_ray
      real     t0_ray,dt_ray,t1_ray

      real     ds_ray
      real     v_min,v_max

      integer  m_xyz_tab
      integer  m_ray
      integer  m_tube
      integer  mt_micro

c parameter for scaling the micro_mt
      integer  t_scale

c  work space pointers
      integer  i_a_tab,   n_a_tab
      integer  i_b_tab,   n_b_tab
      integer  i_e2z_tab, n_e2z_tab
      integer  i_a_ray   ,n_a_ray 
      integer  i_b_ray   ,n_b_ray 
      integer  i_x_ray   ,n_x_ray 
      integer  i_y_ray   ,n_y_ray 
      integer  i_z_ray   ,n_z_ray 
      integer  i_px_ray  ,n_px_ray
      integer  i_py_ray  ,n_py_ray
      integer  i_pz_ray  ,n_pz_ray
      integer  i_tx_ray  ,n_tx_ray
      integer  i_ty_ray  ,n_ty_ray
      integer  i_tz_ray  ,n_tz_ray
      integer  i_e1x_ray ,n_e1x_ray
      integer  i_e1y_ray ,n_e1y_ray
      integer  i_e1z_ray ,n_e1z_ray
      integer  i_e2x_ray ,n_e2x_ray
      integer  i_e2y_ray ,n_e2y_ray
      integer  i_e2z_ray ,n_e2z_ray
      integer  i_s_ray   ,n_s_ray 
      integer  i_s_tab   ,n_s_tab
      integer  i_v_ray   ,n_v_ray
      integer  i_v_ray_tmp,n_v_ray_tmp
      integer  i_vmax_ray ,n_vmax_ray
      integer  i_vmax_tab ,n_vmax_tab
      integer  i_det33_tab ,n_det33_tab
      integer  i_tc_ray  ,n_tc_ray
      integer  i_qq_ray  ,n_qq_ray
      integer  i_hq_ray  ,n_hq_ray
      integer  i_gs_ray  ,n_gs_ray
      integer  i_phase_ray,n_phase_ray
      integer  i_vq11_ray,n_vq11_ray
      integer  i_vq12_ray,n_vq12_ray
      integer  i_vq22_ray,n_vq22_ray
      integer  i_vx_ray  ,n_vx_ray
      integer  i_vy_ray  ,n_vy_ray
      integer  i_vz_ray  ,n_vz_ray
      integer  i_ir_tube ,n_ir_tube
      integer  i_it_tube ,n_it_tube
      integer  i_is_tube ,n_is_tube
      integer  n_che_index
      integer  i_ia_rays,  n_ia_rays
      integer  i_it_flag,n_it_flag
      integer  i_ir_flag,n_ir_flag
      integer  i_ir_pointer, n_ir_pointer
      integer  i_it_pointer, n_it_pointer
      integer  i_points,     n_points
      integer  i_norm_x, n_norm_x
      integer  i_norm_y, n_norm_y
      integer  i_norm_z, n_norm_z
      integer  i_work    ,n_work
      integer  i_work_i  ,i_work_n

      integer  i,j,k

      integer  num_add, inter_type
      real     maxangle

      integer  i_call
      data     i_call/0/
      i_call = i_call + 1

      m_xyz_tab = 0

c  compute the ray time step length
      call util_min_max(v_min,v_max,nx_vel*ny_vel*nz_vel,vel)

      mt_micro = t_scale
      mt_ray   = int((t1_ray-t0_ray)/dt_ray) + 1
c inter_type indicates interpolation type used in ktime_3d_receiver_4
c       1--most energetic    2--first arrival
c       3--shortest raypath  4--smallest maximum velocity
c       5--longest time      6--longest raypath

      maxangle = maxangle * asin(1.) / 90.
      maxangle = cos(maxangle)

      if (mt_ray .gt. 1e6)stop

      m_tube = m_ray*2   ! maximum number of ray tubes

c  compute the number of samples in the travel time table
      m_xyz_tab = iz_tab(ix_tab(ny_tab)+nx_tab(ny_tab))
     1 + nz_tab(ix_tab(ny_tab)+nx_tab(ny_tab))

c  allocate work space
      n_a_tab    = m_xyz_tab   !
      n_b_tab    = m_xyz_tab 
      n_e2z_tab  = m_xyz_tab 
      n_x_ray    = m_ray  * 2  ! ray x location
      n_y_ray    = m_ray  * 2  ! ray x location
      n_z_ray    = m_ray  * 2  ! ray x location
      n_px_ray   = m_ray  * 2  ! ray x ray parameter
      n_py_ray   = m_ray  * 2  ! ray y ray parameter
      n_pz_ray   = m_ray  * 2  ! ray z ray parameter
      n_tx_ray   = m_ray  * 2  ! ray propogation direction
      n_ty_ray   = m_ray  * 2  ! ray propogation direction
      n_tz_ray   = m_ray  * 2  ! ray propogation direction
      n_e1x_ray  = m_ray  * 2  ! ray center basis
      n_e1y_ray  = m_ray  * 2  ! ray center basis
      n_e1z_ray  = m_ray  * 2  ! ray center basis
      n_e2x_ray  = m_ray  * 2  ! ray center basis
      n_e2y_ray  = m_ray  * 2  ! ray center basis
      n_e2z_ray  = m_ray  * 2  ! ray center basis
      n_vx_ray   = m_ray  * 2  ! dv / dx
      n_vy_ray   = m_ray  * 2  ! dv / dy
      n_vz_ray   = m_ray  * 2  ! dv / dz
      n_tc_ray   = m_ray  * 2  ! dynamic system
      n_qq_ray   = m_ray  * 8  ! dynamic system
      n_hq_ray   = m_ray  * 8  ! dynamic system
      n_gs_ray   = m_ray  * 2  ! geometrical spreading
      n_phase_ray= m_ray  * 2  ! phase along rays
      n_vq11_ray = m_ray  * 2  ! d2v / dq2
      n_vq12_ray = m_ray  * 2  ! d2v / dq2
      n_vq22_ray = m_ray  * 2  ! d2v / dq2
      n_v_ray    = m_ray  * 2  ! ray velocity(x,y,z)
      n_v_ray_tmp= m_ray       ! ray velocity(x,y,z)
      n_vmax_ray = m_ray       ! maximum velocity along the rays
      n_vmax_tab = m_xyz_tab   ! maximum velocity
      n_det33_tab = m_xyz_tab   ! det33
      n_s_ray    = m_ray  * 2  ! ray path length
      n_s_tab    = m_xyz_tab   ! ray path length
      n_a_ray    = m_ray       ! ray angle a
      n_b_ray    = m_ray       ! ray angle b
      n_ir_tube  = m_tube * 3  ! ray tube pointers
      n_it_tube  = m_tube * 3  ! ray tube pointers
      n_is_tube  = m_tube * 3  ! ray tube pointers
      n_che_index= m_tube * 2  ! add tube flag
      n_it_flag  = m_tube      ! tube boundary flag
      n_ir_flag   = m_ray      ! live ray flag
      n_ir_pointer= m_ray      ! pointer to the alive rays
      n_it_pointer= m_tube     ! pointer to the alive tubes
      n_points = m_xyz_tab    !
      n_ia_rays = m_ray * 3   ! add rays index
      n_norm_x = m_tube *2
      n_norm_y = m_tube * 2
      n_norm_z = m_tube * 2

      call util_wors(i_work_i,i_work_n,m_work)
      call util_work(i_work_i,i_work_n,i_a_tab ,n_a_tab )         
      call util_work(i_work_i,i_work_n,i_b_tab ,n_b_tab )         
      call util_work(i_work_i,i_work_n,i_e2z_tab,n_e2z_tab )         
      call util_work(i_work_i,i_work_n,i_x_ray   ,n_x_ray   )
      call util_work(i_work_i,i_work_n,i_y_ray   ,n_y_ray   )
      call util_work(i_work_i,i_work_n,i_z_ray   ,n_z_ray   )
      call util_work(i_work_i,i_work_n,i_px_ray  ,n_px_ray  )
      call util_work(i_work_i,i_work_n,i_py_ray  ,n_py_ray  )
      call util_work(i_work_i,i_work_n,i_pz_ray  ,n_pz_ray  )
      call util_work(i_work_i,i_work_n,i_tx_ray  ,n_tx_ray  )
      call util_work(i_work_i,i_work_n,i_ty_ray  ,n_ty_ray  )
      call util_work(i_work_i,i_work_n,i_tz_ray  ,n_tz_ray  )
      call util_work(i_work_i,i_work_n,i_e1x_ray  ,n_e1x_ray  )
      call util_work(i_work_i,i_work_n,i_e1y_ray  ,n_e1y_ray  )
      call util_work(i_work_i,i_work_n,i_e1z_ray  ,n_e1z_ray  )
      call util_work(i_work_i,i_work_n,i_e2x_ray  ,n_e2x_ray  )
      call util_work(i_work_i,i_work_n,i_e2y_ray  ,n_e2y_ray  )
      call util_work(i_work_i,i_work_n,i_e2z_ray  ,n_e2z_ray  )
      call util_work(i_work_i,i_work_n,i_a_ray   ,n_a_ray   )
      call util_work(i_work_i,i_work_n,i_b_ray   ,n_b_ray   )
      call util_work(i_work_i,i_work_n,i_s_ray   ,n_s_ray   )
      call util_work(i_work_i,i_work_n,i_s_tab   ,n_s_tab   )
      call util_work(i_work_i,i_work_n,i_v_ray   ,n_v_ray   )
      call util_work(i_work_i,i_work_n,i_v_ray_tmp,n_v_ray_tmp)
      call util_work(i_work_i,i_work_n,i_vmax_ray,n_vmax_ray)
      call util_work(i_work_i,i_work_n,i_vmax_tab,n_vmax_tab)
      call util_work(i_work_i,i_work_n,i_det33_tab,n_det33_tab)
      call util_work(i_work_i,i_work_n,i_vx_ray  ,n_vx_ray  )
      call util_work(i_work_i,i_work_n,i_vy_ray  ,n_vy_ray  )
      call util_work(i_work_i,i_work_n,i_vz_ray  ,n_vz_ray  )
      call util_work(i_work_i,i_work_n,i_tc_ray  ,n_tc_ray  )
      call util_work(i_work_i,i_work_n,i_qq_ray  ,n_qq_ray  )
      call util_work(i_work_i,i_work_n,i_hq_ray  ,n_hq_ray  )
      call util_work(i_work_i,i_work_n,i_gs_ray  ,n_gs_ray  )
      call util_work(i_work_i,i_work_n,i_phase_ray,n_phase_ray  )
      call util_work(i_work_i,i_work_n,i_vq11_ray,n_vq11_ray)
      call util_work(i_work_i,i_work_n,i_vq12_ray,n_vq12_ray)
      call util_work(i_work_i,i_work_n,i_vq22_ray,n_vq22_ray)
      call util_work(i_work_i,i_work_n,i_ir_tube ,n_ir_tube )
      call util_work(i_work_i,i_work_n,i_it_tube ,n_it_tube )
      call util_work(i_work_i,i_work_n,i_is_tube ,n_is_tube )
      call util_work(i_work_i,i_work_n,i_ia_rays,n_ia_rays)
      call util_work(i_work_i,i_work_n,i_it_flag ,n_it_flag )
      call util_work(i_work_i,i_work_n,i_ir_flag ,n_ir_flag )
      call util_work(i_work_i,i_work_n,i_it_pointer ,n_it_pointer )
      call util_work(i_work_i,i_work_n,i_ir_pointer ,n_ir_pointer )
      call util_work(i_work_i,i_work_n,i_points ,n_points )
      call util_work(i_work_i,i_work_n,i_norm_x ,n_norm_x )
      call util_work(i_work_i,i_work_n,i_norm_y ,n_norm_y )
      call util_work(i_work_i,i_work_n,i_norm_z ,n_norm_z )

      call util_worc(i_work_i,i_work_n,i_err)
      if (i_err .ne. 0) goto 999
      call util_worl(i_work_i,i_work_n,n_work)
      call util_work(i_work_i,i_work_n,i_work,n_work)

c       print'(/,'' memory='',i10,'' max memory='',i10)'
c    1,n_work,m_work

c  compute travel time table
c 3d has velocity in vel(1,1,1,1)  
c        velocity derivatives in  vel(1,1,1,2~10)  
c 1=velocity, 2=dv/dx, 3=dv/dy, 4=dv/dz,
c 5=ddv2/dxdy, 6=ddv2/dxdz, 7=ddv2/dydz,
c 8=ddv2/dxdx, 9=ddv2/dydy, 10=ddv2/dzdz

        print'('' inter_type='',i2)'
     1,inter_type
        print*,'1---most energetic'
        print*,'2---first arrival'
        print*,'3---shortest raypath'
        print*,'4---smallest maximum velocity'
        print*,'5---longest time'
        print*,'6---longest raypath'

      call ktime_3d_raytrace_0(xs,ys,zs
     1,ix_tab,nx_tab,x0_tab,dx_tab
     1       ,ny_tab,y0_tab,dy_tab
     1,iz_tab,nz_tab,z0_tab,dz_tab
     1,t_tab
     1,nx_vel,x0_vel,dx_vel
     1,ny_vel,y0_vel,dy_vel
     1,nz_vel,z0_vel,dz_vel
     1,vel
     1,m_xyz_tab,m_ray,m_tube
     1,mt_micro,mt_ray,t0_ray,dt_ray,ds_ray
     1,dr_ray
     1,na_ray,a0_ray,da_ray,a1_ray
     1,nb_ray,b0_ray,db_ray,b1_ray
     1,v0,vx,vy,vz
     1,amp_tab,phase_tab
     1,q11_tab,q12_tab,q21_tab,q22_tab,p3_tab
     1,work(i_s_tab),work(i_a_tab),work(i_b_tab),work(i_e2z_tab)
     1,work(i_a_ray ),work(i_b_ray ),work(i_s_ray)
     1,work(i_x_ray ),work(i_y_ray ),work(i_z_ray )
     1,work(i_px_ray),work(i_py_ray),work(i_pz_ray)
     1,work(i_tx_ray),work(i_ty_ray),work(i_tz_ray)
     1,work(i_e1x_ray),work(i_e1y_ray),work(i_e1z_ray)
     1,work(i_e2x_ray),work(i_e2y_ray),work(i_e2z_ray)
     1,work(i_tc_ray)
     1,work(i_qq_ray),work(i_hq_ray),work(i_gs_ray)
     1,work(i_phase_ray)
     1,work(i_v_ray ),work(i_v_ray_tmp),work(i_vq11_ray)
     1,work(i_vq12_ray),work(i_vq22_ray)
     1,work(i_vx_ray),work(i_vy_ray),work(i_vz_ray)
     1,work(i_vmax_ray),work(i_vmax_tab),work(i_det33_tab)
     1,work(i_ir_tube),work(i_is_tube),work(i_it_tube)
     1,work(i_points)
     1,work(i_ia_rays)
     1,work(i_it_flag),work(i_ir_flag)
     1,work(i_ir_pointer),work(i_it_pointer)
     1,work(i_norm_x),work(i_norm_y),work(i_norm_z)
     1,num_add,inter_type,maxangle
     1,n_work,work(i_work)
     1,outt_file,outamp_file
     1,outpha_file,outq11_file,outq12_file
     1,outq21_file,outq22_file
     1,outa0_file,outb0_file,outa1_file,outb1_file
     1,oute1x_file,oute1y_file,oute1z_file
     1,oute2x_file,oute2y_file,oute2z_file
     1,i_err)

      return 

  999 continue
      print'('' error in ktime_3d_raytrace'')'
      i_err = -1
      return

      end

c23456789012345678901234567890123456789012345678901234567890123456789012
      subroutine ktime_3d_raytrace_0(xs,ys,zs
     1,ix_tab,nx_tab,x0_tab,dx_tab
     1       ,ny_tab,y0_tab,dy_tab
     1,iz_tab,nz_tab,z0_tab,dz_tab
     1,t_tab
     1,nx_vel,x0_vel,dx_vel
     1,ny_vel,y0_vel,dy_vel
     1,nz_vel,z0_vel,dz_vel
     1,vel
     1,m_xyz_tab,m_ray,m_tube
     1,mt_micro,mt_ray,t0_ray,dt_macro,ds_ray
     1,dr_ray
     1,na_ray,a0_ray,da_ray,a1_ray
     1,nb_ray,b0_ray,db_ray,b1_ray
     1,v0,vx,vy,vz
     1,amp_tab,phase_tab
     1,q11_tab,q12_tab,q21_tab,q22_tab,p3_tab
     1,s_tab,a_tab,b_tab,e2z_tab
     1,a_ray ,b_ray ,s_ray
     1,x_ray ,y_ray ,z_ray 
     1,px_ray,py_ray,pz_ray
     1,tx_ray,ty_ray,tz_ray
     1,e1x_ray,e1y_ray,e1z_ray
     1,e2x_ray,e2y_ray,e2z_ray
     1,tc_ray
     1,qq_ray,hq_ray,gs_ray,phase_ray
     1,v_ray ,v_ray_tmp,vq11_ray
     1,vq12_ray,vq22_ray
     1,vx_ray,vy_ray,vz_ray
     1,vmax_ray,vmax_tab,det33_tab
     1,ir_tube,is_tube,it_tube
     1,points
     1,ia_rays
     1,it_flag,ir_flag
     1,ir_pointer,it_pointer
     1,norm_x,norm_y,norm_z
     1,num_add,inter_type,maxangle
     1,m_work,work
     1,outt_file,outamp_file
     1,outpha_file,outq11_file,outq12_file
     1,outq21_file,outq22_file
     1,outa0_file,outb0_file,outa1_file,outb1_file
     1,oute1x_file,oute1y_file,oute1z_file
     1,oute2x_file,oute2y_file,oute2z_file
     1,i_err)
c  shell for computing a signle travel time/amplitude table
c  for each macro time step ...
c  1 trace rays from current location out mt_micro micro time steps
c  2 determine the ray spreading centers for each tube
c  3 interpolate from ray positions to travel time table
c  4 update index of tubes and rays that inside the target region
c  5 interpolate rays between pairs that have spread apart too much
c  6 check if all rays have left the travel time table area, if so quit
c
c  tube side i_side of tube i_tube connects to 
c  tube side is_tube(i_side,i_tube) of tube it_tube(i_side,i_tube)
c
      implicit none

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

      real     xs,ys,zs,v0,vx,vy,vz
      real     ds_max

      real     nullvalue 

      integer  ix_tab(1),nx_tab(1)
      real     x0_tab(1),dx_tab

      integer            ny_tab
      real     y0_tab,dy_tab

      integer  iz_tab(1),nz_tab(1)
      real     z0_tab(1),dz_tab

      real     t_tab(1)

      integer  m_xyz, n_xyz_tab
      real     amp_tab(1)
      real     phase_tab(1)
      real     q11_tab(1),q12_tab(1),q21_tab(1),q22_tab(1)
      real     p3_tab(1)
      real     s_tab(1)
      real     a_tab(1)
      real     b_tab(1)
      real     e2z_tab(1)
      integer  nt_num_1,nt_num
      integer  nx_vel
      real     x0_vel,dx_vel

      integer  ny_vel
      real     y0_vel,dy_vel

      integer  nz_vel
      real     z0_vel,dz_vel

c  2d has bicubic spline coefficients in vcof 3d has velocity and derivatives
      real     vel(nz_vel,nx_vel,ny_vel,10)

      integer  mt_micro
      integer  mt_ray
      real     t0_ray,dt_macro,dt_ray

      integer  m_ray

      integer  num_add,inter_type
      real     maxangle

      real     a0_ray,da_ray,a1_ray

      real     b0_ray,db_ray,b1_ray

      real      a_ray(m_ray)
      real      b_ray(m_ray)
      real      s_ray(m_ray,2)
      real      x_ray(m_ray,2)
      real      y_ray(m_ray,2)
      real      z_ray(m_ray,2)
      real     px_ray(m_ray,2)
      real     py_ray(m_ray,2)
      real     pz_ray(m_ray,2)
      real     tx_ray(m_ray,2),ty_ray(m_ray,2),tz_ray(m_ray,2)
      real     e1x_ray(m_ray,2),e1y_ray(m_ray,2),e1z_ray(m_ray,2)
      real     e2x_ray(m_ray,2),e2y_ray(m_ray,2),e2z_ray(m_ray,2)
      real      v_ray(m_ray,2)
      real      v_ray_tmp(m_ray)
      real     tc_ray(m_ray,2)
      real     qq_ray(4,m_ray,2)
      real     hq_ray(4,m_ray,2) 
      real     gs_ray(m_ray,2)
      integer  phase_ray(m_ray,2)
      real     vq11_ray(m_ray,2)
      real     vq12_ray(m_ray,2)
      real     vq22_ray(m_ray,2)
      real     vx_ray(m_ray,2)
      real     vy_ray(m_ray,2)
      real     vz_ray(m_ray,2)
      real     vmax_ray(1)
      real     vmax_tab(1)
      real     det33_tab(1)
      real     vs

      integer  m_tube
      integer  ir_tube(3,m_tube)
      integer  is_tube(3,m_tube)
      integer  ia_rays(3,m_ray)
      integer  it_tube(3,m_tube)
      integer  it_flag(m_tube)
      integer  ir_flag(m_ray)
      integer  ir_pointer(m_ray)
      integer  it_pointer(m_tube)

      real     norm_x(m_tube,2)
      real     norm_y(m_tube,2)
      real     norm_z(m_tube,2)

      integer  m_xyz_tab
      integer  points(m_xyz_tab)

      integer  m_work
      real     work(m_work)

      integer  i_err

      integer  nt_ray,it_ray
      integer  nt_micro
      real     t1_ray,t_ray_1,t_ray_2

      real     ds_ray

      integer  na_add

      integer  i,j,k
      real     x_min,x_max
      real     y_min,y_max
      real     z_min,z_max
      integer  i0,i1,i2,i3,i4,i5,i6,i7

      integer  nd_min,nd_max
      real     da_min,da_max

      integer  i_ray_1,i_ray_2
      integer  n_inray,n_outray

      integer  i_xz_print,j_xz_print

      real     t_cpu_1,t_cpu_2

      integer  n_ray,n_tube
      real     r_ray

      real  second,time0,time1,time2,time3,time4
      real  time5,time6,time7,time8,time9
      real  time_cross
      real  t_cpu_intube, t_cpu_ver
      real  totoal_intube,totoal_ver

      integer  n_fill

      integer  n_intube,n_outtube,i_tube
      real     dr_ray
      integer  na_ray,nb_ray
      integer  lu_out
      integer  i_call
      data     i_xz_print/0/
      data     i_call/0/
      i_call = i_call + 1

      i_err = 0

      call util_seti(m_xyz_tab,points,0) 
      call util_seti(m_tube,it_flag,-1)
      call util_seti(m_ray,ir_flag,-1)
      call util_setr(m_xyz_tab,a_tab,99.0)
      call util_setr(m_xyz_tab,b_tab,99.0)
      call util_setr(m_xyz_tab,e2z_tab,0.0)
      call util_setr(m_xyz_tab,s_tab,0.0)
      call util_setr(m_xyz_tab,vmax_tab,0.0)
      call util_setr(m_xyz_tab,det33_tab,0.0)

      dt_ray = dt_macro / mt_micro
      t1_ray = (mt_ray - 1) * dt_macro + t0_ray
      t1_ray = min(5.,t1_ray)

      nt_ray = int((t1_ray-t0_ray)/dt_ray)

      r_ray  = ds_ray *  5.

      lu_out = -88  ! logical unit to writ initial ray info to

c  initalize ray information

c     time0 = second()
      call ktime_3d_raytrace_init_rays1(lu_out
     1,xs,ys,zs,vs,t0_ray
     1,ds_ray,r_ray
     1,a0_ray,a1_ray
     1,b0_ray,b1_ray
     1,na_ray,nb_ray
     1,m_ray,n_ray
     1, a_ray, b_ray,s_ray(1,1)
     1, v_ray(1,1) 
     1, x_ray(1,1), y_ray(1,1), z_ray(1,1)
     1,px_ray(1,1),py_ray(1,1),pz_ray(1,1)
     1,tx_ray(1,1),ty_ray(1,1),tz_ray(1,1)
     1,e1x_ray(1,1),e1y_ray(1,1),e1z_ray(1,1)
     1,e2x_ray(1,1),e2y_ray(1,1),e2z_ray(1,1)
     1,qq_ray(1,1,1),hq_ray(1,1,1),gs_ray(1,1)
     1,vx_ray(1,1),vy_ray(1,1),vz_ray(1,1)
     1,vq11_ray(1,1),vq12_ray(1,1),vq22_ray(1,1)
     1,vmax_ray
     1,m_tube,n_tube,ir_tube,is_tube,it_tube,it_flag
     1,nz_vel,z0_vel,dz_vel
     1,nx_vel,x0_vel,dx_vel
     1,ny_vel,y0_vel,dy_vel
     1,vel
     1,m_xyz_tab,amp_tab
     1,m_work,work
     1,i_err)
      if (i_err .ne. 0) goto 999
c     time1 = second()

      if (n_ray .eq. 0 .or. n_tube .eq. 0 ) stop

      call util_seti(n_tube,it_flag,1)
      call util_seti(n_ray,ir_flag,1)
      call util_lini(n_ray, ir_pointer,1,1) ! ir_pointer=1,2,3....
      call util_lini(n_tube,it_pointer,1,1) ! it_pointer=1,2,3....
      call util_seti(m_ray*2,phase_ray,0)
      call util_setr(m_ray*2,tc_ray,1.0)

c  initialize the inside boundary tubes and rays

        n_intube = n_tube
        n_outtube =0

        n_inray = n_ray

      call ktime_3d_raytrace_crosspro_2(
     1 n_tube, n_intube, it_pointer, it_flag, ir_tube
     1,x_ray(1,1), y_ray(1,1), z_ray(1,1)
     1,norm_x(1,1), norm_y(1,1), norm_z(1,1))
 
c  set the table space limits
      x_min =  x0_tab(ny_tab/2+1)     ! min x location
      x_max = x_min + (nx_tab(ny_tab/2+1)-1) * dx_tab

      y_min = y0_tab                 ! min y location
      y_max = y_min + (ny_tab - 1) * dy_tab      ! max y location

      z_min = z0_tab(nx_tab(ny_tab/2+1)*ny_tab/2+1)      ! min z location
      z_max = z_min + (nz_tab(nx_tab(ny_tab/2+1)*ny_tab/2+1)-1) * dz_tab ! max z location

c set the table a little larger 
      x_min = x_min - dx_tab
      x_max = x_max + dx_tab

      y_min = y_min - dy_tab      
      y_max = y_max + dy_tab

      z_min = z_min - dz_tab 
      z_max = z_max + dz_tab

c the table oringe is relative to the source!!!
      x_min = x_min + xs
      x_max = x_max + xs
 
      y_min = y_min + ys
      y_max = y_max + ys
 
      z_min = z_min + zs
      z_max = z_max + zs
          

c  for each macro time step ...
c  1 trace rays from current location out mt_micro micro time steps
c  2 determine the ray spreading centers for each tube
c  3 interpolate from ray positions to travel time table
c  4 update index of tubes and rays that inside the target region
c  5 interpolate rays between pairs that have spread apart too much
c  6 check if all rays have left the travel time table area, if so quit

c     print*,'nt_ray=',nt_ray

      n_fill = 0
c     time2 = second()
      time3 = 0.0
      time4 = 0.0
      time5 = 0.0
      time6 = 0.0
      totoal_intube = 0.0
      totoal_ver = 0.0
      time_cross = 0.0
      j=0
      k=0
      ds_max = 90.0
      da_min = 1e5
      da_max = 0
      nd_min = 1e5
      nd_max = 0
      i_ray_2 = 1
      na_add = 0
      nt_num_1=0
c  cycle over macro time steps
c  within each macro step there will be mt_micro steps dt_ray long
      do it_ray = 1 , mt_ray

c  there are two memory shells 
c  one for the current and one for the next macro time step
c  swap the memory shell pointers

        i_ray_1 = i_ray_2            ! current time step pointer
        i_ray_2 = mod(i_ray_2,2) + 1 ! next    time step pointer

c  the current and next time value
        t_ray_1 = (it_ray - 1) * (mt_micro * dt_ray) + t0_ray
        t_ray_2 = (it_ray - 0) * (mt_micro * dt_ray) + t0_ray

c  1 trace rays from current location out mt_micro micro time steps
c  the current ray is    in i_ray_1
c  the next step will be in i_ray_2
c  flag 0 for kinemic raytracing, flag 1 for dynamic raytracing

c       t_cpu_1 = second()
        call ktime_3d_raytrace_shoot_n_step1(
     1 xs,ys,zs
     1,mt_micro,nt_micro,dt_ray
     1,n_ray,ir_flag,n_inray,ir_pointer
     1, a_ray,b_ray
     1, x_ray(1,i_ray_1), y_ray(1,i_ray_1), z_ray(1,i_ray_1)
     1, s_ray(1,i_ray_1)
     1,px_ray(1,i_ray_1),py_ray(1,i_ray_1),pz_ray(1,i_ray_1)
     1,tx_ray(1,i_ray_1),ty_ray(1,i_ray_1),tz_ray(1,i_ray_1)
     1,e1x_ray(1,i_ray_1),e1y_ray(1,i_ray_1),e1z_ray(1,i_ray_1)
     1,e2x_ray(1,i_ray_1),e2y_ray(1,i_ray_1),e2z_ray(1,i_ray_1)
     1,tc_ray(1,i_ray_1),qq_ray(1,1,i_ray_1),hq_ray(1,1,i_ray_1)
     1,gs_ray(1,i_ray_1),phase_ray(1,i_ray_1)
     1,  v_ray(1,i_ray_1),v_ray_tmp
     1,vq11_ray(1,i_ray_1)
     1,vq12_ray(1,i_ray_1),vq22_ray(1,i_ray_1)
     1,vx_ray(1,i_ray_1),vy_ray(1,i_ray_1)
     1,vz_ray(1,i_ray_1),vmax_ray
     1, x_ray(1,i_ray_2), y_ray(1,i_ray_2), z_ray(1,i_ray_2)
     1, s_ray(1,i_ray_2)
     1,px_ray(1,i_ray_2),py_ray(1,i_ray_2),pz_ray(1,i_ray_2)
     1,tx_ray(1,i_ray_2),ty_ray(1,i_ray_2),tz_ray(1,i_ray_2)
     1,e1x_ray(1,i_ray_2),e1y_ray(1,i_ray_2),e1z_ray(1,i_ray_2)
     1,e2x_ray(1,i_ray_2),e2y_ray(1,i_ray_2),e2z_ray(1,i_ray_2)
     1,tc_ray(1,i_ray_2),qq_ray(1,1,i_ray_2),hq_ray(1,1,i_ray_2)
     1,gs_ray(1,i_ray_2),phase_ray(1,i_ray_2)
     1, v_ray(1,i_ray_2)
     1,vq11_ray(1,i_ray_2)
     1,vq12_ray(1,i_ray_2),vq22_ray(1,i_ray_2)
     1,vx_ray(1,i_ray_2),vy_ray(1,i_ray_2)
     1,vz_ray(1,i_ray_2)
     1,nx_vel,x0_vel,dx_vel
     1,ny_vel,y0_vel,dy_vel
     1,nz_vel,z0_vel,dz_vel
     1,vel
     1,m_work,work
     1,i_err,t_ray_1
     1)
      if (i_err .ne. 0) goto 999
c     t_cpu_2 = second()
c     time3 = time3 + t_cpu_2 - t_cpu_1

c  2 determine the ray spreading centers for each tube

c      t_cpu_1 = second()
       call ktime_3d_raytrace_crosspro_2(
     1 n_tube, n_intube, it_pointer, it_flag, ir_tube
     1,x_ray(1,i_ray_1), y_ray(1,i_ray_1), z_ray(1,i_ray_1)
     1,norm_x(1,i_ray_1), norm_y(1,i_ray_1), norm_z(1,i_ray_1))
 
       call ktime_3d_raytrace_crosspro_2(
     1 n_tube, n_intube, it_pointer, it_flag, ir_tube
     1,x_ray(1,i_ray_2), y_ray(1,i_ray_2), z_ray(1,i_ray_2)
     1,norm_x(1,i_ray_2), norm_y(1,i_ray_2), norm_z(1,i_ray_2))
c      t_cpu_2 = second()
c      time_cross = time_cross + t_cpu_2 - t_cpu_1

c  3 interpolate from ray positions to travel time table
c      t_cpu_1 = second()
       call ktime_3d_receiver_4(it_ray,xs,ys,zs,vs,inter_type
     1,m_xyz_tab,points
     1,t_ray_1,t_ray_2,n_ray,a_ray,b_ray,n_tube
     1,x_ray(1,i_ray_1),y_ray(1,i_ray_1),z_ray(1,i_ray_1)
     1,s_ray(1,i_ray_1)
     1,e1x_ray(1,i_ray_1),e1y_ray(1,i_ray_1),e1z_ray(1,i_ray_1)
     1,e2x_ray(1,i_ray_1),e2y_ray(1,i_ray_1),e2z_ray(1,i_ray_1)
     1,px_ray(1,i_ray_1),py_ray(1,i_ray_1),pz_ray(1,i_ray_1)
     1,tc_ray(1,i_ray_1),qq_ray(1,1,i_ray_1)
     1,gs_ray(1,i_ray_1),phase_ray(1,i_ray_1)
     1,v_ray(1,i_ray_1)
     1,norm_x(1,i_ray_1),norm_y(1,i_ray_1),norm_z(1,i_ray_1)
     1,x_ray(1,i_ray_2),y_ray(1,i_ray_2),z_ray(1,i_ray_2)
     1,s_ray(1,i_ray_2)
     1,e1x_ray(1,i_ray_2),e1y_ray(1,i_ray_2),e1z_ray(1,i_ray_2)
     1,e2x_ray(1,i_ray_2),e2y_ray(1,i_ray_2),e2z_ray(1,i_ray_2)
     1,px_ray(1,i_ray_2),py_ray(1,i_ray_2),pz_ray(1,i_ray_2)
     1,tc_ray(1,i_ray_2),qq_ray(1,1,i_ray_2)
     1,gs_ray(1,i_ray_2),phase_ray(1,i_ray_2)
     1,v_ray(1,i_ray_2)
     1,norm_x(1,i_ray_2),norm_y(1,i_ray_2),norm_z(1,i_ray_2)
     1,vmax_ray,vmax_tab,det33_tab
     1,ir_tube,is_tube,it_tube
     1,it_flag,n_intube,it_pointer
     1,ix_tab,nx_tab,x0_tab,dx_tab
     1       ,ny_tab,y0_tab,dy_tab
     1,iz_tab,nz_tab,z0_tab,dz_tab
     1,t_tab,amp_tab,phase_tab
     1,q11_tab,q12_tab,q21_tab,q22_tab,p3_tab
     1,s_tab,a_tab,b_tab,e2z_tab
     1,nt_num
     1,m_work,work,i_err,t_cpu_intube,t_cpu_ver
     1)
         if (i_err .ne. 0) goto 999
c     t_cpu_2 = second()
c     time4 = time4 + t_cpu_2 - t_cpu_1
      totoal_intube = totoal_intube + t_cpu_intube
      totoal_ver = totoal_ver + t_cpu_ver
      n_fill = n_fill + nt_num

c  4 update index of tubes and rays that inside the target region
c     t_cpu_1 = second()
          call ktime_3d_check_boundary_4(
     1 it_ray,xs,ys,zs
     1,x_min,x_max,y_min,y_max,z_min,z_max
     1,m_ray,m_tube,n_ray,n_tube
     1, a_ray, b_ray
     1,x_ray(1,i_ray_2),y_ray(1,i_ray_2),z_ray(1,i_ray_2)
     1,s_ray(1,i_ray_2)
     1,px_ray(1,i_ray_2),py_ray(1,i_ray_2),pz_ray(1,i_ray_2)
     1,tx_ray(1,i_ray_2),ty_ray(1,i_ray_2),tz_ray(1,i_ray_2)
     1,e1x_ray(1,i_ray_2),e1y_ray(1,i_ray_2),e1z_ray(1,i_ray_2)
     1,e2x_ray(1,i_ray_2),e2y_ray(1,i_ray_2),e2z_ray(1,i_ray_2)
     1,tc_ray(1,i_ray_2),qq_ray(1,1,i_ray_2),hq_ray(1,1,i_ray_2)
     1,gs_ray(1,i_ray_2),phase_ray(1,i_ray_2)
     1,v_ray(1,i_ray_2)
     1,vq11_ray(1,i_ray_2),vq12_ray(1,i_ray_2),vq22_ray(1,i_ray_2)
     1,vx_ray(1,i_ray_2),vy_ray(1,i_ray_2),vz_ray(1,i_ray_2)
     1,vmax_ray
     1,n_inray,n_outray,ir_flag,ir_pointer
     1,ir_tube,it_tube,is_tube,n_intube,n_outtube
     1,it_flag,it_pointer
     1,maxangle
     1,i_err)
c     t_cpu_2 = second()
c     time6 = time6 + t_cpu_2 - t_cpu_1

c  5 interpolate rays between pairs that have spread apart too much

c     t_cpu_1 = second()
       if (num_add .ne. 0 .and. it_ray .gt. 11 .and. 
     1       mod(it_ray,num_add) .eq. 0 ) 
     1 call ktime_3d_raytrace_add_rays_2(
     1 lu_out,it_ray,dr_ray,xs,ys,zs
     1,i_ray_1,i_ray_2,mt_micro,dt_ray,t0_ray
     1,m_ray,n_ray,n_inray,a_ray,b_ray
     1,x_ray,y_ray,z_ray,s_ray
     1,px_ray,py_ray,pz_ray
     1,tx_ray,ty_ray,tz_ray
     1,e1x_ray,e1y_ray,e1z_ray
     1,e2x_ray,e2y_ray,e2z_ray
     1,m_tube,n_tube,ir_tube,is_tube,it_tube
     1,it_flag
     1,n_intube,it_pointer
     1,ia_rays,tc_ray
     1,qq_ray,hq_ray,gs_ray,phase_ray
     1,ir_flag
     1,v_ray,vq11_ray,vq12_ray,vq22_ray
     1,vx_ray,vy_ray,vz_ray
     1,vmax_ray,v_ray_tmp
     1,nx_vel,x0_vel,dx_vel
     1,ny_vel,y0_vel,dy_vel
     1,nz_vel,z0_vel,dz_vel
     1,vel
     1,maxangle
     1,m_work,work
     1,i_err
     1)
       if (i_err .ne. 0) goto 999     
c     t_cpu_2 = second()
c     time5 = time5 + t_cpu_2 - t_cpu_1

      if (i_call .eq. 1)
     1print'('' it='',i3,'' t='',f8.5
     1,''n_inray='',i5,''n_outray='',i5
     1,''n_intube='',i5
     1,'' n_outtube='',i5
     1)'
     1,it_ray,t_ray_2
     1,n_inray,n_outray
     1,n_intube,n_outtube

c  6 check if all rays have left the travel time table area, if so quit
c  quit if all the rays are outside the table space 
c  this is good for 3d

        if (n_intube .le.0 .or. n_inray  .le. 0
     1 .or. n_outray  .ge. n_ray
     1 .or. t_ray_2 .ge. t1_ray
     1)  goto 1

      enddo    ! do it_ray = 1 , nt_ray
     
c  come to here if all rays have left the travel time table area
    1 continue
c     time7 = second()

       nullvalue = 0.0
       call ktime_3d_fill_points(
     1 nullvalue,m_xyz_tab
     1,ix_tab,nx_tab,x0_tab,dx_tab
     1       ,ny_tab,y0_tab,dy_tab
     1,iz_tab,nz_tab,z0_tab,dz_tab
     1,t_tab,amp_tab,phase_tab,a_tab,b_tab
     1,q11_tab,q12_tab,q21_tab,q22_tab,p3_tab
     1,e2z_tab
     1,i_err
     1)
c     time8 = second()
c  some printout information
c     time9 = time8 - time0

c     print '(''init     time='',f12.1,f8.2)'
c    1,time1-time0,(time1-time0)*100.0/time9
c     print '(''nstep    time='',f12.1,f8.2)',time3,time3*100/time9
c     print '(''cross_2  time='',f12.1,f8.2)'
c    1,time_cross,time_cross*100/time9
c     print '(''ver_3    time='',f12.1,f8.2)',time4,time4*100/time9
c     print '(''intube   time='',f12.1,f8.2)'
c    1,totoal_intube,totoal_intube*100/time9
c     print '(''addray   time='',f12.1,f8.2)',time5,time5*100/time9
c     print '(''boundary time='',f12.1,f8.2)',time6,time6*100/time9
c     print '(''process  time='',f12.1,f8.2)',time7-time2
c    1,(time7-time2)*100.0/time9
c     print '(''fill     time='',f12.1,f8.2)'
c    1,time8-time7,(time8-time7)*100.0/time9

      i0=0
      i1=0
      i2=0
      i3=0
      i4=0
      i5=0
      i6=0
      i7=0
 
      do i=1,m_xyz_tab
 
         if ( points(i) .eq. 0) then
           i0= i0+1
         elseif ( points(i) .eq. 1) then
           i1 =i1+1
         elseif (points(i) .eq. 2)  then
           i2= i2+1
         elseif (points(i) .eq. 3) then
           i3=i3+1
         elseif (points(i) .eq. 4) then
           i4=i4+1
         elseif (points(i) .eq. 5) then
           i5=i5+1
         elseif (points(i) .eq. 6) then
           i6=i6+1
         else
           i7=i7+1
         endif
 
      enddo
 
c     print'(/,'' m_xyz='',i8,''n_fill='',i8
c    1,/,'' i0='',i8,'' i1='',i8
c    1,/,'' i2='',i8,'' i3='',i8,'' i4='',i8
c    1,/,'' i5='',i8,'' i6='',i8,'' i7='',i8)'
c    1,m_xyz_tab,n_fill,i0,i1,i2,i3,i4,i5,i6,i7

      n_xyz_tab = iz_tab(ix_tab(ny_tab)+nx_tab(ny_tab))
     1          + nz_tab(ix_tab(ny_tab)+nx_tab(ny_tab))

      call write_tables(outt_file,outamp_file
     1,outpha_file,outq11_file,outq12_file
     1,outq21_file,outq22_file
     1,outa0_file,outb0_file,outa1_file,outb1_file
     1,oute1x_file,oute1y_file,oute1z_file
     1,oute2x_file,oute2y_file,oute2z_file
     1,ix_tab,nx_tab,x0_tab,dx_tab
     1       ,ny_tab,y0_tab,dy_tab
     1,iz_tab,nz_tab,z0_tab,dz_tab
     1,n_xyz_tab
     1,t_tab,amp_tab,phase_tab
     1,q11_tab,q12_tab,q21_tab,q22_tab
     1,a_tab,b_tab,a_tab,b_tab
     1,q11_tab,q12_tab,q21_tab
     1,q22_tab,det33_tab,e2z_tab
     1,i_err
     1)

      return

  999 continue
      print'('' error in ktime_3d_raytrace_0'')'
      i_err = -1
      return

      entry ptime_3d_raytrace_xz_print(j_xz_print)

      i_xz_print = max(0,min(1,j_xz_print))

      return

      end
c23456789012345678901234567890123456789012345678901234567890123456789012
      subroutine ktime_3d_raytrace_shoot_n_step1(
     1 xs,ys,zs
     1,mt_ray,nt_ray,dt_ray
     1,n_ray,ir_flag,n_inray,ir_pointer
     1,a_ray,b_ray
     1, x_ray_1, y_ray_1, z_ray_1
     1, s_ray_1
     1,px_ray_1,py_ray_1,pz_ray_1
     1,tx_ray_1,ty_ray_1,tz_ray_1
     1,e1x_ray_1,e1y_ray_1,e1z_ray_1
     1,e2x_ray_1,e2y_ray_1,e2z_ray_1
     1,tc_ray_1,qq_ray_1,hq_ray_1,gs_ray_1,phase_ray_1
     1,  v_ray_1,v_ray_tmp,vq11_ray_1
     1,vq12_ray_1,vq22_ray_1
     1,vx_ray_1,vy_ray_1,vz_ray_1,vmax_ray
     1, x_ray_2, y_ray_2, z_ray_2
     1, s_ray_2
     1,px_ray_2,py_ray_2,pz_ray_2
     1,tx_ray_2,ty_ray_2,tz_ray_2
     1,e1x_ray_2,e1y_ray_2,e1z_ray_2
     1,e2x_ray_2,e2y_ray_2,e2z_ray_2
     1,tc_ray_2,qq_ray_2,hq_ray_2,gs_ray_2,phase_ray_2
     1,  v_ray_2,vq11_ray_2
     1,vq12_ray_2,vq22_ray_2
     1,vx_ray_2,vy_ray_2,vz_ray_2
     1,nx_vel,x0_vel,dx_vel
     1,ny_vel,y0_vel,dy_vel
     1,nz_vel,z0_vel,dz_vel
     1,vcof
     1,m_work,work
     1,i_err,t_ray_1
     1)
cccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccc
c  trace rays a max of mt_ray time steps through a gridded velocity model
c  start at x_ray_1,y_ray_1,z_ray_1
c  end   at x_ray_2,y_ray_2,z_ray_2
c  may be inplace
c
c  input :
c        gs_flag 
c        mt_ray,nt_ray,dt_ray
c        n_ray,n_inray,in_rays
c        x_ray_1, y_ray_1, z_ray_1
c        px_ray_1,py_ray_1,pz_ray_1,tc_ray_1
c        v_ray_1,vq11_ray_1,vq12_ray_1,vq22_ray_1
c        vx_ray_1,vy_ray_1,vz_ray_1
c        nx_vel,x0_vel,dx_vel
c        ny_vel,y0_vel,dy_vel
c        nz_vel,z0_vel,dz_vel
c        vcof
c
c  output :
c        x_ray_2, y_ray_2, z_ray_2
c        px_ray_2,py_ray_2,pz_ray_2,tc_ray_2
c        v_ray_2,vq11_ray_2,vq12_ray_2,vq22_ray_2
c        vx_ray_2,vy_ray_2,vz_ray_2
cccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccc
      implicit  none

      real      xs,ys,zs
 
      integer   mt_ray,nt_ray
      real      dt_ray
 
      integer   n_ray,n_inray,ir_flag(n_ray)
      integer  ir_pointer(n_inray)
 
      real       a_ray(n_ray), b_ray(n_ray)
      real       x_ray_1(n_ray), y_ray_1(n_ray), z_ray_1(n_ray)
      real      px_ray_1(n_ray),py_ray_1(n_ray),pz_ray_1(n_ray)
      real      tx_ray_1(n_ray),ty_ray_1(n_ray),tz_ray_1(n_ray)
      real      e1x_ray_1(n_ray),e1y_ray_1(n_ray),e1z_ray_1(n_ray)
      real      e2x_ray_1(n_ray),e2y_ray_1(n_ray),e2z_ray_1(n_ray)
      real       s_ray_1(n_ray)
      real      v_ray_1(n_ray),v_ray_tmp(n_ray),vq11_ray_1(n_ray)
      real      vq12_ray_1(n_ray),vq22_ray_1(n_ray)
      real      tc_ray_1(n_ray),qq_ray_1(4,n_ray),hq_ray_1(4,n_ray)
      real      gs_ray_1(n_ray)
      integer   phase_ray_1(n_ray)
      real      vx_ray_1(n_ray),vy_ray_1(n_ray),vz_ray_1(n_ray)
      real      vmax_ray(n_ray)
 
      real       x_ray_2(n_ray), y_ray_2(n_ray), z_ray_2(n_ray)
      real      px_ray_2(n_ray),py_ray_2(n_ray),pz_ray_2(n_ray)
      real      tx_ray_2(n_ray),ty_ray_2(n_ray),tz_ray_2(n_ray)
      real      e1x_ray_2(n_ray),e1y_ray_2(n_ray),e1z_ray_2(n_ray)
      real      e2x_ray_2(n_ray),e2y_ray_2(n_ray),e2z_ray_2(n_ray)
      real       s_ray_2(n_ray)
      real      v_ray_2(n_ray),vq11_ray_2(n_ray)
      real      vq12_ray_2(n_ray),vq22_ray_2(n_ray)
      real      tc_ray_2(n_ray),qq_ray_2(4,n_ray),hq_ray_2(4,n_ray)
      real      gs_ray_2(n_ray)
      integer   phase_ray_2(n_ray)
      real      vx_ray_2(n_ray),vy_ray_2(n_ray),vz_ray_2(n_ray)
 
      integer   nx_vel
      real      x0_vel,dx_vel
 
      integer   ny_vel
      real      y0_vel,dy_vel
 
      integer   nz_vel
      real      z0_vel,dz_vel

      real      vcof(nz_vel,nx_vel,ny_vel,10)
 
      integer   m_work
      real      work(m_work)
 
      integer   i_err
      real      t_ray_1,t_const
 
      integer   it_ray
 
      integer  i_call

      integer  i_ray, i_inray

      real     t_a,amp_a, amp_c, r1,r2, a1,a2,trans,qq
      real     a2_ct,a2_cp,q11,q12,q21,q22,hq11,hq22
      real     e1x,e1y,e1z,e2x,e2y,e2z,tx,ty,tz
     
      real      tc_cos1,tc_cos2,tmp,vx,vy,vz
      real      tmp1,tmp2,tmp3,tmp4
      real      px_t,py_t,pz_t,a1_t,b1_t
      data     i_call/0/

      i_call = i_call + 1
 
      i_err = 0
      hq22=1./2000.

c  trace rays mt_ray steps of length dt_ray
c  the rays start in arrays x_ray_1. etc.
c  and end in arrays x_ray_2, etc.
c  these may be the same arrays

      do it_ray = 1 , mt_ray

        t_const = t_ray_1 + it_ray*dt_ray
c  at the first step go from x_ray_1 to x_ray_2
        if (it_ray .eq. 1) then
 
          call ktime_3d_raytrace_shoot_1_step1(
     1 xs,ys,zs
     1,dt_ray
     1,n_ray,ir_flag,n_inray,ir_pointer
     1, x_ray_1, y_ray_1, z_ray_1
     1, s_ray_1, a_ray
     1,px_ray_1,py_ray_1,pz_ray_1
     1,tx_ray_1,ty_ray_1,tz_ray_1
     1,e1x_ray_1,e1y_ray_1,e1z_ray_1
     1,e2x_ray_1,e2y_ray_1,e2z_ray_1
     1,tc_ray_1,qq_ray_1,hq_ray_1,gs_ray_1,phase_ray_1
     1,  v_ray_1,v_ray_tmp,vq11_ray_1
     1,vq12_ray_1,vq22_ray_1
     1,vx_ray_1,vy_ray_1,vz_ray_1
     1, x_ray_2, y_ray_2, z_ray_2
     1, s_ray_2
     1,px_ray_2,py_ray_2,pz_ray_2
     1,tx_ray_2,ty_ray_2,tz_ray_2
     1,e1x_ray_2,e1y_ray_2,e1z_ray_2
     1,e2x_ray_2,e2y_ray_2,e2z_ray_2
     1,tc_ray_2,qq_ray_2,hq_ray_2,gs_ray_2,phase_ray_2
     1,  v_ray_2,vq11_ray_2
     1,vq12_ray_2,vq22_ray_2
     1,vx_ray_2,vy_ray_2,vz_ray_2
     1,vmax_ray
     1,nx_vel,x0_vel,dx_vel
     1,ny_vel,y0_vel,dy_vel
     1,nz_vel,z0_vel,dz_vel
     1,vcof
     1,m_work,work
     1,i_err,t_const
     1)
 
c  at the first step go from x_ray_2 to x_ray_2
        else    ! if (it_ray .eq. 1) then
 
          call ktime_3d_raytrace_shoot_1_step1(
     1 xs,ys,zs
     1,dt_ray
     1,n_ray,ir_flag,n_inray,ir_pointer
     1, x_ray_2, y_ray_2, z_ray_2
     1, s_ray_2, a_ray
     1,px_ray_2,py_ray_2,pz_ray_2
     1,tx_ray_2,ty_ray_2,tz_ray_2
     1,e1x_ray_2,e1y_ray_2,e1z_ray_2
     1,e2x_ray_2,e2y_ray_2,e2z_ray_2
     1,tc_ray_2,qq_ray_2,hq_ray_2,gs_ray_2,phase_ray_2
     1,  v_ray_2,v_ray_tmp,vq11_ray_2
     1,vq12_ray_2,vq22_ray_2
     1,vx_ray_2,vy_ray_2,vz_ray_2
     1, x_ray_2, y_ray_2, z_ray_2
     1, s_ray_2
     1,px_ray_2,py_ray_2,pz_ray_2
     1,tx_ray_2,ty_ray_2,tz_ray_2
     1,e1x_ray_2,e1y_ray_2,e1z_ray_2
     1,e2x_ray_2,e2y_ray_2,e2z_ray_2
     1,tc_ray_2,qq_ray_2,hq_ray_2,gs_ray_2,phase_ray_2
     1,  v_ray_2,vq11_ray_2
     1,vq12_ray_2,vq22_ray_2
     1,vx_ray_2,vy_ray_2,vz_ray_2
     1,vmax_ray
     1,nx_vel,x0_vel,dx_vel
     1,ny_vel,y0_vel,dy_vel
     1,nz_vel,z0_vel,dz_vel
     1,vcof
     1,m_work,work
     1,i_err,t_const
     1)
 
        endif    ! if (it_ray .eq. 1) then

c check consistancy btw tz_ray_2 and pz_ray_2

        if (i_err .ne. 0) goto 999
 
        nt_ray = it_ray
 
      enddo    ! do it_ray = 1 , mt_ray

c compute tc_ray_2

      do i_ray = 1, n_inray

        vx = (vx_ray_1(i_ray)+vx_ray_2(i_ray))*0.5   
        vy = (vy_ray_1(i_ray)+vy_ray_2(i_ray))*0.5   
        vz = (vz_ray_1(i_ray)+vz_ray_2(i_ray))*0.5   

        tc_cos1 = px_ray_1(i_ray)*vx + py_ray_1(i_ray)*vy
     1          + pz_ray_1(i_ray)*vz

        tc_cos2 = px_ray_2(i_ray)*vx + py_ray_2(i_ray)*vy
     1          + pz_ray_2(i_ray)*vz

        tmp = v_ray_2(i_ray)*tc_cos1 + v_ray_1(i_ray)*tc_cos2

        if (abs(tmp) .le. 0.001) then

            tc_ray_2(i_ray) = tc_ray_1(i_ray)

        else

            tc_ray_2(i_ray) = tc_ray_1(i_ray) * 2.0 
     1      *v_ray_2(i_ray)*tc_cos1/tmp

        endif !if (abs(tmp) .le. eps) then

      enddo !  do i_ray = 1, n_inray

      return
 
  999 continue
      print'('' error in ktime_3d_raytrace_shoot_n_step'')'
      i_err = -1
      return
 
      end
 
c23456789012345678901234567890123456789012345678901234567890123456789012
      subroutine ktime_3d_raytrace_shoot_1_step1(
     1 xs,ys,zs
     1,dt_ray
     1,n_ray,ir_flag,n_inray,ir_pointer
     1, x_ray_1, y_ray_1, z_ray_1
     1, s_ray_1, a_ray
     1,px_ray_1,py_ray_1,pz_ray_1
     1,tx_ray_1,ty_ray_1,tz_ray_1
     1,e1x_ray_1,e1y_ray_1,e1z_ray_1
     1,e2x_ray_1,e2y_ray_1,e2z_ray_1
     1,tc_ray_1,qq_ray_1,hq_ray_1,gs_ray_1,phase_ray_1
     1,  v_ray_1,v_ray_tmp,vq11_ray_1
     1,vq12_ray_1,vq22_ray_1
     1,vx_ray_1,vy_ray_1,vz_ray_1
     1, x_ray_2, y_ray_2, z_ray_2
     1, s_ray_2
     1,px_ray_2,py_ray_2,pz_ray_2
     1,tx_ray_2,ty_ray_2,tz_ray_2
     1,e1x_ray_2,e1y_ray_2,e1z_ray_2
     1,e2x_ray_2,e2y_ray_2,e2z_ray_2
     1,tc_ray_2,qq_ray_2,hq_ray_2,gs_ray_2,phase_ray_2
     1,  v_ray_2,vq11_ray_2
     1,vq12_ray_2,vq22_ray_2
     1,vx_ray_2,vy_ray_2,vz_ray_2
     1,vmax_ray
     1,nx_vel,x0_vel,dx_vel
     1,ny_vel,y0_vel,dy_vel
     1,nz_vel,z0_vel,dz_vel
     1,vcof
     1,m_work,work
     1,i_err,t_const
     1)
ccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccc
c  trace rays a SINGLE time step through a gridded velocity model
c  start at x_ray_1,y_ray_1,z_ray_1
c  end   at x_ray_2,y_ray_2,z_ray_2
c  may be inplace
c
c  input :
c        gs_flag
c        mt_ray,nt_ray,dt_ray
c        n_ray,n_inray,in_rays
c        x_ray_1, y_ray_1, z_ray_1
c        px_ray_1,py_ray_1,pz_ray_1
c        v_ray_1,vq11_ray_1,vq12_ray_1,vq22_ray_1
c        vx_ray_1,vy_ray_1,vz_ray_1
c        nx_vel,x0_vel,dx_vel
c        ny_vel,y0_vel,dy_vel
c        nz_vel,z0_vel,dz_vel
c        vcof
c
c  output :
c        x_ray_2, y_ray_2, z_ray_2
c        px_ray_2,py_ray_2,pz_ray_2
c        v_ray_2,vq11_ray_2,vq12_ray_2,vq22_ray_2
c        vx_ray_2,vy_ray_2,vz_ray_2
cccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccc
      implicit  none

      real      xs,ys,zs
 
      real      dt_ray,t_const
 
      integer   n_ray,n_inray,ir_flag(n_ray),i_inray
      integer   ir_pointer(n_inray)
 
      real       x_ray_1(n_ray), y_ray_1(n_ray), z_ray_1(n_ray)
      real      px_ray_1(n_ray),py_ray_1(n_ray),pz_ray_1(n_ray)
      real      tx_ray_1(n_ray),ty_ray_1(n_ray),tz_ray_1(n_ray)
      real      e1x_ray_1(n_ray),e1y_ray_1(n_ray),e1z_ray_1(n_ray)
      real      e2x_ray_1(n_ray),e2y_ray_1(n_ray),e2z_ray_1(n_ray)
      real       s_ray_1(n_ray), a_ray(n_ray), ca
      real       v_ray_1(n_ray),v_ray_tmp(n_ray),vq11_ray_1(n_ray)
      real       vq12_ray_1(n_ray),vq22_ray_1(n_ray)
      real      tc_ray_1(n_ray),qq_ray_1(4,n_ray),hq_ray_1(4,n_ray)
      real      gs_ray_1(n_ray)
      integer   phase_ray_1(n_ray)
      real      vx_ray_1(n_ray),vy_ray_1(n_ray),vz_ray_1(n_ray)
 
      real       x_ray_2(n_ray), y_ray_2(n_ray), z_ray_2(n_ray)
      real      px_ray_2(n_ray),py_ray_2(n_ray),pz_ray_2(n_ray)
      real      tx_ray_2(n_ray),ty_ray_2(n_ray),tz_ray_2(n_ray)
      real      e1x_ray_2(n_ray),e1y_ray_2(n_ray),e1z_ray_2(n_ray)
      real      e2x_ray_2(n_ray),e2y_ray_2(n_ray),e2z_ray_2(n_ray)
      real       s_ray_2(n_ray)
      real       v_ray_2(n_ray),vq11_ray_2(n_ray)
      real       vq12_ray_2(n_ray),vq22_ray_2(n_ray)
      real      tc_ray_2(n_ray),qq_ray_2(4,n_ray),hq_ray_2(4,n_ray)
      real      gs_ray_2(n_ray)
      integer   phase_ray_2(n_ray)
      real      vx_ray_2(n_ray),vy_ray_2(n_ray),vz_ray_2(n_ray)
      real      vmax_ray(n_ray)

      real      vv_ray 
      integer   nx_vel
      real      x0_vel,dx_vel
 
      integer   ny_vel
      real      y0_vel,dy_vel
 
      integer   nz_vel
      real      z0_vel,dz_vel

      real      vcof(nz_vel,nx_vel,ny_vel,10)

      real      gs_ray_tmp
 
      integer   m_work
      real      work(m_work)
 
      integer   i_err
 
      integer   i_ray
      real      pxyz
      real      dx,dy,dz,r
      real      tc_cos1,tc_cos2,tmp
      real      eps

      real      dce1,dce2,dct,dtx,dty,dtz,txyz
      real      de1x,de1y,de1z, de2x,de2y,de2z
      real      tmp1,tmp2,tmp3,tmp4
      real      qray1,qray2,qray3,qray4

      integer  i_call
      data      eps/1e-6/
      data     i_call/0/
      i_call = i_call + 1
 
      i_err = 0
 
c  compute the new x,y,z coords useing the current velocity and ray parameters
c  s_ray is the ray path length
 
      do i_ray = 1 , n_inray

        vv_ray = v_ray_1(i_ray)*v_ray_1(i_ray)
 
        dx = dt_ray * px_ray_1(i_ray) * vv_ray
        x_ray_2(i_ray) = x_ray_1(i_ray) + dx
 
        dy = dt_ray * py_ray_1(i_ray) * vv_ray
        y_ray_2(i_ray) = y_ray_1(i_ray) + dy
 
        dz = dt_ray * pz_ray_1(i_ray) * vv_ray
        z_ray_2(i_ray) = z_ray_1(i_ray) + dz
 
        s_ray_2(i_ray) = s_ray_1(i_ray) + sqrt(dx**2+dy**2+dz**2)

	dce1 = vx_ray_1(i_ray)*e1x_ray_1(i_ray) 
     1       + vy_ray_1(i_ray)*e1y_ray_1(i_ray)
     1       + vz_ray_1(i_ray)*e1z_ray_1(i_ray)
        dce2 = vx_ray_1(i_ray)*e2x_ray_1(i_ray) 
     1       + vy_ray_1(i_ray)*e2y_ray_1(i_ray)
     1       + vz_ray_1(i_ray)*e2z_ray_1(i_ray)
  	dct  = vx_ray_1(i_ray)*tx_ray_1(i_ray) 
     1       + vy_ray_1(i_ray)*ty_ray_1(i_ray)
     1       + vz_ray_1(i_ray)*tz_ray_1(i_ray)

        de1x = tx_ray_1(i_ray)*dce1 
        de1y = ty_ray_1(i_ray)*dce1 
        de1z = tz_ray_1(i_ray)*dce1 

        de2x = tx_ray_1(i_ray)*dce2 
        de2y = ty_ray_1(i_ray)*dce2 
        de2z = tz_ray_1(i_ray)*dce2

        dtx = -vx_ray_1(i_ray) + tx_ray_1(i_ray)*dct
        dty = -vy_ray_1(i_ray) + ty_ray_1(i_ray)*dct
        dtz = -vz_ray_1(i_ray) + tz_ray_1(i_ray)*dct

        tx_ray_2(i_ray) = tx_ray_1(i_ray) + dtx*dt_ray
        ty_ray_2(i_ray) = ty_ray_1(i_ray) + dty*dt_ray
        tz_ray_2(i_ray) = tz_ray_1(i_ray) + dtz*dt_ray

        txyz = sqrt(tx_ray_2(i_ray)**2+ty_ray_2(i_ray)**2
     1	        +tz_ray_2(i_ray)**2)

        tx_ray_2(i_ray) = tx_ray_2(i_ray) / txyz
        ty_ray_2(i_ray) = ty_ray_2(i_ray) / txyz
        tz_ray_2(i_ray) = tz_ray_2(i_ray) / txyz

	e1x_ray_2(i_ray) = e1x_ray_1(i_ray) + de1x*dt_ray
	e1y_ray_2(i_ray) = e1y_ray_1(i_ray) + de1y*dt_ray
	e1z_ray_2(i_ray) = e1z_ray_1(i_ray) + de1z*dt_ray
        
        txyz = sqrt(e1x_ray_2(i_ray)**2+e1y_ray_2(i_ray)**2
     1	        +e1z_ray_2(i_ray)**2)

        e1x_ray_2(i_ray) = e1x_ray_2(i_ray) / txyz
        e1y_ray_2(i_ray) = e1y_ray_2(i_ray) / txyz
        e1z_ray_2(i_ray) = e1z_ray_2(i_ray) / txyz

        e2x_ray_2(i_ray) = e2x_ray_1(i_ray) + de2x*dt_ray
        e2y_ray_2(i_ray) = e2y_ray_1(i_ray) + de2y*dt_ray
        e2z_ray_2(i_ray) = e2z_ray_1(i_ray) + de2z*dt_ray

        txyz = sqrt(e2x_ray_2(i_ray)**2+e2y_ray_2(i_ray)**2
     1	        +e2z_ray_2(i_ray)**2)

        e2x_ray_2(i_ray) = e2x_ray_2(i_ray) / txyz
        e2y_ray_2(i_ray) = e2y_ray_2(i_ray) / txyz
        e2z_ray_2(i_ray) = e2z_ray_2(i_ray) / txyz

      enddo    ! do i_inray = 1 , n_inray
 
c  compute the velocity at this new x,y,z location
c  use the current px,py,pz values

      do i_ray= 1,n_inray
         v_ray_tmp(i_ray) = v_ray_1(i_ray)
      enddo

      call ktime_3d_compute_v_grad_4(
     1 n_ray,n_inray,ir_pointer
     1, z_ray_2, x_ray_2, y_ray_2
     1,e1x_ray_2,e1y_ray_2,e1z_ray_2
     1,e2x_ray_2,e2y_ray_2,e2z_ray_2
     1,nz_vel,z0_vel,dz_vel
     1,nx_vel,x0_vel,dx_vel
     1,ny_vel,y0_vel,dy_vel
     1,vcof
     1,v_ray_2,vz_ray_2,vx_ray_2,vy_ray_2,vq11_ray_2
     1,vq12_ray_2,vq22_ray_2
     1,m_work,work
     1,i_err
     1)
      if (i_err .ne. 0) goto 999

      do i_ray = 1 , n_inray
c  compute the ray parameter at this new x,z location
 
        px_ray_2(i_ray) = px_ray_1(i_ray)
     1- dt_ray * vx_ray_2(i_ray) / v_ray_2(i_ray)
 
        py_ray_2(i_ray) = py_ray_1(i_ray)
     1- dt_ray * vy_ray_2(i_ray) / v_ray_2(i_ray)
 
        pz_ray_2(i_ray) = pz_ray_1(i_ray)
     1- dt_ray * vz_ray_2(i_ray) / v_ray_2(i_ray)

        pxyz = v_ray_2(i_ray)
     1 * sqrt(px_ray_2(i_ray)**2+py_ray_2(i_ray)**2+pz_ray_2(i_ray)**2)
 
        px_ray_2(i_ray) = px_ray_2(i_ray) / pxyz
        py_ray_2(i_ray) = py_ray_2(i_ray) / pxyz
        pz_ray_2(i_ray) = pz_ray_2(i_ray) / pxyz

        vmax_ray(i_ray) = max(vmax_ray(i_ray),v_ray_2(i_ray))

      enddo    ! do i_inray = 1 , n_inray

      do i_ray = 1 , n_inray
c  compute the new spreading factor qx,qy,qz
c  normalize the ray parameters

        vv_ray = v_ray_2(i_ray) * v_ray_2(i_ray)

	qray1 = qq_ray_1(1,i_ray)
	qray2 = qq_ray_1(2,i_ray)
	qray3 = qq_ray_1(3,i_ray)
	qray4 = qq_ray_1(4,i_ray)

	qq_ray_2(1,i_ray) = qray1 + dt_ray
     1* hq_ray_1(1,i_ray) * vv_ray 
 
        qq_ray_2(2,i_ray) = qray2 + dt_ray
     1* hq_ray_1(2,i_ray) * vv_ray
 
        qq_ray_2(3,i_ray) = qray3 + dt_ray
     1* hq_ray_1(3,i_ray) * vv_ray
 
        qq_ray_2(4,i_ray) = qray4 + dt_ray
     1* hq_ray_1(4,i_ray) * vv_ray
 
        hq_ray_2(1,i_ray) = hq_ray_1(1,i_ray) - dt_ray
     1* ( vq11_ray_2(i_ray) * qray1
     1+   vq12_ray_2(i_ray) * qray2 ) /v_ray_2(i_ray)
 
        hq_ray_2(2,i_ray) = hq_ray_1(2,i_ray) - dt_ray
     1* ( vq12_ray_2(i_ray) * qray1
     1+   vq22_ray_2(i_ray) * qray2 ) /v_ray_2(i_ray)
 
        hq_ray_2(3,i_ray) = hq_ray_1(3,i_ray) - dt_ray
     1* ( vq11_ray_2(i_ray) * qray3
     1+   vq12_ray_2(i_ray) * qray4 ) /v_ray_2(i_ray)
 
        hq_ray_2(4,i_ray) = hq_ray_1(4,i_ray) - dt_ray
     1* ( vq12_ray_2(i_ray) * qray3
     1+   vq22_ray_2(i_ray) * qray4 ) /v_ray_2(i_ray)
 
c  compute the new spreading
 
        gs_ray_tmp = gs_ray_1(i_ray)
        gs_ray_2(i_ray) =  qq_ray_2(1,i_ray) * qq_ray_2(4,i_ray)
     1-                  qq_ray_2(2,i_ray) * qq_ray_2(3,i_ray) 

	if (gs_ray_tmp .ge. 0.0 .and. gs_ray_2(i_ray) .le. 0.0
     1 .or. gs_ray_tmp .le. 0.0 .and. gs_ray_2(i_ray) .ge. 0.0 )
     1   then
           phase_ray_2(i_ray) = phase_ray_1(i_ray) + 1
        else
           phase_ray_2(i_ray) = phase_ray_1(i_ray)
        endif !if (gs_ray_1(i_ray) .ge. 0.0 .and. gs_ray_2(i_ray) .le. 0.0

      enddo    ! do i_inray = 1 , n_inray

      return
 
  998 continue
      print'('' error in ktime_3d_raytrace_shoot_1_step''
     1,/,'' need more work memory need='',i8,'' and have='',i8)'
     1,n_ray*3,m_work
      goto 999
 
  999 continue
      print'('' error in ktime_3d_raytrace_shoot_1_step'')'
      i_err = -1
      return
 
      end
 
c23456789012345678901234567890123456789012345678901234567890123456789012
      subroutine ktime_3d_raytrace_init_rays1(lu_out
     1,xs,ys,zs,vs,t0_ray
     1,ds_ray,r_ray
     1,a0_ray,a1_ray
     1,b0_ray,b1_ray
     1,na_ray,nb_ray
     1,m_ray,n_ray
     1, a_ray, b_ray, s_ray
     1, v_ray
     1, x_ray, y_ray, z_ray
     1,px_ray,py_ray,pz_ray
     1,tx_ray,ty_ray,tz_ray
     1,e1x_ray,e1y_ray,e1z_ray
     1,e2x_ray,e2y_ray,e2z_ray
     1,qq_ray,hq_ray,gs_ray
     1,vx_ray,vy_ray,vz_ray
     1,vq11_ray,vq12_ray,vq22_ray
     1,vmax_ray
     1,m_tube,n_tube,ir_tube,is_tube,it_tube,it_flag
     1,nz_vel,z0_vel,dz_vel
     1,nx_vel,x0_vel,dx_vel
     1,ny_vel,y0_vel,dy_vel
     1,vel
     1,m_xyz,amp_tab
     1,m_work,work
     1,i_err)
ccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccc
c  compute an inital list of rays a,b 
c  and pointers to the associated ray tubes
c
c  Input:
c  lu_out = logical unit to write ray and ray tube information to
c             lu_out < 0 means to output is written
c  xs     = source x location
c  ys     = source y location
c  zs     = source z location
c  ds_ray = maxiomum spatial separation between rays 
c             in either the a or b directions
c  r_ray  = nominal radius at which separation is measured 
c             10. * dsray is good
c             the actual value used will be max(ds_ray,r_ray)
c  a0_ray = minimum a angle in radians
c  a1_ray = maximum a angle in radians
c  b0_ray = minimum b angle in radians
c  b1_ray = maximum b angle in radians
c  m_ray  = maximum number of rays
c  m_tube = maximum number of ray tubes
c
c  Output:
c  n_ray  = actual number of rays established
c  a_ray  = real array of a angles established dimensioned m_ray
c  b_ray  = real array of b angles established dimensioned m_ray
c  x_ray  = real array of x values established dimensioned m_ray
c  y_ray  = real array of y values established dimensioned m_ray
c  z_ray  = real array of z values established dimensioned m_ray
c  the x,y,z values are set at radius r_ray
c  the will need to be reset to xs,ys,zs when used
c
c  px_ray  = real array of px values established dimensioned m_ray
c  py_ray  = real array of py values established dimensioned m_ray
c  pz_ray  = real array of pz values established dimensioned m_ray
c  n_tube = actual number of ray tubes established
c  ir_tube = integer array of tube pointers established dimensioned 3,m_tube
c  it_tube = integer array of side pointers established dimensioned 3,m_tube
c  is_tube = integer array of side pointers established dimensioned 3,m_tube
c  i_err = error flag 0 = o.k. -1 = error
c
ccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccc
      implicit none

      integer  m_xyz
      real     amp_tab(m_xyz)

      integer  lu_out
      real     xs,ys,zs
      real     ds_ray,r_ray,t0_ray

      real     a0_ray,a1_ray
      real     b0_ray,b1_ray
      integer  na_ray,nb_ray

      integer  m_ray,n_ray,m_tube
      real      a_ray(m_ray)
      real      b_ray(m_ray)
      real      s_ray(m_ray)
      real      x_ray(m_ray)
      real      y_ray(m_ray)
      real      z_ray(m_ray)
      real     px_ray(m_ray)
      real     py_ray(m_ray)
      real     pz_ray(m_ray)
      real     tx_ray(m_ray),ty_ray(m_ray),tz_ray(m_ray)
      real     e1x_ray(m_ray),e1y_ray(m_ray),e1z_ray(m_ray)
      real     e2x_ray(m_ray),e2y_ray(m_ray),e2z_ray(m_ray)
      real      v_ray(m_ray)
      real     vx_ray(m_ray)
      real     vy_ray(m_ray)
      real     vz_ray(m_ray)
      real     vmax_ray(m_ray)
      real     qq_ray(4,m_ray)
      real     hq_ray(4,m_ray)
      real     gs_ray(m_ray)
      real     vq11_ray(m_ray)
      real     vq12_ray(m_ray)
      real     vq22_ray(m_ray)

      integer  n_tube
      integer  ir_tube(3,m_tube)
      integer  is_tube(3,m_tube)
      integer  it_tube(3,m_tube)
      integer  it_flag(m_tube)

      integer  nx_vel
      real     x0_vel,dx_vel

      integer  ny_vel
      real     y0_vel,dy_vel

      integer  nz_vel
      real     z0_vel,dz_vel

c  2d has bicubic spline coefficients in vcof 3d has velocitya
      real     vel(nz_vel,nx_vel,ny_vel,10)

      integer  m_work
      real     work(m_work)

      integer  i_err

      real     vs,vxs,vys,vzs,vq11s,vq12s,vq22s
      real     gss,qq0
      integer  i_ray

      i_err = 0

c  compute the velocity at the source location

      call ktime_3d_compute_v_grad_1(
     1 xs,ys,zs
     1,1
     1, zs,xs,ys
     1,1.,1.0,1.
     1,nz_vel,z0_vel,dz_vel
     1,nx_vel,x0_vel,dx_vel
     1,ny_vel,y0_vel,dy_vel
     1,vel
     1,vs,vzs,vxs,vys,vq11s,vq12s,vq22s
     1,m_work,work
     1,i_err
     1)
      if (i_err .ne. 0) goto 999

      call ktime_3d_raytrace_init_ab_1(lu_out
     1,xs,ys,zs
     1,ds_ray,r_ray,vs,t0_ray
     1,a0_ray,a1_ray
     1,b0_ray,b1_ray
     1,na_ray,nb_ray
     1,m_ray,n_ray,a_ray,b_ray,s_ray,x_ray,y_ray,z_ray
     1,px_ray,py_ray,pz_ray
     1,tx_ray,ty_ray,tz_ray
     1,e1x_ray,e1y_ray,e1z_ray
     1,e2x_ray,e2y_ray,e2z_ray
     1,qq_ray,hq_ray
     1,m_tube,n_tube,ir_tube,is_tube,it_tube,it_flag
     1,i_err)
      if (i_err .ne. 0) goto 999

c  initialize rays at the source location

      call util_setr(m_ray,v_ray,vs)            ! local velocity
      call util_setr(m_ray,vx_ray,vxs)          ! velocity x gradient
      call util_setr(m_ray,vy_ray,vys)          ! velocity y gradient
      call util_setr(m_ray,vz_ray,vzs)          ! velocity z gradient

      call util_setr(m_ray,vq11_ray,vq11s)      ! velocity q gradient
      call util_setr(m_ray,vq12_ray,vq12s)      ! velocity q gradient
      call util_setr(m_ray,vq22_ray,vq22s)      ! velocity q gradient

      call util_setr(m_ray,vmax_ray,vs)            ! local velocity

      call util_setr(m_xyz,amp_tab,-0.00001)    ! amplitude

c  set ray parameters
      qq0 = vs*t0_ray
      do i_ray = 1 , n_ray

        qq_ray(1,i_ray) = qq0            !dynamic raytracing 
        qq_ray(2,i_ray) = 0              !dynamic raytracing 
        qq_ray(3,i_ray) = 0              !dynamic raytracing 
        qq_ray(4,i_ray) = qq0            !dynamic raytracing 

        hq_ray(1,i_ray) = 1./vs             !dynamic raytracing 
        hq_ray(2,i_ray) = 0              !dynamic raytracing 
        hq_ray(3,i_ray) = 0              !dynamic raytracing 
        hq_ray(4,i_ray) = 1./vs             !dynamic raytracing 
        
        gs_ray(i_ray)   = qq0*qq0        !geometrical spreading

      enddo    ! do i_ray = 1 , n_ray

      return

  999 continue
      print'(/,'' error in ktime_3d_raytrace_init_rays'')'
      i_err = -1
      return

      end

c23456789012345678901234567890123456789012345678901234567890123456789012
      subroutine ktime_3d_raytrace_check_tcon(lu_out
     1,m_ray,n_ray,a_ray,b_ray,x_ray,y_ray,z_ray,px_ray,py_ray,pz_ray
     1,m_tube,n_tube,ir_tube,is_tube,it_tube,it_flag
     1,i_err)
c  check the list of ray tube connections to make sure they are consistent
c
c  i = input o = output b = both
c
c  i lu_out = logical unit to write ray and ray tube information to
c             lu_out < 0 means to output is written
c
c  i m_ray  = maximum number of rays
c  i n_ray  = actual number of rays established
c  i a_ray  = real array of a anglesi established dimensioned m_ray
c  i b_ray  = real array of b angles established dimensioned m_ray
c
c  i m_tube = maximum number of ray tubes
c  i n_tube = actual number of ray tubes established
c  i ir_tube = integer array of tube pointers established dimensioned 3,m_tube
c  o it_tube = integer array of side pointers established dimensioned 3,m_tube
c  o is_tube = integer array of side pointers established dimensioned 3,m_tube
c
c  i_err = error flag 0 = o.k. -1 = error
c
      implicit none

      integer  lu_out

      integer  m_ray,n_ray
      real     a_ray(m_ray)
      real     b_ray(m_ray)
      real     x_ray(m_ray)
      real     y_ray(m_ray)
      real     z_ray(m_ray)
      real     px_ray(m_ray)
      real     py_ray(m_ray)
      real     pz_ray(m_ray)

      integer  m_tube,n_tube
      integer  ir_tube(3,m_tube)
      integer  is_tube(3,m_tube)
      integer  it_tube(3,m_tube)
      integer  it_flag(m_tube)

      integer  i_err

      integer  i_tube_1
      integer  i_tube_2
      integer  i_tube_3,i_side_1,i_side_2,i_side_3

      i_err = 0

      print'(/,'' ktime_3d_raytrace_check_tcon''
     1,/,'' lu_out='',i8
     1,/,'' m_ray='',i8,'' n_ray='',i8
     1,/,'' m_tube='',i8,'' n_tube='',i8
     1)'
     1,lu_out
     1,m_ray,n_ray
     1,m_tube,n_tube

c  make sure the tube connections are consistent

c  for each tube ...
      do i_tube_1 = 1 , n_tube

       if(it_flag(i_tube_1) .eq. 1) then
c  check the connection of each side...
        do i_side_1 = 1 , 3

          i_side_2 = is_tube(i_side_1,i_tube_1)    ! the side opposite
          i_tube_2 = it_tube(i_side_1,i_tube_1)    ! the tube opposite

c  Some tube sides are on the  dges of the aperture and have no opposite
          if (i_tube_2 .ne. 0 .and. it_flag(i_tube_2) .eq. 1) then    
c  ! if there is a tube opposite

            i_side_3 = is_tube(i_side_2,i_tube_2)
            i_tube_3 = it_tube(i_side_2,i_tube_2)

c23456789012345678901234567890123456789012345678901234567890123456789012
            if (i_side_1 .ne. i_side_3 .or. i_tube_1 .ne. i_tube_3) then

          print'('' tube connection wrong''
     1,'' i_tube='',i6,1x,i6,1x,i6,'' i_side='',i6,1x,i6,1x,i6)'
     1,i_tube_1,i_tube_2,i_tube_3
     1,i_side_1,i_side_2,i_side_3

          write(13,'('' tube connection wrong''
     1,'' i_tube='',i6,1x,i6,1x,i6,'' i_side='',i6,1x,i6,1x,i6)')
     1 i_tube_1,i_tube_2,i_tube_3
     1,i_side_1,i_side_2,i_side_3                   

              i_err = i_err + 1

            endif    ! if (i_side_1 .ne. i_side_3 .or. i_tube_1 .ne. i_tube_3) then

          endif    ! if (i_tube_2 .ne. 0) then

        enddo    ! do i_side_1 = 1 , 3

       endif ! if(it_flag(i_tube_1) .eq. 1) then
      enddo    ! do i_tube_1 = 1 , n_tube

      if (i_err .ne. 0) goto 999

      return

  999 continue
      print'(/,'' error in ktime_3d_raytrace_check_tcon'')'
      i_err = -1
      return

      end

c23456789012345678901234567890123456789012345678901234567890123456789012
      subroutine ktime_3d_raytrace_check_tdir(lu_out
     1,m_ray,n_ray,a_ray,b_ray,x_ray,y_ray,z_ray,px_ray,py_ray,pz_ray
     1,m_tube,n_tube,ir_tube,is_tube,it_tube,it_flag
     1,i_err)
c  make sure ray tubes are in the correct direction by right hand rule
c
c  i = input o = output b = both
c
c  i lu_out = logical unit to write ray and ray tube information to
c             lu_out < 0 means to output is written
c
c  i m_ray  = maximum number of rays
c  i n_ray  = actual number of rays established
c  i a_ray  = real array of a angles dimensioned m_ray
c  i b_ray  = real array of b angles dimensioned m_ray
c  i x_ray  = real array of x values dimensioned m_ray
c  i y_ray  = real array of y values dimensioned m_ray
c  i z_ray  = real array of z values dimensioned m_ray
c
c  i m_tube = maximum number of ray tubes
c  i n_tube = actual number of ray tubes established
c  i ir_tube = integer array of tube pointers established dimensioned 3,m_tube
c  i it_tube = integer array of side pointers established dimensioned 3,m_tube
c  i is_tube = integer array of side pointers established dimensioned 3,m_tube
c
c  i_err = error flag 0 = o.k. -1 = error
c
      implicit none

      integer  lu_out

      integer  m_ray,n_ray
      real     a_ray(m_ray)
      real     b_ray(m_ray)
      real     x_ray(m_ray)
      real     y_ray(m_ray)
      real     z_ray(m_ray)
      real     px_ray(m_ray)
      real     py_ray(m_ray)
      real     pz_ray(m_ray)

      integer  m_tube,n_tube
      integer  ir_tube(3,m_tube)
      integer  is_tube(3,m_tube)
      integer  it_tube(3,m_tube)
      integer  it_flag(m_tube)

      integer  i_err

      integer  i_tube
      integer  i_ray_1,i_ray_2,i_ray_3

      real     x0,y0,z0,px0,py0,pz0,p0,p1,p2,p3
      real     x1,y1,z1,px1,py1,pz1,r1,dx1,dy1,dz1
      real     x2,y2,z2,px2,py2,pz2,r2,dx2,dy2,dz2
      real     x3,y3,z3,px3,py3,pz3,r3,dx3,dy3,dz3
      real     cx12,cy12,cz12,dot_12
      real     cx23,cy23,cz23,dot_23
      real     cx31,cy31,cz31,dot_31

      print'(/,'' ktime_3d_raytrace_check_tdir''
     1,/,'' lu_out='',i8
     1,/,'' m_ray='',i8,'' n_ray='',i8
     1,/,'' m_tube='',i8,'' n_tube='',i8
     1)'
     1,lu_out
     1,m_ray,n_ray
     1,m_tube,n_tube

      do i_tube = 1 , n_tube

       if (it_flag(i_tube) .eq. 1) then

c23456789012345678901234567890123456789012345678901234567890123456789012
        i_ray_1 = ir_tube(1,i_tube)
        i_ray_2 = ir_tube(2,i_tube)
        i_ray_3 = ir_tube(3,i_tube)

        x1 = x_ray(i_ray_1)
        y1 = y_ray(i_ray_1)
        z1 = z_ray(i_ray_1)

        x2 = x_ray(i_ray_2)
        y2 = y_ray(i_ray_2)
        z2 = z_ray(i_ray_2)

        x3 = x_ray(i_ray_3)
        y3 = y_ray(i_ray_3)
        z3 = z_ray(i_ray_3)
        px1 = px_ray(i_ray_1)
        py1 = py_ray(i_ray_1)
        pz1 = pz_ray(i_ray_1)

        px2 = px_ray(i_ray_2)
        py2 = py_ray(i_ray_2)
        pz2 = pz_ray(i_ray_2)

        px3 = px_ray(i_ray_3)
        py3 = py_ray(i_ray_3)
        pz3 = pz_ray(i_ray_3)

        p1 = sqrt(px1**2+py1**2+pz1**2)
        px1 = px1 / p1
        py1 = py1 / p1
        pz1 = pz1 / p1

        p2 = sqrt(px2**2+py2**2+pz2**2)
        px2 = px2 / p2
        py2 = py2 / p2
        pz2 = pz2 / p2

        p3 = sqrt(px3**2+py3**2+pz3**2)
        px3 = px3 / p3
        py3 = py3 / p3
        pz3 = pz3 / p3

c  find ray tube center
        x0 = (x1 + x2 + x3) / 3.
        y0 = (y1 + y2 + y3) / 3.
        z0 = (z1 + z2 + z3) / 3.

c  take average p value - this should be the normal to the plane
        px0 = (px1 + px2 + px3) / 3.
        py0 = (py1 + py2 + py3) / 3.
        pz0 = (pz1 + pz2 + pz3) / 3.

c  normalize p
        p0 = sqrt(px0**2+py0**2+pz0**2)
        px0 = px0 / p0
        py0 = py0 / p0
        pz0 = pz0 / p0

        r1 = sqrt((x1-x0)**2+(y1-y0)**2+(z1-z0)**2)
        r2 = sqrt((x2-x0)**2+(y2-y0)**2+(z2-z0)**2)
        r3 = sqrt((x3-x0)**2+(y3-y0)**2+(z3-z0)**2)

c  normalize dx, dy, dz
        dx1 = (x1 - x0) / r1
        dy1 = (y1 - y0) / r1
        dz1 = (z1 - z0) / r1

        dx2 = (x2 - x0) / r2
        dy2 = (y2 - y0) / r2
        dz2 = (z2 - z0) / r2

        dx3 = (x3 - x0) / r3
        dy3 = (y3 - y0) / r3
        dz3 = (z3 - z0) / r3

c  compute cross product from 1 to 2, 2 to 3 and 3 to 1
c  each of these should be || to p0
        cx12 = dy1 * dz2 - dy2 * dz1
        cy12 = dz1 * dx2 - dz2 * dx1
        cz12 = dx1 * dy2 - dx2 * dy1

        cx23 = dy2 * dz3 - dy3 * dz2
        cy23 = dz2 * dx3 - dz3 * dx2
        cz23 = dx2 * dy3 - dx3 * dy2

        cx31 = dy3 * dz1 - dy1 * dz3
        cy31 = dz3 * dx1 - dz1 * dx3
        cz31 = dx3 * dy1 - dx1 * dy3

        dot_12 = cx12 * px0 + cy12 * py0 + cz12 * pz0
        dot_23 = cx23 * px0 + cy23 * py0 + cz23 * pz0
        dot_31 = cx31 * px0 + cy31 * py0 + cz31 * pz0

        if (dot_12 .lt. 0. .or. dot_23 .lt. 0. .or. dot_23 .lt. 0.) then

          print'('' dot err i_tube='',i6,'' ir='',i5,1x,i5,1x,i5
     1,'' dot='',f8.5,1x,f8.5,1x,f8.5)'
     1,i_tube,i_ray_1,i_ray_2,i_ray_3,dot_12,dot_23,dot_31

      print'(
     1 /,''    x0='',f10.2,''    y0='',f10.2,''    z0='',f10.2
     1,/,''    x1='',f10.2,''    y1='',f10.2,''    z1='',f10.2
     1,/,''    x2='',f10.2,''    y2='',f10.2,''    z2='',f10.2
     1,/,''    x3='',f10.2,''    y3='',f10.2,''    z3='',f10.2
     1)'
     1,x0,y0,z0
     1,x1,y1,z1
     1,x2,y2,z2
     1,x3,y3,z3

      print'(
     1 /,''   dx1='',f10.7,''   dy1='',f10.7,''   dz1='',f10.7
     1,/,''   dx2='',f10.7,''   dy2='',f10.7,''   dz2='',f10.7
     1,/,''   dx3='',f10.7,''   dy3='',f10.7,''   dz3='',f10.7
     1)'
     1,dx1,dy1,dz1
     1,dx2,dy2,dz2
     1,dx3,dy3,dz3

      print'(
     1 /,''   px0='',f10.7,''   py0='',f10.7,''   pz0='',f10.7
     1,/,''   px1='',f10.7,''   py1='',f10.7,''   pz1='',f10.7
     1,/,''   px2='',f10.7,''   py2='',f10.7,''   pz2='',f10.7
     1,/,''   px3='',f10.7,''   py3='',f10.7,''   pz3='',f10.7
     1)'
     1,px0,py0,pz0
     1,px1,py1,pz1
     1,px2,py2,pz2
     1,px3,py3,pz3

      print'(
     1 /,''  cx12='',f10.7,''  cy12='',f10.7,''  cz12='',f10.7
     1,/,''  cx23='',f10.7,''  cy23='',f10.7,''  cz23='',f10.7
     1,/,''  cx31='',f10.7,''  cy31='',f10.7,''  cz31='',f10.7
     1)'
     1,cx12,cy12,cz12
     1,cx23,cy23,cz23
     1,cx31,cy31,cz31

      print'(
     1 /,'' dot12='',f10.7,'' dot23='',f10.7,'' dot31='',f10.7
     1)'
     1,dot_12,dot_23,dot_31

          i_err = i_err + 1

        endif    ! if (dot_12 .lt. 0. .or. dot_23 .lt. 0. .or. dot_23 .lt. 0.) then

       endif !   if (it_flag(i_tube) .eq. 1) then                 
      enddo    ! do i_tube = 1 , n_tube

      print'(/,'' after checking ray tube directions i_err='',i8)',i_err
      if (i_err .ne. 0) stop

      return

      end

c23456789012345678901234567890123456789012345678901234567890123456789012
      subroutine ktime_3d_raytrace_check_tcon_2(
     1 it_ray,m_tube,n_tube,ir_tube,is_tube,it_tube,it_flag
     1,i_err)
c  check the list of ray tube connections to make sure they are consistent
c
c  i = input o = output b = both
c
c  i lu_out = logical unit to write ray and ray tube information to
c             lu_out < 0 means to output is written
c
c  i m_ray  = maximum number of rays
c  i n_ray  = actual number of rays established
c  i a_ray  = real array of a anglesi established dimensioned m_ray
c  i b_ray  = real array of b angles established dimensioned m_ray
c
c  i m_tube = maximum number of ray tubes
c  i n_tube = actual number of ray tubes established
c  i ir_tube = integer array of tube pointers established dimensioned 3,m_tube
c  o it_tube = integer array of side pointers established dimensioned 3,m_tube
c  o is_tube = integer array of side pointers established dimensioned 3,m_tube
c
c  i_err = error flag 0 = o.k. -1 = error
c
      implicit none

      integer  lu_out,it_ray

      integer  m_tube,n_tube
      integer  ir_tube(3,m_tube)
      integer  is_tube(3,m_tube)
      integer  it_tube(3,m_tube)
      integer  it_flag(m_tube)

      integer  i_err

      integer  i_tube_1
      integer  i_tube_2
      integer  i_tube_3,i_side_1,i_side_2,i_side_3

      i_err = 0

c  make sure the tube connections are consistent

c  for each tube ...
      do i_tube_1 = 1 , n_tube

c  check the connection of each side...
        do i_side_1 = 1 , 3

          i_side_2 = is_tube(i_side_1,i_tube_1)    ! the side opposite
          i_tube_2 = it_tube(i_side_1,i_tube_1)    ! the tube opposite

c  Some tube sides are on the  dges of the aperture and have no opposite
c         if (i_tube_2 .gt. 0 .and. it_flag(i_tube_2) .ne. -1) then    
          if (i_tube_2 .gt. 0 ) then    
c  ! if there is a tube opposite

            i_side_3 = is_tube(i_side_2,i_tube_2)
            i_tube_3 = it_tube(i_side_2,i_tube_2)

c23456789012345678901234567890123456789012345678901234567890123456789012
            if (i_side_1 .ne. i_side_3 .or. i_tube_1 .ne. i_tube_3) then

          print'('' tube connection wrong''
     1,'' i_tube='',i6,1x,i6,1x,i6,'' i_side='',i6,1x,i6,1x,i6)'
     1,i_tube_1,i_tube_2,i_tube_3
     1,i_side_1,i_side_2,i_side_3

          write(13,'('' tube connection wrong''
     1,'' i_tube='',i6,1x,i6,1x,i6,'' i_side='',i6,1x,i6,1x,i6)')
     1 i_tube_1,i_tube_2,i_tube_3
     1,i_side_1,i_side_2,i_side_3                   

            endif    ! if (i_side_1 .ne. i_side_3 .or. i_tube_1 .ne. i_tube_3) then

          endif    ! if (i_tube_2 .ne. 0) then

        enddo    ! do i_side_1 = 1 , 3

      enddo    ! do i_tube_1 = 1 , n_tube

      if (i_err .ne. 0) goto 999

      return

  999 continue
      print'(/,'' error in ktime_3d_raytrace_check_tcon'')'
      i_err = -1
      return

      end

c23456789012345678901234567890123456789012345678901234567890123456789012
      subroutine ktime_3d_check_boundary_4(it_ray,xs,ys,zs
     1,x_min,x_max,y_min,y_max,z_min,z_max
     1,m_ray,m_tube,n_ray,n_tube
     1, a_ray, b_ray
     1, x_ray, y_ray, z_ray, s_ray
     1,px_ray,py_ray,pz_ray
     1,tx_ray,ty_ray,tz_ray
     1,e1x_ray,e1y_ray,e1z_ray
     1,e2x_ray,e2y_ray,e2z_ray
     1,tc_ray,qq_ray,hq_ray
     1,gs_ray,phase_ray
     1,v_ray,vq11_ray,vq12_ray,vq22_ray
     1,vx_ray,vy_ray,vz_ray,vmax_ray
     1,n_inray,n_outray,ir_flag,ir_pointer
     1,ir_tube,it_tube,is_tube,n_intube,n_outtube
     1,it_flag,it_pointer
     1,maxangle
     1,i_err)
c  check if rays have left the travel time table area
c  update index of tubes and rays that are inside the target region
      implicit none
      integer  m_ray,m_tube

      integer   it_ray
      real      xs,ys,zs
      real      a_ray(m_ray), b_ray(m_ray), s_ray(m_ray)
      real      x_ray(m_ray), y_ray(m_ray), z_ray(m_ray)
      real     px_ray(m_ray),py_ray(m_ray),pz_ray(m_ray)
      real     tx_ray(m_ray),ty_ray(m_ray),tz_ray(m_ray)
      real     e1x_ray(m_ray),e1y_ray(m_ray),e1z_ray(m_ray)
      real     e2x_ray(m_ray),e2y_ray(m_ray),e2z_ray(m_ray)
      real     tc_ray(m_ray), qq_ray(4,m_ray), hq_ray(4,m_ray)
      real     gs_ray(m_ray), phase_ray(m_ray)
      real      v_ray(m_ray), vq11_ray(m_ray), vq12_ray(m_ray)
      real     vq22_ray(m_ray), vmax_ray(m_ray)
      real     vx_ray(m_ray), vy_ray(m_ray), vz_ray(m_ray)
      integer  ir_flag(m_ray)
      integer  ir_pointer(m_ray)
      integer  ir_tube(3,m_tube)
      integer  it_tube(3,m_tube)
      integer  is_tube(3,m_tube)
      integer  it_flag(m_tube)
      integer  it_pointer(m_tube)
      integer  n_ray,n_tube
 
      integer  i,n_inray,n_inray_2,n_outray,i_inray
      integer  i_intube,n_intube,n_intube_2
      integer  n_outtube
      real     x_min,x_max,y_min,y_max,z_min,z_max
      integer  i1,i2,i3,j_bound,i_tube
      real     u(3,3)

      real     maxangle, maxang_tmp
 
      integer  i_err
      call util_seti(n_inray,ir_flag,-1)
      call util_seti(n_intube,it_flag,-1)

      maxang_tmp=cos(3.1416/2.0)

      n_intube_2 = n_intube
      n_intube= 0
      do i_tube =1, n_intube_2

         i1 = ir_tube(1,i_tube)
         i2 = ir_tube(2,i_tube)
         i3 = ir_tube(3,i_tube)

         u(1,1) = x_ray(i1)
         u(2,1) = y_ray(i1)
         u(3,1) = z_ray(i1)
         u(1,2) = x_ray(i2)
         u(2,2) = y_ray(i2)
         u(3,2) = z_ray(i2)
         u(1,3) = x_ray(i3)
         u(2,3) = y_ray(i3)
         u(3,3) = z_ray(i3)

         j_bound = 0
         do i = 1,3

            if (u(1,i) .ge. x_min .and. u(1,i) .le. x_max
     1   .and.  u(2,i) .ge. y_min .and. u(2,i) .le. y_max
     1   .and.  u(3,i) .ge. z_min .and. u(3,i) .le. z_max)
     1      then
            j_bound = j_bound + 1
            endif !  if (u(1,i) .lt.

         enddo ! do i = 1,3

         if (j_bound .eq. 0) then

            n_outtube = n_outtube +1

         elseif (pz_ray(i1)*v_ray(i1) .lt.  maxangle
     1  .and. pz_ray(i2)*v_ray(i2) .lt. maxangle 
     1  .and. pz_ray(i3)*v_ray(i3) .lt. maxangle ) then

            n_outtube = n_outtube +1

         elseif ( u(3,1) .gt. 1800 .and. u(3,2) .gt. 1800.0
     1  .and. u(3,3) .gt. 1800 .and.
     1     pz_ray(i1)*v_ray(i1) .lt.  maxang_tmp
     1  .and. pz_ray(i2)*v_ray(i2) .lt. maxang_tmp 
     1  .and. pz_ray(i3)*v_ray(i3) .lt. maxang_tmp ) then

            n_outtube = n_outtube +1

          else

            ir_flag(i1) = 1
            ir_flag(i2) = 1
            ir_flag(i3) = 1

            n_intube = n_intube +1
            it_flag(i_tube) = i_tube - n_intube

         endif ! if (j_bound .ge. 0) then


      enddo ! do i_tube =1, n_intube

      n_inray_2 = n_inray
      n_inray = 0
      do i =1, n_inray_2
 
         if (ir_flag(i) .ne. -1) then
           n_inray = n_inray +1 
           ir_pointer(n_inray) = i
           ir_flag(i) = i - n_inray
          
         endif !if (ir_flag(i) .eq. 1) then
 
      enddo ! do i =1, n_ray
      n_outray = n_inray_2 - n_inray
 
      do i_tube =1, n_intube_2

         if (it_flag(i_tube) .ge. 0) then

            i_intube = i_tube-it_flag(i_tube)

            i1 = it_tube(1,i_tube)
            i2 = it_tube(2,i_tube)
            i3 = it_tube(3,i_tube)

            if (it_flag(i1) .ge. 0 ) then 
	       it_tube(1,i_intube) = i1 - it_flag(i1)
               is_tube(1,i_intube) = is_tube(1,i_tube) 
            else
               it_tube(1,i_intube) = -1
               is_tube(1,i_intube) = -1
            endif !if (it_flag(i1) .ne. -1) then

            if (it_flag(i2) .ge. 0) then 
               it_tube(2,i_intube) = i2 - it_flag(i2)
               is_tube(2,i_intube) = is_tube(2,i_tube) 
            else
               it_tube(2,i_intube) = -1
               is_tube(2,i_intube) = -1
            endif !if (it_flag(i2) .ne. -1) then

            if (it_flag(i3) .ge. 0) then 
               it_tube(3,i_intube) = i3 - it_flag(i3)
               is_tube(3,i_intube) = is_tube(3,i_tube) 
            else
               it_tube(3,i_intube) = -1
               is_tube(3,i_intube) = -1
            endif !if (it_flag(i3) .ne. -1) then

            i1 = ir_tube(1,i_tube)
            i2 = ir_tube(2,i_tube)
            i3 = ir_tube(3,i_tube)
            
	    if (ir_flag(i1) .ne. -1) then 
              ir_tube(1,i_intube) = i1 - ir_flag(i1)
            else
              ir_tube(1,i_intube) = -1
              print*,'ir_tube error at ',i_tube, i1
            endif !if (ir_flag(ir_tube(1,i_tube)) .ne. -1) then

            if (ir_flag(i2) .ne. -1) then 
              ir_tube(2,i_intube) = i2 - ir_flag(i2)
            else
              ir_tube(2,i_intube) = -1
              print*,'ir_tube error at ',i_tube, i2
            endif !if (ir_flag(ir_tube(2,i_tube)) .ne. -1) then

            if (ir_flag(i3) .ne. -1) then 
              ir_tube(3,i_intube) = i3 - ir_flag(i3)
            else
              ir_tube(3,i_intube) = -1
              print*,'ir_tube error at ',i_tube, i3
            endif !if (ir_flag(ir_tube(3,i_tube)) .ne. -1) then

         endif !if (it_flag(i_tube) .gt. 0) then

      enddo !do i_tube =1, n_intube_2

      do i =1, n_inray_2
 
         if (ir_flag(i) .gt. 0) then

           i_inray = i - ir_flag(i)

	   a_ray(i_inray) = a_ray(i)
	   b_ray(i_inray) = b_ray(i)
	   x_ray(i_inray) = x_ray(i)
	   y_ray(i_inray) = y_ray(i)
	   z_ray(i_inray) = z_ray(i)
	   s_ray(i_inray) = s_ray(i)
	   px_ray(i_inray) = px_ray(i)
	   py_ray(i_inray) = py_ray(i)
	   pz_ray(i_inray) = pz_ray(i)
	   tx_ray(i_inray) = tx_ray(i)
	   ty_ray(i_inray) = ty_ray(i)
	   tz_ray(i_inray) = tz_ray(i)
	   e1x_ray(i_inray) = e1x_ray(i)
	   e1y_ray(i_inray) = e1y_ray(i)
	   e1z_ray(i_inray) = e1z_ray(i)
	   e2x_ray(i_inray) = e2x_ray(i)
	   e2y_ray(i_inray) = e2y_ray(i)
	   e2z_ray(i_inray) = e2z_ray(i)
	   tc_ray(i_inray) = tc_ray(i)
	   qq_ray(1,i_inray) = qq_ray(1,i)
	   qq_ray(2,i_inray) = qq_ray(2,i)
	   qq_ray(3,i_inray) = qq_ray(3,i)
	   qq_ray(4,i_inray) = qq_ray(4,i)
	   hq_ray(1,i_inray) = hq_ray(1,i)
	   hq_ray(2,i_inray) = hq_ray(2,i)
	   hq_ray(3,i_inray) = hq_ray(3,i)
	   hq_ray(4,i_inray) = hq_ray(4,i)
	   gs_ray(i_inray) = gs_ray(i)
	   phase_ray(i_inray) = phase_ray(i)
	   v_ray(i_inray) = v_ray(i)
	   vq11_ray(i_inray) = vq11_ray(i)
	   vq12_ray(i_inray) = vq12_ray(i)
	   vq22_ray(i_inray) = vq22_ray(i)
	   vx_ray(i_inray) = vx_ray(i)
	   vy_ray(i_inray) = vy_ray(i)
	   vz_ray(i_inray) = vz_ray(i)
	   vmax_ray(i_inray) = vmax_ray(i)
	   
         endif !if (ir_flag(i) .eq. 1) then
 
      enddo ! do i =1, n_ray

      call ktime_3d_raytrace_check_tcon_2(
     1 it_ray,n_intube_2,n_intube,ir_tube,is_tube,it_tube,it_flag
     1,i_err)
      return
 
      end

c23456789012345678901234567890123456789012345678901234567890123456789012
      subroutine ktime_3d_fill_points(
     1 nullvalue,m_xyz
     1,ix_tab,nx_tab,x0_tab,dx_tab
     1       ,ny_tab,y0_tab,dy_tab
     1,iz_tab,nz_tab,z0_tab,dz_tab
     1,t_tab,amp_tab,phase_tab,a_tab,b_tab
     1,q11_tab,q12_tab,q21_tab,q22_tab,p3_tab
     1,e2z_tab
     1,i_err
     1)
ccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccc
c  fill the residue gridpoints that haven't been filled by 
c  ktime_3d_receiver().  the quantities on these points are estimated
c  by the average traveltime of the six points around them(if they have
c  traveltimes).
cccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccc
      implicit none

      real     nullvalue
      integer  m_xyz
      real     t_tab(1)
      integer  ix_tab(1),nx_tab(1)
      real     x0_tab(1),dx_tab
 
      integer            ny_tab
      real     y0_tab,dy_tab
 
      integer  iz_tab(1),nz_tab(1)
      real     z0_tab(1),dz_tab

      real     amp_tab(m_xyz)
      real     a_tab(m_xyz),b_tab(m_xyz)
      real     q11_tab(m_xyz),q12_tab(m_xyz)
      real     q21_tab(m_xyz),q22_tab(m_xyz),p3_tab(m_xyz)
      real     e2z_tab(m_xyz)

      real     phase_tab(m_xyz)

      integer  n_xyz,i_xyz,i_xyz0,ix,iy,iz,i_num,i_xy0
      integer  i1,i2,i3,i4,i5,i6,i_unset
      real     time,amp

      real     q11,q12,q21,q22,p3

      real     phase
      integer  i_err

      real     e2z,a_t,b_t

      n_xyz = nx_tab(1)*ny_tab*nz_tab(1)      

      i_xyz0 = 1 + nz_tab(1)*(nx_tab(1)-1)/2 
     1           + nz_tab(1)*nx_tab(1)*(ny_tab-1)/2
      t_tab(i_xyz0) = 0.0
      amp_tab(i_xyz0) = amp_tab(i_xyz0+1)
      phase_tab(i_xyz0) =0
      a_tab(i_xyz0) =0.0
      b_tab(i_xyz0) =0.0
      q11_tab(i_xyz0) = q11_tab(i_xyz0+1)
      q12_tab(i_xyz0) = q12_tab(i_xyz0+1)
      q21_tab(i_xyz0) = q21_tab(i_xyz0+1)
      q22_tab(i_xyz0) = q22_tab(i_xyz0+1)
      e2z_tab(i_xyz0) = e2z_tab(i_xyz0+1)

      i_unset = 0
      do i_xyz = 1, n_xyz 

         if (abs(t_tab(i_xyz)-nullvalue) .le. 0.01 .and.
     1        i_xyz .ne. i_xyz0	 ) then

           i_unset = i_unset + 1
           time = 0.0
           amp = 0.0
           phase = 0
	   q11=0.0
	   q12=0.0
	   q21=0.0
	   q22=0.0
	   p3=0.0
	   e2z=0.0
	   a_t=0.0
	   b_t=0.0

           i_num = 0
           iz = mod(i_xyz, nz_tab(1))
           i_xy0 = i_xyz/nz_tab(1)
           if( iz .eq. 0) then
              iz = nz_tab(1)
              i_xy0 = i_xy0-1
           endif
           ix = mod(i_xy0,nx_tab(1)) + 1
           iy = i_xy0/nx_tab(1) + 1
           
           if (ix .gt. 1) then
              i1 = ((iy-1)*nx_tab(1) + ix-2)*nz_tab(1) + iz
           else
              i1 = -1
           endif 

           if(ix .lt. nx_tab(1)) then
              i2 = ((iy-1)*nx_tab(1) + ix)*nz_tab(1) + iz
           else
              i2=-1
           endif

           if (iy .gt. 1) then 
              i3 = ((iy-2)*nx_tab(1) + ix-1)*nz_tab(1) + iz
           else
              i3 = -1
           endif

           if (iy .lt. ny_tab) then
              i4 = (iy*nx_tab(1) + ix-1)*nz_tab(1) + iz 
           else
              i4 = -1
           endif

           if (iz .gt. 1) then
               i5 = i_xyz -1
           else
               i5=-1
           endif

           if (iz .lt. nz_tab(1)) then
               i6 = i_xyz +1
           else
               i6 = -1
           endif

           if (i1 .gt. 0 .and. abs(t_tab(i1)-nullvalue) .ge. 0.01) then
              time = time + t_tab(i1)
              amp = amp + amp_tab(i1)
              phase = phase + phase_tab(i1)
	      q11 = q11 +q11_tab(i1)
	      q12 = q12 +q12_tab(i1)
	      q21 = q21 +q21_tab(i1)
	      q22 = q22 +q22_tab(i1)
	      p3 = p3 +p3_tab(i1)
	      e2z = e2z + e2z_tab(i1)
	      a_t = a_t + a_tab(i1)
	      b_t = b_t + b_tab(i1)
              i_num = i_num + 1
           endif ! if (i1 .gt. 0  .and. t_tab(i1) .le. 10.0 ) then

           if (i2 .gt. 0 .and. abs(t_tab(i2)-nullvalue) .ge. 0.01) then
              time = time + t_tab(i2)
              amp = amp + amp_tab(i2)
              phase = phase + phase_tab(i2)
	      q11 = q11 +q11_tab(i2)
	      q12 = q12 +q12_tab(i2)
	      q21 = q21 +q21_tab(i2)
	      q22 = q22 +q22_tab(i2)
	      p3 = p3 +p3_tab(i2)
	      e2z = e2z + e2z_tab(i2)
	      a_t = a_t + a_tab(i2)
	      b_t = b_t + b_tab(i2)
              i_num = i_num + 1
           endif ! if (i1 .gt. 0  .and. t_num(i1) .ne. 0 ) then

           if (i3 .gt. 0 .and. abs(t_tab(i3)-nullvalue) .ge. 0.01) then
              time = time + t_tab(i3)
              amp = amp + amp_tab(i3)
              phase = phase + phase_tab(i3)
	      q11 = q11 +q11_tab(i3)
	      q12 = q12 +q12_tab(i3)
	      q21 = q21 +q21_tab(i3)
	      q22 = q22 +q22_tab(i3)
	      p3 = p3 +p3_tab(i3)
	      e2z = e2z + e2z_tab(i3)
	      a_t = a_t + a_tab(i3)
	      b_t = b_t + b_tab(i3)
              i_num = i_num + 1
           endif ! if (i1 .gt. 0  .and. t_num(ij) .ne. 0 ) then

           if (i4 .gt. 0 .and. abs(t_tab(i4)-nullvalue) .ge. 0.01) then
              time = time + t_tab(i4)
              amp = amp + amp_tab(i4)
              phase = phase + phase_tab(i4)
	      q11 = q11 +q11_tab(i4)
	      q12 = q12 +q12_tab(i4)
	      q21 = q21 +q21_tab(i4)
	      q22 = q22 +q22_tab(i4)
	      p3 = p3 +p3_tab(i4)
	      e2z = e2z + e2z_tab(i4)
	      a_t = a_t + a_tab(i4)
	      b_t = b_t + b_tab(i4)
              i_num = i_num + 1
           endif ! if (i1 .gt. 0  .and. t_num(i1) .ne. 0 ) then

           if (i5 .gt. 0 .and. abs(t_tab(i5)-nullvalue) .ge. 0.01) then
              time = time + t_tab(i5)
              amp = amp + amp_tab(i5)
              phase = phase + phase_tab(i5)
	      q11 = q11 +q11_tab(i5)
	      q12 = q12 +q12_tab(i5)
	      q21 = q21 +q21_tab(i5)
	      q22 = q22 +q22_tab(i5)
	      p3 = p3 +p3_tab(i5)
	      e2z = e2z + e2z_tab(i5)
	      a_t = a_t + a_tab(i5)
	      b_t = b_t + b_tab(i5)
              i_num = i_num + 1
           endif ! if (i5 .gt. 0  .and. t_num(i5) .ne. 0 ) then

           if (i6 .gt. 0 .and. abs(t_tab(i6)-nullvalue) .ge. 0.01) then
              time = time + t_tab(i6)
              amp = amp + amp_tab(i6)
              phase = phase + phase_tab(i6)
	      q11 = q11 +q11_tab(i6)
	      q12 = q12 +q12_tab(i6)
	      q21 = q21 +q21_tab(i6)
	      q22 = q22 +q22_tab(i6)
	      p3 = p3 +p3_tab(i6)
	      e2z = e2z + e2z_tab(i6)
	      a_t = a_t + a_tab(i6)
	      b_t = b_t + b_tab(i6)
              i_num = i_num + 1
           endif ! if (i6 .gt. 0  .and. t_num(i6) .ne. 0 ) then

           if (i_num .ne. 0) then
              t_tab(i_xyz) = time/i_num
              amp_tab(i_xyz) = amp/i_num
              phase_tab(i_xyz) = phase/i_num
              q11_tab(i_xyz) = q11/i_num
              q12_tab(i_xyz) = q12/i_num
              q21_tab(i_xyz) = q21/i_num
              q22_tab(i_xyz) = q22/i_num
              p3_tab(i_xyz) = p3/i_num
              e2z_tab(i_xyz) = e2z/i_num
              a_tab(i_xyz) = a_t/i_num
              b_tab(i_xyz) = b_t/i_num
           endif ! if (i_num .ne. 0) then
               
         endif ! if (t_num(i_xyz) .eq. 0 ) then

      enddo ! do i_xyz = 1, n_xyz

c     print'('' unset points:'',i8)',i_unset

      return
      end
c23456789012345678901234567890123456789012345678901234567890123456789012
      subroutine ktime_3d_raytrace_gaussj(a,n,np,b,m,mp)
      implicit none

      integer  n,np,m,mp
      real     a(3,3), b(3) ,ainv(3,3)
  
      real     deter,b1,b2

      deter = a(1,1)*a(2,2)*a(3,3) + a(2,1)*a(3,2)*a(1,3)
     1      + a(3,1)*a(1,2)*a(2,3) - a(3,1)*a(2,2)*a(1,3)
     1      - a(2,1)*a(1,2)*a(3,3) - a(1,1)*a(3,2)*a(2,3)

      if ( abs(deter) .le. 0.01) then

         b(1) = 0.1
         b(2) = 0.1
         b(3) = 0.0
         print'('' cannot tell'')'
         return

      endif ! if ( abs(deter) .le. 0.01) then 

      ainv(1,1) = a(2,2)*a(3,3) - a(3,2)*a(2,3)
      ainv(1,1) = ainv(1,1)/deter

      ainv(1,2) = a(3,1)*a(2,3) - a(2,1)*a(3,3)
      ainv(1,2) = ainv(1,2)/deter

      ainv(1,3) = a(2,1)*a(3,2) - a(3,1)*a(2,2)
      ainv(1,3) = ainv(1,3)/deter

      ainv(2,1) = a(3,2)*a(1,3) - a(1,2)*a(3,3)
      ainv(2,1) = ainv(2,1)/deter

      ainv(2,2) = a(1,1)*a(3,3) - a(3,1)*a(1,3)
      ainv(2,2) = ainv(2,2)/deter

      ainv(2,3) = a(3,1)*a(1,2) - a(1,1)*a(3,2)
      ainv(2,3) = ainv(2,3)/deter

      b1 = a(1,1)*b(1) + a(1,2)*b(2) + a(1,3)*b(3)
      b2 = a(2,1)*b(1) + a(2,2)*b(2) + a(2,3)*b(3)  

      b(1) = b1
      b(2) = b2

      return
      end
c23456789012345678901234567890123456789012345678901234567890123456789012
      subroutine ktime_3d_raytrace_gaussj_0(a,n,np,b,m,mp)
      implicit none

      integer  n,np,m,mp
      real     a(3,3), b(3)
                                  
      integer  ipiv(3),indxr(3),indxc(3)
      integer  i,j,k,l,ll,irow,icol
      real     big,dum,pivinv

      do j=1,3
         ipiv(j) = 0
      enddo ! do j=1,3

      do i=1,3

         big = 0.0
         do j=1,3

            if (ipiv(j) .ne. 1) then

               do k=1,3 

                  if(ipiv(k) .eq. 0) then

                     if (abs(a(j,k)) .ge. big) then
                        big = abs(a(j,k))
                        irow = j
                        icol =k
                     endif ! if (abs(a(j,k)) .ge. big) then

                  elseif (ipiv(k) .gt. 1) then

                     pause 'Singular matrix'
                  endif ! if(ipiv(k) .eq. 0) then

               enddo ! do k=1,3  

            endif ! if (ipiv(j) .ne. 1) then 

         enddo ! do j=1,3
 
         ipiv(icol) = ipiv(icol)+1
         if (irow .ne. icol) then

            do l=1,3
               dum = a(irow,l)
               a(irow,l) = a(icol,l)
               a(icol,l) = dum
            enddo! do l=1,3 
            dum = b(irow)
            b(irow) = b(icol)
            b(icol) = dum
         endif !if (irow .ne. icol) then

         indxr(i) = irow
         indxc(i) = icol
         if (a(icol,icol) .eq. 0) pause 'Singular matrix'
         pivinv = 1.0/a(icol,icol)
         a(icol,icol)=1.0

         do l=1,3 
            a(icol,l) = a(icol,l)*pivinv
         enddo ! do l=1,3
         b(icol) = b(icol)*pivinv
         
         do ll=1,3

            if (ll .ne. icol) then
 
                dum = a(ll,icol)
                a(ll, icol) = 0.0
                do l=1,3
                   a(ll,l) = a(ll,l) - a(icol,l)*dum
                enddo !  do l=1,3
                b(ll) = b(ll)-b(icol)*dum

            endif !if (ll ne, icol) then                                                
         enddo!do ll=1,3 

      enddo ! do i=1,3

      do l=1,3,1 ! -1 to 1

         if (indxr(l) .ne. indxc(l)) then

            do k=1,3
               dum = a(k,indxr(l))
               a(k,indxr(l)) = a(k,indxc(l))
               a(k,indxc(l)) = dum
            enddo! do k=1,3
         endif! if (indxr(l) .ne. indxc(l)) then
      enddo ! do l=1,3,-1 

      return
      end
c23456789012345678901234567890123456789012345678901234567890123456789012
      subroutine ktime_3d_raytrace_intube_4(
     1 on_flag,x,y,z
     1,i1,i2,i3
     1,x_ray,y_ray,z_ray
     1,norm_x,norm_y,norm_z
     1,px,py,pz
     1,lambda,u,v
     1,inter_flag
     1)
      implicit none
c on_flag: flag that indicates that to which (old or new WF) the grid point
c          is relatively computed.
c          1-- relative to the old WF,i.e., x_ray_1,y_ray_1,z_ray_1;
c          2-- relative to the new WF,i.e., x_ray_2,y_ray_2,z_ray_2;
c          0-- return only the innerproduct lambda, and skip the crossproduct
c              u and v.
c inter_flag: flag that indicates whether the grid point falling in the ray tube
c             -1 ---not in the ray tube
c              1 ---ready for the further checking.
      integer  on_flag, inter_flag
      real     x,y,z,px,py,pz
      integer  i1,i2,i3
      real     x_ray(1),y_ray(1),z_ray(1)
      real     lambda,lambda_1,u,v,pnorm
 
      real     r1_x, r1_y, r1_z
      real     r2_x, r2_y, r2_z
 
      real     norm_x, norm_y, norm_z, norm_3
      real     x_project, y_project, z_project
 
      real     d1,d2,d3

      real     c1
 
      real     eps
      data     eps/1.e-6/
 
      inter_flag = 1
      r1_x = x_ray(i2) - x_ray(i1)
      r1_y = y_ray(i2) - y_ray(i1)
      r1_z = z_ray(i2) - z_ray(i1)
 
      r2_x = x_ray(i3) - x_ray(i1)
      r2_y = y_ray(i3) - y_ray(i1)
      r2_z = z_ray(i3) - z_ray(i1)

      lambda = (x-x_ray(i1))*norm_x + (y-y_ray(i1))*norm_y
     1        +(z-z_ray(i1))*norm_z
 
      if (on_flag .eq. 0) return

      pnorm = px*norm_x + py*norm_y + pz*norm_z
      if (abs(pnorm) .le. eps ) then
           inter_flag = -1
           return
      endif !if (abs(pnorm) .ge. eps )

      lambda_1 = lambda/pnorm

      x_project = x - lambda_1*px
      y_project = y - lambda_1*py
      z_project = z - lambda_1*pz

      c1 = norm_x*(x_project - x_ray(i1)) + norm_y
     1   *(y_project - y_ray(i1)) +norm_z*(z_project - z_ray(i1))

      if (abs(c1) .gt. 1.0) then

           print*,'intube_4 error at:', x,y,z,c1,pnorm
           inter_flag = -1
           return
      endif !if (abs(c1-1.0) .gt. 0.03 ) then

      d1 = r1_x*r2_y-r1_y*r2_x
      d2 = r1_x*r2_z-r1_z*r2_x
 
      d3 = (x_project - x_ray(i1))**2 + (y_project - y_ray(i1))**2
     1    +(z_project - z_ray(i1))**2

      if (abs(d1) .ge. eps) then
 
        u = (x_project -x_ray(i1))*r2_y - (y_project - y_ray(i1))*r2_x
        u = u/d1
        v = (y_project - y_ray(i1))*r1_x - (x_project -x_ray(i1))*r1_y
        v = v/d1

      elseif (abs(d2) .ge. eps) then
 
        u = (x_project -x_ray(i1))*r2_z - (z_project - z_ray(i1))*r2_x
        u = u/d2
        v = (z_project - z_ray(i1))*r1_x - (x_project - x_ray(i1))*r1_z
        v = v/d2
 
      elseif ( d3 .le. eps) then
 
          u = 0.0
          v = 0.0
 
      else
 
          inter_flag = -1
      endif ! if (abs(d1) .ge. eps) then
 
      return
 
      end
c23456789012345678901234567890123456789012345678901234567890123456789012
      subroutine ktime_3d_raytrace_intersect_2(
     1 x1,y1,z1,px1,py1,pz1,px2,py2,pz2
     1,x_project,y_project,z_project
     1,t1,t2,inter_flag
     1)
      implicit none
 
      real     x1,y1,z1,px1,py1,pz1
      real     px2,py2,pz2
      real     x_project,y_project,z_project
 
      integer  inter_flag
 
      real     d1,d2,d3
      real     t1,t2
 
      real     eps
      data     eps/1.e-4/
 
      inter_flag = 1
 
      d1 = px1*py2-py1*px2
      d2 = px1*pz2-pz1*px2

      d3 = (x_project - x1)**2 + (y_project - y1)**2
     1    +(z_project - z1)**2
 
      if (abs(d1) .ge. eps) then
 
        t1 = (x_project -x1)*py2 - (y_project - y1)*px2
        t1 = t1/d1
        t2 = (y_project - y1)*px1 - (x_project -x1)*py1
        t2 = t2/d1
 
      elseif (abs(d2) .ge. eps) then
 
        t1 = (x_project -x1)*pz2 - (z_project - z1)*px2
        t1 = t1/d2
        t2 = (z_project - z1)*px1 - (x_project - x1)*pz1 
        t2 = t2/d2

      elseif ( d3 .le. 1.0) then

          t1 = 0.0
          t2 = 0.0

      else

          t1=9.0
          t2=9.0
          inter_flag = -1
      endif ! if (abs(t1) .ge. 0.1) then

      return
      end
c23456789012345678901234567890123456789012345678901234567890123456789012
      subroutine ktime_3d_raytrace_crossproduct(
     1 n_cross
     1,vec1_x, vec1_y, vec1_z
     1,vec2_x, vec2_y, vec2_z
     1,vec3_x, vec3_y, vec3_z)
      implicit none

      integer  n_cross
      
      real     vec1_x(1), vec1_y(1), vec1_z(1)
      real     vec2_x(1), vec2_y(1), vec2_z(1)
      real     vec3_x(1), vec3_y(1), vec3_z(1)

      integer  i
      real     norm_3

      real     eps
      data     eps/1.e-6/
 
      do i = 1, n_cross

         vec3_x(i) = vec1_y(i)*vec2_z(i) - vec2_y(i)*vec1_z(i)
         vec3_y(i) = vec2_x(i)*vec1_z(i) - vec1_x(i)*vec2_z(i)
         vec3_z(i) = vec1_x(i)*vec2_y(i) - vec2_x(i)*vec1_y(i)

         norm_3 = sqrt(vec3_x(i)*vec3_x(i) 
     1          + vec3_y(i)*vec3_y(i) + vec3_z(i)*vec3_z(i)) 

         vec3_x(i) = vec3_x(i)/max(eps,norm_3)
         vec3_y(i) = vec3_y(i)/max(eps,norm_3)  
         vec3_z(i) = vec3_z(i)/max(eps,norm_3)  
   
      enddo ! do i = 1, n_cross

      return
      end
c23456789012345678901234567890123456789012345678901234567890123456789012
      subroutine ktime_3d_raytrace_crosspro_2(
     1 n_tube, n_intube, it_pointer, it_flag, ir_tube
     1,x_ray, y_ray, z_ray
     1,norm_x, norm_y, norm_z)
      implicit none

      integer  n_tube, n_intube, i_intube, i_tube
      integer  it_pointer(n_intube),ir_tube(3,n_tube)
      integer  it_flag(n_tube)

      real     x_ray(1), y_ray(1), z_ray(1)
      real     norm_x(1), norm_y(1), norm_z(1)

      integer  i1,i2,i3

      real     r1_x, r1_y, r1_z
      real     r2_x, r2_y, r2_z

      real     norm_3
 
      real     eps
      data     eps/1.e-6/

      do i_tube = 1, n_intube

         i1 = ir_tube(1,i_tube)
         i2 = ir_tube(2,i_tube)
         i3 = ir_tube(3,i_tube)

         r1_x = x_ray(i2) - x_ray(i1)
         r1_y = y_ray(i2) - y_ray(i1)
         r1_z = z_ray(i2) - z_ray(i1)
 
         r2_x = x_ray(i3) - x_ray(i1)
         r2_y = y_ray(i3) - y_ray(i1)
         r2_z = z_ray(i3) - z_ray(i1)
 
         norm_x(i_tube) = r1_y*r2_z - r2_y*r1_z
         norm_y(i_tube) = r2_x*r1_z - r1_x*r2_z
         norm_z(i_tube) = r1_x*r2_y - r2_x*r1_y
 
         norm_3 = sqrt(norm_x(i_tube)*norm_x(i_tube)
     1               + norm_y(i_tube)*norm_y(i_tube) 
     1               + norm_z(i_tube)*norm_z(i_tube))
 
         norm_x(i_tube) = norm_x(i_tube)/max(eps,norm_3)
         norm_y(i_tube) = norm_y(i_tube)/max(eps,norm_3)
         norm_z(i_tube) = norm_z(i_tube)/max(eps,norm_3)
  
      enddo !do i_intube = 1, n_intube

      return
      end
c23456789012345678901234567890123456789012345678901234567890123456789012
      subroutine ktime_3d_raytrace_is_cross(
     1 n_tube, n_intube, ir_tube
     1,a_ray,b_ray,v_ray
     1,x_ray_1, y_ray_1, z_ray_1,px_ray_1, py_ray_1, pz_ray_1
     1,x_ray_2, y_ray_2, z_ray_2,px_ray_2, py_ray_2, pz_ray_2
     1,norm_x_1, norm_y_1, norm_z_1
     1,norm_x_2, norm_y_2, norm_z_2)
      implicit none

      integer  n_tube, n_intube, i_intube, i_tube
      integer  ir_tube(3,n_tube)

      real     a_ray(1), b_ray(1),v_ray(1)
      real     x_ray_1(1), y_ray_1(1), z_ray_1(1)
      real     px_ray_1(1), py_ray_1(1), pz_ray_1(1)
      real     x_ray_2(1), y_ray_2(1), z_ray_2(1)
      real     px_ray_2(1), py_ray_2(1), pz_ray_2(1)
      real     norm_x_1(1), norm_y_1(1), norm_z_1(1)
      real     norm_x_2(1), norm_y_2(1), norm_z_2(1)

      integer  i1,i2,i3

      real     pdotn_1, ndotn

      real     norm_3
 
      real     eps
      data     eps/1.e-6/

      do i_tube = 1, n_intube

         i1 = ir_tube(1,i_tube)
         i2 = ir_tube(2,i_tube)
         i3 = ir_tube(3,i_tube)

         pdotn_1 = px_ray_1(i1)*norm_x_1(i1) + py_ray_1(i1)*norm_y_1(i1) 
     1         + pz_ray_1(i1)*norm_z_1(i1)
         pdotn_1 = pdotn_1*v_ray(i1)

         ndotn = norm_x_2(i1)*norm_x_1(i1) + norm_y_2(i1)*norm_y_1(i1) 
     1         + norm_z_2(i1)*norm_z_1(i1)

         if (ndotn .le. 0.0) then

          print*,'ndotn:', i_tube,i1,a_ray(i1)*180/3.1415926
     1    ,b_ray(i1)*180/3.1415926
     1    ,acos(pz_ray_1(i1)*v_ray(i1))*180/3.1415926
     1    ,acos(pz_ray_2(i1)*v_ray(i1))*180/3.1415926
     1    ,acos(ndotn)*180/3.1415926

          print*,i1,x_ray_1(i1),y_ray_1(i1),z_ray_1(i1)
     1    ,x_ray_2(i1),y_ray_2(i1),z_ray_2(i1)
          print*,i2,x_ray_1(i2),y_ray_1(i2),z_ray_1(i2)
     1    ,x_ray_2(i2),y_ray_2(i2),z_ray_2(i2)
          print*,i3,x_ray_1(i3),y_ray_1(i3),z_ray_1(i3)
     1    ,x_ray_2(i3),y_ray_2(i3),z_ray_2(i3)
          print*,x_ray_1(i2)-x_ray_1(i1),y_ray_1(i2)-y_ray_1(i1)
     1    ,z_ray_1(i2)-z_ray_1(i1),x_ray_1(i3)-x_ray_1(i1)
     1    ,y_ray_1(i3)-y_ray_1(i1),z_ray_1(i3)-z_ray_1(i1)
          print*,x_ray_2(i2)-x_ray_2(i1),y_ray_2(i2)-y_ray_2(i1)
     1    ,z_ray_2(i2)-z_ray_2(i1),x_ray_2(i3)-x_ray_2(i1)
     1    ,y_ray_2(i3)-y_ray_2(i1),z_ray_2(i3)-z_ray_2(i1)
          print*,i1,norm_x_1(i1),norm_y_1(i1),norm_z_1(i1)
     1    ,norm_x_2(i1),norm_y_2(i1),norm_z_2(i1)

         endif !if (ndotn .le. 0.0) then

	enddo !do i_intube = 1, n_intube

      return
      end
c23456789012345678901234567890123456789012345678901234567890123456789012
      subroutine ktime_3d_receiver_4(it_ray,xs,ys,zs,vs
     1,inter_type,m_xyz,points
     1,t_ray_1,t_ray_2,n_ray,a_ray,b_ray,n_tube
     1,x_ray_1,y_ray_1,z_ray_1
     1,s_ray_1
     1,e1x_ray_1,e1y_ray_1,e1z_ray_1
     1,e2x_ray_1,e2y_ray_1,e2z_ray_1
     1,px_ray_1,py_ray_1,pz_ray_1
     1,tc_ray_1,qq_ray_1
     1,gs_ray_1,phase_ray_1
     1,v_ray_1
     1,norm_x1,norm_y1,norm_z1
     1,x_ray_2,y_ray_2,z_ray_2
     1,s_ray_2
     1,e1x_ray_2,e1y_ray_2,e1z_ray_2
     1,e2x_ray_2,e2y_ray_2,e2z_ray_2
     1,px_ray_2,py_ray_2,pz_ray_2
     1,tc_ray_2,qq_ray_2
     1,gs_ray_2,phase_ray_2
     1,v_ray_2
     1,norm_x2,norm_y2,norm_z2
     1,vmax_ray,vmax_tab,det33_tab
     1,ir_tube,is_tube,it_tube
     1,it_flag,n_intube,it_pointer
     1,ix_tab,nx_tab,x0_tab,dx_tab
     1       ,ny_tab,y0_tab,dy_tab
     1,iz_tab,nz_tab,z0_tab,dz_tab
     1,t_tab,amp_tab,phase_tab
     1,q11_tab,q12_tab,q21_tab,q22_tab,p3_tab
     1,s_tab,a_tab,b_tab,e2z_tab,nt_num
     1,m_work,work,i_err,t_cpu_intube,t_cpu_total
     1)
      implicit none
      integer  it_ray
      integer  n_tube,n_ray
      integer  ir_tube(3,n_tube)
      integer  is_tube(3,n_tube)
      integer  it_tube(3,n_tube)
      integer  m_xyz
      real     amp_tab(m_xyz)
      real     phase_tab(m_xyz)
      real     q11_tab(m_xyz),q12_tab(m_xyz)
      real     q21_tab(m_xyz),q22_tab(m_xyz),p3_tab(m_xyz)
      real     s_tab(m_xyz)
      real     a_tab(m_xyz),b_tab(m_xyz),e2z_tab(m_xyz)
      integer  nt_num
      integer  points(m_xyz),n_point
      integer  inter_type
 
      real     xs,ys,zs,vs
      real     t_ray_1,t_ray_2
      integer  i_intube, n_intube
      integer  it_flag(n_tube),it_pointer(n_intube)
      real     a_ray(n_ray),b_ray(n_ray)
      real     x_ray_1(n_ray),y_ray_1(n_ray),z_ray_1(n_ray)
      real     s_ray_1(n_ray),tc_ray_1(n_ray)
      real     e1x_ray_1(n_ray),e1y_ray_1(n_ray),e1z_ray_1(n_ray)
      real     e2x_ray_1(n_ray),e2y_ray_1(n_ray),e2z_ray_1(n_ray)
      real     px_ray_1(n_ray),py_ray_1(n_ray),pz_ray_1(n_ray)
      real     v_ray_1(n_ray),gs_ray_1(n_ray),qq_ray_1(4,n_ray)
      real     norm_x1(n_tube),norm_y1(n_tube),norm_z1(n_tube)
      integer  phase_ray_1(n_ray)
 
      real     x_ray_2(n_ray),y_ray_2(n_ray),z_ray_2(n_ray)
      real     s_ray_2(n_ray),tc_ray_2(n_ray)
      real     e1x_ray_2(n_ray),e1y_ray_2(n_ray),e1z_ray_2(n_ray)
      real     e2x_ray_2(n_ray),e2y_ray_2(n_ray),e2z_ray_2(n_ray)
      real     px_ray_2(n_ray),py_ray_2(n_ray),pz_ray_2(n_ray)
      real     v_ray_2(n_ray),gs_ray_2(n_ray),qq_ray_2(4,n_ray)
      real     norm_x2(n_tube),norm_y2(n_tube),norm_z2(n_tube)
      integer  phase_ray_2(n_ray)
      real     phase
      integer  i_anga,ix_anga,iz_anga
      real     a1,a2,p3_a,q11_a,q22_a,r1,r2,temp

      real     vmax_ray(1),vmax_tab(1),det33_tab(1)
 
      integer  ix_tab(1),nx_tab(1)
      real     x0_tab(1),dx_tab
 
      integer            ny_tab
      real     y0_tab,dy_tab
 
      integer  iz_tab(1),nz_tab(1)
      real     z0_tab(1),dz_tab
      real     t_tab(1)

      integer  m_work
      real     work(m_work)
      integer  i_err
 
      real     xmin,xmax,ymin,ymax,zmin,zmax
 
      integer  ixmin,ixmax,iymin,iymax,izmin,izmax
      integer  ix,iy,iz,i_tube,i_xy,i_xyz
      real     x,y,z,r
      integer  i1,i2,i3
      real     amp,amp0,amp1,amp2,amp3,amp4,timet,trans
      real     amp1_1,amp1_2,amp2_1,amp2_2,amp3_1,amp3_2
      real     s_path,vmax
 
      real     lambda_1,u_1,v_1
      real     lambda_2,u_2,v_2

      real     t1_1,t2_1,t3_1
      real     t1_2,t2_2,t3_2
      real     t1,t2,t3,tt1,tt2,tt3
      real     q11,q12,q21,q22
      real     p3_t,px_t,py_t

      integer  i_flag_1, i_flag_2
      integer  in_flag 

      real     t_cpu_0, t_cpu_1, t_cpu_2, t_cpu_3
      real     t_cpu_total,t_cpu_intube
      real     time1, time3, time4
      real     second

      real     tc_ray_11,tc_ray_12,tc_ray_13
      real     tc_ray_21,tc_ray_22,tc_ray_23
      real     tc

      real     e1x,e1y,e1z,e2x,e2y,e2z,e3x,e3y,e3z
      real     a0_t,b0_t,a1_t,b1_t

      real     q11i,q12i,q21i,q22i
      real     e1x0,e1y0,e2x0,e2y0
      real     tmp11,tmp12,tmp13,tmp21,tmp22,tmp23
      real     det21,det22,det23,det31,det32,det33

      real     s1,s2,p1,p2

      real     t_eps,uv_eps
 
      amp0 = 0.25 /3.1415926
      amp3 = 1.2/1.482 
      amp4 = 1.5/2.397
      amp1 = 0.9
      uv_eps = 0.01

      t_eps = t_ray_1 * 0.01

      u_1 = 0.4
      u_2 = 0.4
      v_1 = 0.4
      v_2 = 0.4
      time1 = 0.0
      time3 = 0.0
      time4 = 0.0
      t_cpu_intube = 0.0
      n_point=0
      nt_num = 0

      do i_tube = 1, n_intube
 
        i1 = ir_tube(1,i_tube)
        i2 = ir_tube(2,i_tube)
        i3 = ir_tube(3,i_tube)

        xmin=min(x_ray_1(i1),x_ray_1(i2),x_ray_1(i3),x_ray_2(i1),
     1            x_ray_2(i2),x_ray_2(i3))
        xmax=max(x_ray_1(i1),x_ray_1(i2),x_ray_1(i3),x_ray_2(i1),
     1            x_ray_2(i2),x_ray_2(i3))
        ymin=min(y_ray_1(i1),y_ray_1(i2),y_ray_1(i3),y_ray_2(i1),
     1            y_ray_2(i2),y_ray_2(i3))
        ymax=max(y_ray_1(i1),y_ray_1(i2),y_ray_1(i3),y_ray_2(i1),
     1            y_ray_2(i2),y_ray_2(i3))
        zmin=min(z_ray_1(i1),z_ray_1(i2),z_ray_1(i3),z_ray_2(i1),
     1            z_ray_2(i2),z_ray_2(i3))
        zmax=max(z_ray_1(i1),z_ray_1(i2),z_ray_1(i3),z_ray_2(i1),
     1            z_ray_2(i2),z_ray_2(i3))
 
c subset of gridpoints of interests
 
        ixmin = (xmin-xs-x0_tab(1)-0.1)/dx_tab +2
        ixmin = max(1, ixmin)
        ixmax = (xmax-xs-x0_tab(1))/dx_tab + 1
        ixmax = min(nx_tab(1), ixmax )
        iymin = (ymin-ys-y0_tab -0.1)/dy_tab +2
        iymin = max(1, iymin)
        iymax = (ymax-ys-y0_tab)/dy_tab + 1
        iymax = min(ny_tab, iymax)
        izmin = (zmin-zs-z0_tab(1)-0.1)/dz_tab +2
        izmin = max(1, izmin)
        izmax = (zmax-zs-z0_tab(1))/dz_tab + 1
        izmax = min(nz_tab(1), izmax )

        if (ixmin .le. ixmax .and. iymin .le. iymax
     1       .and. izmin .le. izmax) then
 
          do iy = iymin, iymax
            do ix = ixmin, ixmax
              i_xy = ix_tab(iy) + ix
              do iz = izmin, izmax
 
                i_xyz = iz_tab(i_xy) + iz
 
                y = y0_tab + (iy - 1) * dy_tab +ys
                x = x0_tab(iy) + (ix - 1) * dx_tab +xs
                z = z0_tab(i_xy) + (iz - 1) * dz_tab

                t1_1 = (x-x_ray_1(i1))*px_ray_1(i1) +(y-y_ray_1(i1))
     1                 *py_ray_1(i1) + (z-z_ray_1(i1))*pz_ray_1(i1)
                t2_1 = (x-x_ray_1(i2))*px_ray_1(i2) +(y-y_ray_1(i2))
     1                 *py_ray_1(i2) + (z-z_ray_1(i2))*pz_ray_1(i2)
                t3_1 = (x-x_ray_1(i3))*px_ray_1(i3) +(y-y_ray_1(i3))
     1                 *py_ray_1(i3) + (z-z_ray_1(i3))*pz_ray_1(i3)

                t1_2 = (x-x_ray_2(i1))*px_ray_2(i1) +(y-y_ray_2(i1))
     1                 *py_ray_2(i1) + (z-z_ray_2(i1))*pz_ray_2(i1)
                t2_2 = (x-x_ray_2(i2))*px_ray_2(i2) +(y-y_ray_2(i2))
     1                 *py_ray_2(i2) + (z-z_ray_2(i2))*pz_ray_2(i2)
                t3_2 = (x-x_ray_2(i3))*px_ray_2(i3) +(y-y_ray_2(i3))
     1                 *py_ray_2(i3) + (z-z_ray_2(i3))*pz_ray_2(i3)

                in_flag = 0
                if (t1_1 .ge. -t_eps .and. t1_2 .le. t_eps) 
     1              in_flag = in_flag +1
                if (t2_1 .ge. -t_eps .and. t2_2 .le. t_eps) 
     1              in_flag = in_flag +1
                if (t3_1 .ge. -t_eps .and. t3_2 .le. t_eps) 
     1              in_flag = in_flag +1
                if (in_flag .le. 0) goto 2

                call ktime_3d_raytrace_intube_4(
     1 2,x,y,z
     1,i1,i2,i3
     1,x_ray_2,y_ray_2,z_ray_2
     1,norm_x2(i_tube),norm_y2(i_tube),norm_z2(i_tube)
     1,px_ray_2(i1),py_ray_2(i1),pz_ray_2(i1)
     1,lambda_2,u_2,v_2
     1,i_flag_2)
                if (i_flag_2 .lt. 0 ) goto 2

                call ktime_3d_raytrace_intube_4(
     1 1,x,y,z
     1,i1,i2,i3
     1,x_ray_1,y_ray_1,z_ray_1
     1,norm_x1(i_tube),norm_y1(i_tube),norm_z1(i_tube)
     1,px_ray_1(i1),py_ray_1(i1),pz_ray_1(i1)
     1,lambda_1,u_1,v_1
     1,i_flag_1)
                if (i_flag_1 .lt. 0 ) goto 2

                if ( u_2 .ge. -uv_eps .and. u_2 .le. 1+uv_eps .and. 
     1	v_2 .ge. -uv_eps .and. v_2 .le. 1+uv_eps  
     1  .and. u_2+v_2 .le. 1+uv_eps
     1  .or.  u_1 .ge. -uv_eps .and. u_1 .le. 1+uv_eps .and. 
     1  v_1 .ge. -uv_eps .and. v_1 .le. 1+uv_eps .and. 
     1  u_1+v_1 .le. 1+uv_eps ) then
  
                     timet=(t1_1+t2_1+t3_1+t1_2+t2_2+t3_2)/6
     1                    +(t_ray_1+t_ray_2)*0.5 

                     t1 = max(t_eps,t1_1 - t1_2)
                     t2 = max(t_eps,t2_1 - t2_2)
                     t3 = max(t_eps,t3_1 - t3_2)

                 s_path = ((s_ray_1(i1)*(1-u_1)
     1            + s_ray_1(i2)*u_1)*(1-v_1)
     1            + s_ray_1(i3)*v_1)*0.5
     1            + ((s_ray_2(i1)*(1-u_2)
     1            + s_ray_2(i2)*u_2)*(1-v_2)
     1            + s_ray_2(i3)*v_2)*0.5

                 q11 = ((qq_ray_1(1,i1)*(1-u_1)
     1            + qq_ray_1(1,i2)*u_1)*(1-v_1)
     1            + qq_ray_1(1,i3)*v_1)*0.5
     1            + ((qq_ray_2(1,i1)*(1-u_2)
     1            + qq_ray_2(1,i2)*u_2)*(1-v_2)
     1            + qq_ray_2(1,i3)*v_2)*0.5

                 q12 = ((qq_ray_1(2,i1)*(1-u_1)
     1            + qq_ray_1(2,i2)*u_1)*(1-v_1)
     1            + qq_ray_1(2,i3)*v_1)*0.5
     1            + ((qq_ray_2(2,i1)*(1-u_2)
     1            + qq_ray_2(2,i2)*u_2)*(1-v_2)
     1            + qq_ray_2(2,i3)*v_2)*0.5

                 q21 = ((qq_ray_1(3,i1)*(1-u_1)
     1            + qq_ray_1(3,i2)*u_1)*(1-v_1)
     1            + qq_ray_1(3,i3)*v_1)*0.5
     1            + ((qq_ray_2(3,i1)*(1-u_2)
     1            + qq_ray_2(3,i2)*u_2)*(1-v_2)
     1            + qq_ray_2(3,i3)*v_2)*0.5

                 q22 = ((qq_ray_1(4,i1)*(1-u_1)
     1            + qq_ray_1(4,i2)*u_1)*(1-v_1)
     1            + qq_ray_1(4,i3)*v_1)*0.5
     1            + ((qq_ray_2(4,i1)*(1-u_2)
     1            + qq_ray_2(4,i2)*u_2)*(1-v_2)
     1            + qq_ray_2(4,i3)*v_2)*0.5

                     amp1 = q11*q22-q12*q21
                     q11i = q22/amp1
                     q12i = -q12/amp1
                     q21i = -q21/amp1
                     q22i = q11/amp1
		     amp2 = amp0/sqrt(abs(amp1))

                 p3_t = ((pz_ray_1(i1)*v_ray_1(i1)*(1-u_1)
     1            + pz_ray_1(i2)*v_ray_1(i2)*u_1)*(1-v_1)
     1            + pz_ray_1(i3)*v_ray_1(i3)*v_1)*0.5
     1            + ((pz_ray_2(i1)*v_ray_2(i1)*(1-u_2)
     1            + pz_ray_2(i2)*v_ray_2(i2)*u_2)*(1-v_2)
     1            + pz_ray_2(i3)*v_ray_2(i3)*v_2)*0.5
 
                 if (p3_t .lt. -1.0) p3_t =-1.0
        	 if (p3_t .gt. 1.0) p3_t =1.0
                 a1_t = acos(p3_t)
 
                 px_t = ((px_ray_1(i1)*v_ray_1(i1)*(1-u_1)
     1            + px_ray_1(i2)*v_ray_1(i2)*u_1)*(1-v_1)
     1            + px_ray_1(i3)*v_ray_1(i3)*v_1)*0.5
     1            + ((px_ray_2(i1)*v_ray_2(i1)*(1-u_2)
     1            + px_ray_2(i2)*v_ray_2(i2)*u_2)*(1-v_2)
     1            + px_ray_2(i3)*v_ray_2(i3)*v_2)*0.5

                 py_t = ((py_ray_1(i1)*v_ray_1(i1)*(1-u_1)
     1            + py_ray_1(i2)*v_ray_1(i2)*u_1)*(1-v_1)
     1            + py_ray_1(i3)*v_ray_1(i3)*v_1)*0.5
     1            + ((py_ray_2(i1)*v_ray_2(i1)*(1-u_2)
     1            + py_ray_2(i2)*v_ray_2(i2)*u_2)*(1-v_2)
     1            + py_ray_2(i3)*v_ray_2(i3)*v_2)*0.5
 
   	         temp = max(0.001,sqrt(1-p3_t*p3_t))
                 px_t = px_t/temp
                 py_t = py_t/temp
 
                 if (px_t .lt. -1.0) px_t =-1.0
 	         if (px_t .gt. 1.0) px_t =1.0
                 if (py_t .lt. -1.0) py_t =-1.0
 	         if (py_t .gt. 1.0) py_t =1.0
                 
 	         if (px_t .ge. 0.0) then
 	            b1_t = asin(py_t)
 	            if (b1_t .lt. 0.0) b1_t=b1_t+2.0*3.1415926
 	         else
 	            b1_t = 3.1415926 - asin(py_t)
 	         endif !if (px_t .ge. 0.0) then

                 tc = ((tc_ray_1(i1)*(1-u_1)
     1            + tc_ray_1(i2)*u_1)*(1-v_1)
     1            + tc_ray_1(i3)*v_1)*0.5
     1            + ((tc_ray_2(i1)*(1-u_2)
     1            + tc_ray_2(i2)*u_2)*(1-v_2)
     1            + tc_ray_2(i3)*v_2)*0.5

                 a0_t = ((a_ray(i1)*(1-u_1)
     1            + a_ray(i2)*u_1)*(1-v_1)
     1            + a_ray(i3)*v_1)*0.5
     1            + ((a_ray(i1)*(1-u_2)
     1            + a_ray(i2)*u_2)*(1-v_2)
     1            + a_ray(i3)*v_2)*0.5
 
                 b0_t = ((b_ray(i1)*(1-u_1)
     1            + b_ray(i2)*u_1)*(1-v_1)
     1            + b_ray(i3)*v_1)*0.5
     1            + ((b_ray(i1)*(1-u_2)
     1            + b_ray(i2)*u_2)*(1-v_2)
     1            + b_ray(i3)*v_2)*0.5
 
                 if (abs(y) .le. 0.0001  .and. x .ge. 0.0) b0_t=0.0
 
                 e1x0 = cos(a0_t)*cos(b0_t)
                 e1y0 = cos(a0_t)*sin(b0_t)
                 e2x0 = -sin(b0_t)
                 e2y0 = cos(b0_t)
 	 
                 e1x = ((e1x_ray_1(i1)*(1-u_1)
     1            + e1x_ray_1(i2)*u_1)*(1-v_1)
     1            + e1x_ray_1(i3)*v_1)*0.5
     1            + ((e1x_ray_2(i1)*(1-u_2)
     1            + e1x_ray_2(i2)*u_2)*(1-v_2)
     1            + e1x_ray_2(i3)*v_2)*0.5
 
                 e1y = ((e1y_ray_1(i1)*(1-u_1)
     1            + e1y_ray_1(i2)*u_1)*(1-v_1)
     1            + e1y_ray_1(i3)*v_1)*0.5
     1            + ((e1y_ray_2(i1)*(1-u_2)
     1            + e1y_ray_2(i2)*u_2)*(1-v_2)
     1            + e1y_ray_2(i3)*v_2)*0.5
 
                 e1z = ((e1z_ray_1(i1)*(1-u_1)
     1            + e1z_ray_1(i2)*u_1)*(1-v_1)
     1            + e1z_ray_1(i3)*v_1)*0.5
     1            + ((e1z_ray_2(i1)*(1-u_2)
     1            + e1z_ray_2(i2)*u_2)*(1-v_2)
     1            + e1z_ray_2(i3)*v_2)*0.5
 
                 e2x = ((e2x_ray_1(i1)*(1-u_1)
     1            + e2x_ray_1(i2)*u_1)*(1-v_1)
     1            + e2x_ray_1(i3)*v_1)*0.5
     1            + ((e2x_ray_2(i1)*(1-u_2)
     1            + e2x_ray_2(i2)*u_2)*(1-v_2)
     1            + e2x_ray_2(i3)*v_2)*0.5
 
                 e2y = ((e2y_ray_1(i1)*(1-u_1)
     1            + e2y_ray_1(i2)*u_1)*(1-v_1)
     1            + e2y_ray_1(i3)*v_1)*0.5
     1            + ((e2y_ray_2(i1)*(1-u_2)
     1            + e2y_ray_2(i2)*u_2)*(1-v_2)
     1            + e2y_ray_2(i3)*v_2)*0.5
 
                 e2z = ((e2z_ray_1(i1)*(1-u_1)
     1            + e2z_ray_1(i2)*u_1)*(1-v_1)
     1            + e2z_ray_1(i3)*v_1)*0.5
     1            + ((e2z_ray_2(i1)*(1-u_2)
     1            + e2z_ray_2(i2)*u_2)*(1-v_2)
     1            + e2z_ray_2(i3)*v_2)*0.5
 
                 tmp11 = q11i*e1x + q21i*e2x
 	         tmp12 = q11i*e1y + q21i*e2y
 	         tmp13 = q11i*e1z + q21i*e2z
 
                 tmp21 = q12i*e1x + q22i*e2x
 	         tmp22 = q12i*e1y + q22i*e2y
 	         tmp23 = q12i*e1z + q22i*e2z
 
                 det21 = e1x0*tmp11 + e2x0*tmp21
                 det22 = e1x0*tmp12 + e2x0*tmp22
                 det23 = e1x0*tmp13 + e2x0*tmp23
 
 	 	 det31 = e1y0*tmp11 + e2y0*tmp21
                 det32 = e1y0*tmp12 + e2y0*tmp22
                 det33 = e1y0*tmp13 + e2y0*tmp23

c                phase = ((phase_ray_1(i1)*(1-u_1)
c    1            + phase_ray_1(i2)*u_1)*(1-v_1)
c    1            + phase_ray_1(i3)*v_1)*0.5
c    1            + ((phase_ray_2(i1)*(1-u_2)
c    1            + phase_ray_2(i2)*u_2)*(1-v_2)
c    1            + phase_ray_2(i3)*v_2)*0.5
                 phase = max(phase_ray_1(i1), phase_ray_1(i2),
     1             phase_ray_1(i3), phase_ray_2(i1),
     1             phase_ray_2(i2), phase_ray_2(i3) )
 
                     if (t_tab(i_xyz) .le. 0.0 ) then
 
                       t_tab(i_xyz) = timet
                       amp_tab(i_xyz) = amp2
                       phase_tab(i_xyz) = phase
                       s_tab(i_xyz) = s_path
                       vmax_tab(i_xyz) = vmax
                      a_tab(i_xyz) = a1_t
                      b_tab(i_xyz) = b1_t
                       q11_tab(i_xyz) = det21
                       q12_tab(i_xyz) = det22
                       q21_tab(i_xyz) = det23
                       q22_tab(i_xyz) = det31
 	               p3_tab(i_xyz) = p3_t
 	               e2z_tab(i_xyz) = det33
		       det33_tab(i_xyz) = det32
                       points(i_xyz) = points(i_xyz)+ 1
                       nt_num = nt_num +1

		     elseif (inter_type .eq. 2 .and. 
     1                     t_tab(i_xyz) .gt. timet) then

                       t_tab(i_xyz) = timet
                       amp_tab(i_xyz) = amp2
                       phase_tab(i_xyz) = phase
                       s_tab(i_xyz) = s_path
                       vmax_tab(i_xyz) = vmax
                      a_tab(i_xyz) = a1_t
                      b_tab(i_xyz) = b1_t
                       q11_tab(i_xyz) = det21
                       q12_tab(i_xyz) = det22
                       q21_tab(i_xyz) = det23
                       q22_tab(i_xyz) = det31
 	               p3_tab(i_xyz) = p3_t
 	               e2z_tab(i_xyz) = det33
		       det33_tab(i_xyz) = det32
                       points(i_xyz) = points(i_xyz)+ 1
                       nt_num = nt_num +1

                     elseif (inter_type .eq. 1 .and. 
     1                     amp_tab(i_xyz) .lt. amp) then

                       t_tab(i_xyz) = timet
                       amp_tab(i_xyz) = amp2
                       phase_tab(i_xyz) = phase
                       s_tab(i_xyz) = s_path
                       vmax_tab(i_xyz) = vmax
                      a_tab(i_xyz) = a1_t
                      b_tab(i_xyz) = b1_t
                       q11_tab(i_xyz) = det21
                       q12_tab(i_xyz) = det22
                       q21_tab(i_xyz) = det23
                       q22_tab(i_xyz) = det31
 	               p3_tab(i_xyz) = p3_t
 	               e2z_tab(i_xyz) = det33
		       det33_tab(i_xyz) = det32
                       points(i_xyz) = points(i_xyz)+ 1
                       nt_num = nt_num +1

                     elseif (inter_type .eq. 3 .and. 
     1                     s_tab(i_xyz) .gt. s_path) then

		       t_tab(i_xyz) = timet
                       amp_tab(i_xyz) = amp2
                       phase_tab(i_xyz) = phase
                       s_tab(i_xyz) = s_path
                       vmax_tab(i_xyz) = vmax
                      a_tab(i_xyz) = a1_t
                      b_tab(i_xyz) = b1_t
                       q11_tab(i_xyz) = det21
                       q12_tab(i_xyz) = det22
                       q21_tab(i_xyz) = det23
                       q22_tab(i_xyz) = det31
 	               p3_tab(i_xyz) = p3_t
 	               e2z_tab(i_xyz) = det33
		       det33_tab(i_xyz) = det32
                       points(i_xyz) = points(i_xyz)+ 1
                       nt_num = nt_num +1
 
		     elseif (inter_type .eq. 4 .and. 
     1                     vmax_tab(i_xyz) .gt. vmax) then
 
                       t_tab(i_xyz) = timet
                       amp_tab(i_xyz) = amp2
                       phase_tab(i_xyz) = phase
                       s_tab(i_xyz) = s_path
                       vmax_tab(i_xyz) = vmax
                      a_tab(i_xyz) = a1_t
                      b_tab(i_xyz) = b1_t
                       q11_tab(i_xyz) = det21
                       q12_tab(i_xyz) = det22
                       q21_tab(i_xyz) = det23
                       q22_tab(i_xyz) = det31
 	               p3_tab(i_xyz) = p3_t
 	               e2z_tab(i_xyz) = det33
		       det33_tab(i_xyz) = det32
                       points(i_xyz) = points(i_xyz)+ 1
                       nt_num = nt_num +1
 
                     elseif (inter_type .eq. 5 .and.
     1                     t_tab(i_xyz) .lt. timet) then
 
                       t_tab(i_xyz) = timet
                       amp_tab(i_xyz) = amp2
                       phase_tab(i_xyz) = phase
                       s_tab(i_xyz) = s_path
                       vmax_tab(i_xyz) = vmax
                      a_tab(i_xyz) = a1_t
                      b_tab(i_xyz) = b1_t
                       q11_tab(i_xyz) = det21
                       q12_tab(i_xyz) = det22
                       q21_tab(i_xyz) = det23
                       q22_tab(i_xyz) = det31
 	               p3_tab(i_xyz) = p3_t
 	               e2z_tab(i_xyz) = det33
		       det33_tab(i_xyz) = det32
                       points(i_xyz) = points(i_xyz)+ 1
                       nt_num = nt_num +1
 
                     elseif (inter_type .eq. 6 .and.
     1                     s_tab(i_xyz) .lt. s_path) then
 
                       t_tab(i_xyz) = timet
                       amp_tab(i_xyz) = amp2
                       phase_tab(i_xyz) = phase
                       s_tab(i_xyz) = s_path
                       vmax_tab(i_xyz) = vmax
                      a_tab(i_xyz) = a1_t
                      b_tab(i_xyz) = b1_t
                       q11_tab(i_xyz) = det21
                       q12_tab(i_xyz) = det22
                       q21_tab(i_xyz) = det23
                       q22_tab(i_xyz) = det31
 	               p3_tab(i_xyz) = p3_t
 	               e2z_tab(i_xyz) = det33
		       det33_tab(i_xyz) = det32
                       points(i_xyz) = points(i_xyz)+ 1
                       nt_num = nt_num +1
 
                     endif! if (t_tab(i_xyz) .eq. 0.0 ) then

                  endif !if (u_2 .ge. -0.05 .and. v_2 .ge. -0.05
 
    2  continue
               enddo ! do iz = izmin, izmax
             enddo ! do ix = ixmin, ixmax
           enddo !do iy = iymin, iymax
 
        endif ! if (ixmin .lt. ixmax .and. iymin .lt. iymax
 
      enddo ! do i_bound = 1, n_intube

      return
      end
c23456789012345678901234567890123456789012345678901234567890123456789012
      subroutine ktime_3d_raytrace_init_ab_1(lu_out
     1,xs,ys,zs
     1,ds_ray,r_ray,vs,t0_ray
     1,a0_ray,a1_ray
     1,b0_ray,b1_ray
     1,na_ray,nb_ray
     1,m_ray,n_ray,a_ray,b_ray,s_ray,x_ray,y_ray,z_ray
     1,px_ray,py_ray,pz_ray
     1,tx_ray,ty_ray,tz_ray
     1,e1x_ray,e1y_ray,e1z_ray
     1,e2x_ray,e2y_ray,e2z_ray
     1,qq_ray,hq_ray
     1,m_tube,n_tube,ir_tube,is_tube,it_tube,it_flag
     1,i_err)
c  compute an inital list of rays a,b 
c  and pointers to the associated ray tubes
      implicit none

      real     xs,ys,zs

      real     util_rd,util_dr

      integer  lu_out
      real     ds_ray,r_ray,vs,t0_ray

      real     a0_ray,a1_ray
      real     b0_ray,b1_ray

      integer  m_ray,n_ray
      real     a_ray(m_ray)
      real     b_ray(m_ray)
      real     s_ray(m_ray)
      real     x_ray(m_ray)
      real     y_ray(m_ray)
      real     z_ray(m_ray)
      real     px_ray(m_ray)
      real     py_ray(m_ray)
      real     pz_ray(m_ray)
      real     tx_ray(m_ray),ty_ray(m_ray),tz_ray(m_ray)
      real     e1x_ray(m_ray),e1y_ray(m_ray),e1z_ray(m_ray)
      real     e2x_ray(m_ray),e2y_ray(m_ray),e2z_ray(m_ray)
      real    qq_ray(4,m_ray),hq_ray(4,m_ray)

      integer  m_tube,n_tube
      integer  ir_tube(3,m_tube)
      integer  is_tube(3,m_tube)
      integer  it_tube(3,m_tube)
      integer  it_flag(m_tube)

      integer  i_err

      integer  na_ray
      integer  ia_ray
      real     da_ray,a_ray_0,a_eps

      integer  nb_ray,ib_ray,mb_ray
      integer  nb_ray_1
      real     db_ray,d_db_ray,b_ray_0
      real     ca,cb,sa,sb

      real     r_ray_0

      integer  n_ray_0,n_ray_1
      integer  i_tube,i_tube_1,i_tube_2
      integer  n_tube_0,n_tube_1
      data     a_eps/1.e-6/
      i_err = 0

      r_ray_0 = vs * 0.0005

c  arnitrarily redu e the number of a's
      a0_ray = util_dr( a0_ray)
      a1_ray = util_dr( a1_ray)
      da_ray = (a1_ray - a0_ray) / max(1,na_ray-1)

      b0_ray = util_dr(   0.)
      b1_ray = util_dr(b1_ray)
      db_ray = (b1_ray - b0_ray) / max(1,nb_ray)

      mb_ray = 2**12 + 1

      n_ray = 0
      n_tube = 0
      n_tube_0 = 0
      n_tube_1 = 0
      n_ray_0 = 0
      n_ray_1 = 0
 
      do ia_ray = 1 , na_ray

        a_ray_0 = (ia_ray - 1) * da_ray + a0_ray

        if (abs(sin(a_ray_0)) .le. a_eps) then

          nb_ray_1 = 1

        else    ! if (abs(a_ray_0) .lt. a_eps) then

          nb_ray_1 = nb_ray

        endif    ! if (abs(a_ray_0) .lt. a_eps) then

c  check against the maximum number of angles
        if (n_ray+nb_ray_1 .gt. m_ray) goto 998

        n_ray_0 = n_ray_1     ! pointer to start of previous a shell
        n_ray_1 = n_ray       ! pointer to start of current  a shell


c  add a,b x,y,z px,py,pz values in this a shell
        do ib_ray = 1 , nb_ray_1

          d_db_ray = (ia_ray-1)*0.5*db_ray
          b_ray_0 = (ib_ray - 1) * db_ray + b0_ray - d_db_ray

          n_ray = n_ray +1

          a_ray(n_ray) = a_ray_0
          b_ray(n_ray) = b_ray_0

          ca = cos(a_ray_0)
	  cb = cos(b_ray_0)
	  sa = sin(a_ray_0)
	  sb = sin(b_ray_0)

          x_ray(n_ray) = r_ray_0 * sa * cb + xs
          y_ray(n_ray) = r_ray_0 * sa * sb + ys
          z_ray(n_ray) = r_ray_0 * ca +zs

          tx_ray(n_ray) = sa * cb  ! tx
          ty_ray(n_ray) = sa * sb  ! ty
          tz_ray(n_ray) = ca       ! tz

	  px_ray(n_ray) = tx_ray(n_ray) / vs ! px
          py_ray(n_ray) = ty_ray(n_ray) / vs ! py
          pz_ray(n_ray) = tz_ray(n_ray) / vs ! pz
 
          e1x_ray(n_ray) = ca * cb
	  e1y_ray(n_ray) = ca * sb
	  e1z_ray(n_ray) = -sa

          e2x_ray(n_ray) = -sb
	  e2y_ray(n_ray) = cb
	  e2z_ray(n_ray) = 0

          s_ray(n_ray) = r_ray_0

        enddo    ! do ib_ray = 1 , nb_ray

c  compute ir_tube,it_tube and is_tube

        n_tube_0 = n_tube_1
        n_tube_1 = n_tube

        if (ia_ray .eq. 2) then
           
           do i_tube = 1, nb_ray_1

              n_tube =n_tube +1

              if (i_tube .eq. 1) then 
                  i_tube_1= nb_ray_1
              else 
                  i_tube_1= n_tube -1
              endif 

              ir_tube(1,n_tube) = 1 
              ir_tube(2,n_tube) = 1 + i_tube 
              ir_tube(3,n_tube) = 2 + mod(i_tube,nb_ray_1)

              it_tube(1,n_tube) = i_tube_1 
              it_tube(3,n_tube) = 1 + mod(n_tube,nb_ray_1)

              is_tube(1,n_tube) = 3
              is_tube(3,n_tube) = 1 
           
          enddo !  do i_tube = 1, nb_ray_1

        elseif (ia_ray .gt. 2) then

          do i_tube = 1, nb_ray_1

              n_tube =n_tube +1

              if (i_tube .eq. 1) then 
                  i_tube_1= n_tube_1
              else 
                  i_tube_1= n_tube_1 - nb_ray_1 -1 + i_tube
              endif 

              ir_tube(1,n_tube) = ir_tube(2,i_tube_1)
              ir_tube(2,n_tube) = n_ray_1 + i_tube
              ir_tube(3,n_tube) = ir_tube(3,i_tube_1)

              it_tube(3,n_tube) = i_tube_1
              it_tube(2,i_tube_1) = n_tube

              is_tube(3,n_tube) = 2
              is_tube(2,i_tube_1) = 3 
           
          enddo !  do i_tube = 1, nb_ray_1

          do i_tube = 1, nb_ray_1

              n_tube =n_tube +1

              i_tube_1 = n_tube_1 + i_tube
              i_tube_2= mod(i_tube,nb_ray_1) +1 +n_tube_1

              ir_tube(1,n_tube) = ir_tube(3, i_tube_1)
              ir_tube(2,n_tube) = ir_tube(2, i_tube_1)
              ir_tube(3,n_tube) = ir_tube(2, i_tube_2)

              it_tube(1,n_tube) = i_tube_1
              it_tube(3,n_tube) = i_tube_2
              it_tube(1,i_tube_2) = n_tube
              it_tube(2,i_tube_1) = n_tube

              is_tube(1,n_tube) = 2
              is_tube(3,n_tube) = 1
              is_tube(2,i_tube_1) = 1
              is_tube(1,i_tube_2) = 3
           
          enddo !  do i_tube = 1, nb_ray_1

        endif !  if (ia_ray .eq. 2) then

      enddo    ! do ia_ray = 1 , na_ray

c     print'(/,'' in routine ktime_3d_raytrace_init_ab''
c    1,/,'' lu_out='',i8
c    1,/,'' ds_ray='',f10.2,'' r_ray='',f10.2,'' r_ray_0='',f10.2
c    1,/,'' na_ray='',i6,'' a0_ray='',f8.4,'' a1_ray='',f8.4
c    1,'' da_ray='',f8.4
c    1,/,'' nb_ray='',i6,'' b0_ray='',f8.4,'' b1_ray='',f8.4
c    1,'' db_ray='',f8.4
c    1,/,'' m_ray='',i8,'' n_ray='',i8
c    1,/,'' m_tube='',i8,'' n_tube='',i8
c    1)'
c    1,lu_out
c    1,ds_ray,r_ray,r_ray_0
c    1,na_ray,util_rd(a0_ray),util_rd(a1_ray),util_rd(da_ray)
c    1,nb_ray,util_rd(b0_ray),util_rd(b1_ray),util_rd(db_ray)
c    1,m_ray,n_ray
c    1,m_tube,n_tube
  
	call util_seti(n_tube,it_flag,1)
  	
c  check the tube connections for consistency
c     call ktime_3d_raytrace_check_tcon(lu_out
c    1,m_ray,n_ray,a_ray,b_ray,x_ray,y_ray,z_ray,px_ray,py_ray,pz_ray
c    1,m_tube,n_tube,ir_tube,is_tube,it_tube,it_flag
c    1,i_err)
c     if (i_err .ne. 0) goto 999

c  check the tube directions to make sure the obey the right hand rule
c     call ktime_3d_raytrace_check_tdir(lu_out
c    1,m_ray,n_ray,a_ray,b_ray,x_ray,y_ray,z_ray,px_ray,py_ray,pz_ray
c    1,m_tube,n_tube,ir_tube,is_tube,it_tube,it_flag
c    1,i_err)
c     if (i_err .ne. 0) goto 999
      return

  998 continue
      print'(/,'' error in ktime_3d_raytrace_init_ab''
     1,/,'' not enough memory for rays m_ray='',i8,'' n_ray='',i8)'
     1,m_ray,n_ray
      goto 999

  999 continue
      print'(/,'' error in ktime_3d_raytrace_init_ab'')'
      i_err = -1
      return

      end

c23456789012345678901234567890123456789012345678901234567890123456789012
      subroutine ktime_write_outputs(xs,ys,zs
     1,ix_tab,nx_tab,x0_tab,dx_tab
     1       ,ny_tab,y0_tab,dy_tab
     1,iz_tab,nz_tab,z0_tab,dz_tab
     1,t_tab,m_xyz_tab,amp_tab,a_tab,b_tab
     1,i_err
     1)

      implicit none
      real     xs,ys,zs
 
      integer  ix_tab(1),nx_tab(1)
      real     x0_tab(1),dx_tab
 
      integer            ny_tab
      real     y0_tab,dy_tab
 
      integer  iz_tab(1),nz_tab(1)
      real     z0_tab(1),dz_tab
 
      real     t_tab(1)

      integer  m_xyz_tab
      real     amp_tab(m_xyz_tab),a_tab(m_xyz_tab)
      real     b_tab(m_xyz_tab)
 
      integer  i,i_xyz,i_xy,iy1
      integer  ix,iy,iz,ix1,i_err,i_xyz_1,i_xyz_2
      real     x,y,z,diff1,diff2

      iy1 = ny_tab/2 +1
      ix1 = nx_tab(1)/2 +1
      i=0
      do iy=1, ny_tab
 
        y =  y0_tab + (iy-1)*dy_tab
 
        do ix= 1, nx_tab(iy)
 
          x=  x0_tab(iy) + (ix-1)*dx_tab

          i_xy = ix_tab(iy) + ix
 
          do iz = 2,nz_tab(i_xy)
 
            z =  z0_tab(i_xy) + (iz-1)*dz_tab
 
            i_xyz = iz_tab(i_xy) + iz
 
            if (iy .eq. iy1) then

            if (ix .eq. ix1 .and. iy .eq. iy1) then

               i_xyz_1 = (iy1-1)*nx_tab(1)*nz_tab(1)
     1                 +(ix1-2)*nz_tab(1) +iz

               i_xyz_2 = (iy1-1)*nx_tab(1)*nz_tab(1)
     1                 + ix1*nz_tab(1) +iz

               write(67,*) (amp_tab(i_xyz_1)+amp_tab(i_xyz_2))/2.0

            elseif (iz .gt. 1 .and. iz .lt. nz_tab(1) ) then

               diff1= abs(amp_tab(i_xyz)-amp_tab(i_xyz-1))
     1                   /amp_tab(i_xyz-1)
               diff2= abs(amp_tab(i_xyz)-amp_tab(i_xyz+1))
     1                  /amp_tab(i_xyz+1)
               if(diff1 .ge. 0.05 .and. diff2 .ge. 0.05) then
                 write(67,*) (amp_tab(i_xyz-1)+amp_tab(i_xyz+1))/2.0
               else
                   write(67,*) amp_tab(i_xyz)
               endif

            else

               write(67,*) amp_tab(i_xyz)
            endif! if (ix .eq. ix1 ) then
  
           endif !if (iy .eq. iy1) then 

         enddo ! do j = 1,nz_tab(i_xy)
 
        enddo ! do i= 1, nx_tab
 
      enddo ! do iy=1, ny_tab
 
      print'('' iy1='',i4)',iy1
      return

      end


c23456789012345678901234567890123456789012345678901234567890123456789012
      subroutine ktime_3d_write_wavefront(lu_out,t_ray
     1,m_ray,n_ray
     1,xs,ys,zs
     1,x_ray,y_ray,z_ray)

c  write x,y,z for a single ray tube
c if i_flag .ne. 0 add vectors which point towards the center of the tube
c  and increase in length with each side
      implicit none
 
      integer  lu_out
      real     t_ray
 
      integer  m_ray,n_ray
      real     x_ray(n_ray)
      real     y_ray(n_ray)
      real     z_ray(n_ray)
      real     xs,ys,zs 

      integer  i_ray,i
 
c Create output file and open as old
      OPEN(12,FILE='ct20',STATUS='unknown')
      CLOSE(12)
      OPEN(12,FILE='ct20',STATUS='OLD')

      i=0
      do i_ray = 1, n_ray

        if (z_ray(i_ray) .ge. 0.0)  then
        i=i+1
        write(12,'(''PVRTX'',1x,i6,1x,f10.2
     1,1x,f10.2,1x,f10.2,1x,f10.6)')
     1 i,x_ray(i_ray),y_ray(i_ray),z_ray(i_ray)
     1,t_ray
       endif

      enddo 

      return
      end

c23456789012345678901234567890123456789012345678901234567890123456789012
      subroutine ktime_3d_add_1_ray_2(
     1 n_ray,n_add,ia_rays
     1,x_ray,y_ray,z_ray,s_ray
     1,px_ray,py_ray,pz_ray,tc_ray
     1,tx_ray,ty_ray,tz_ray
     1,e1x_ray,e1y_ray,e1z_ray
     1,e2x_ray,e2y_ray,e2z_ray
     1,qq_ray,hq_ray,gs_ray,phase_ray
     1,v_ray
     1,vq11_ray,vq12_ray,vq22_ray
     1,vx_ray,vy_ray,vz_ray
     1,vmax_ray
     1,nx_vel,x0_vel,dx_vel
     1,ny_vel,y0_vel,dy_vel
     1,nz_vel,z0_vel,dz_vel
     1,vcof
     1,m_work,work
     1,i_err
     1)
c  interpolate ray parameters for the adding rays.
c  ray position is interpolated along the wavefront.
c  amplitude quantities are linearly interpolated.
      implicit  none

      integer   n_ray
      integer   ray1,ray2,ray3
      integer   n_add,ia_rays(3,n_add),i_add

      real      x_ray(n_ray),y_ray(n_ray),z_ray(n_ray)
      real      s_ray(n_ray),tc_ray(n_ray)
      real      px_ray(n_ray),py_ray(n_ray),pz_ray(n_ray)
      real     tx_ray(n_ray),ty_ray(n_ray),tz_ray(n_ray)
      real     e1x_ray(n_ray),e1y_ray(n_ray),e1z_ray(n_ray)
      real     e2x_ray(n_ray),e2y_ray(n_ray),e2z_ray(n_ray)
      real      qq_ray(4,n_ray),hq_ray(4,n_ray),gs_ray(n_ray)
      integer   phase_ray(n_ray)

      real      v_ray(n_ray)
      real      vq11_ray(n_ray),vq12_ray(n_ray),vq22_ray(n_ray)
      real      vx_ray(n_ray),vy_ray(n_ray),vz_ray(n_ray)
      real      vmax_ray(n_ray)

      integer   nx_vel
      real      x0_vel,dx_vel

      integer   ny_vel
      real      y0_vel,dy_vel

      integer   nz_vel
      real      z0_vel,dz_vel

      real      vcof(nz_vel,nx_vel,ny_vel,10)

      integer   m_work
      real      work(m_work)

      integer   i_err

      real      px,py,pz

      real      norm3

      real      v_ray1,v_ray2
      real      sina,cosa,deltas,s3,dd

      real      tx,ty,tz,e1x,e1y,e1z,e2x,e2y,e2z

      real      phase_tmp

      real      tmp1,tmp2,tmp3,tmp4
      real      eps
      data      eps/1.e-6/

      do i_add = 1,n_add

         ray1 = ia_rays(1,i_add)
         ray2 = ia_rays(2,i_add)
         ray3 = ia_rays(3,i_add)
         v_ray1 = v_ray(ray1)
         v_ray2 = v_ray(ray2)

         x_ray(ray3) = (x_ray(ray1) + x_ray(ray2))/2.0
         y_ray(ray3) = (y_ray(ray1) + y_ray(ray2))/2.0
         z_ray(ray3) = (z_ray(ray1) + z_ray(ray2))/2.0

         dd = sqrt( (x_ray(ray1)-x_ray(ray3))*(x_ray(ray1)-x_ray(ray3))
     1           +  (y_ray(ray1)-y_ray(ray3))*(y_ray(ray1)-y_ray(ray3))
     1           +  (z_ray(ray1)-z_ray(ray3))*(z_ray(ray1)-z_ray(ray3)))

         s3 = (s_ray(ray1) +s_ray(ray2))/2.0
         s_ray(ray3) = s3
         sina = min(1.0, dd/s3)
         cosa = sqrt(1.0 - sina*sina)
         deltas = s_ray(ray3)*(1.0 -cosa)

         px = px_ray(ray1) + px_ray(ray2)
         py = py_ray(ray1) + py_ray(ray2)
         pz = pz_ray(ray1) + pz_ray(ray2)
         norm3 = max(eps, sqrt(px*px + py*py + pz*pz))
         px = px/norm3
         py = py/norm3
         pz = pz/norm3
         vmax_ray(ray3) = (vmax_ray(ray1) + vmax_ray(ray2))*0.5

         x_ray(ray3) = x_ray(ray3) + px*deltas
         y_ray(ray3) = y_ray(ray3) + py*deltas
         z_ray(ray3) = z_ray(ray3) + pz*deltas

	 tc_ray(ray3) = (tc_ray(ray1) + tc_ray(ray2))/2.0

         tx = tx_ray(ray1) + tx_ray(ray2)
         ty = ty_ray(ray1) + ty_ray(ray2)
         tz = tz_ray(ray1) + tz_ray(ray2)
         norm3 = max(eps, sqrt(tx*tx + ty*ty + tz*tz))
         tx_ray(ray3) = tx/norm3
         ty_ray(ray3) = ty/norm3
         tz_ray(ray3) = tz/norm3

	 e1x = e1x_ray(ray1) + e1x_ray(ray2)
         e1y = e1y_ray(ray1) + e1y_ray(ray2)
         e1z = e1z_ray(ray1) + e1z_ray(ray2)
         norm3 = max(eps, sqrt(e1x*e1x + e1y*e1y + e1z*e1z))
         e1x_ray(ray3) = e1x/norm3
         e1y_ray(ray3) = e1y/norm3
         e1z_ray(ray3) = e1z/norm3

	e2x_ray(ray3) = ty_ray(ray3)*e1z_ray(ray3) 
     1	- tz_ray(ray3)*e1y_ray(ray3) 
	e2y_ray(ray3) = tz_ray(ray3)*e1x_ray(ray3) 
     1	- tx_ray(ray3)*e1z_ray(ray3) 
	e2z_ray(ray3) = tx_ray(ray3)*e1y_ray(ray3) 
     1	- ty_ray(ray3)*e1x_ray(ray3) 

         tmp2 = e1x_ray(ray3)*tx_ray(ray3) + e1y_ray(ray3)
     1  *ty_ray(ray3)+ e1z_ray(ray3)*tz_ray(ray3) 

        if (abs(tmp2) .ge. 0.01 ) then
        
	  e1x_ray(ray3) = e2y_ray(ray3)*tz_ray(ray3)
     1                  - ty_ray(ray3)*e2z_ray(ray3)
	  e1y_ray(ray3) = tx_ray(ray3)*e2z_ray(ray3)
     1                  - e2x_ray(ray3)*tz_ray(ray3)
	  e1z_ray(ray3) = e2x_ray(ray3)*ty_ray(ray3)
     1                  - tx_ray(ray3)*e2y_ray(ray3)

	endif !if (abs(tmp2) .ge. 0.01 ) then

	 qq_ray(1,ray3) = (qq_ray(1,ray1)+qq_ray(1,ray2))*0.5
         qq_ray(2,ray3) = (qq_ray(2,ray1)+qq_ray(2,ray2))*0.5
         qq_ray(3,ray3) = (qq_ray(3,ray1)+qq_ray(3,ray2))*0.5
         qq_ray(4,ray3) = (qq_ray(4,ray1)+qq_ray(4,ray2))*0.5
         hq_ray(1,ray3) = (hq_ray(1,ray1)+hq_ray(1,ray2))*0.5
         hq_ray(2,ray3) = (hq_ray(2,ray1)+hq_ray(2,ray2))*0.5
         hq_ray(3,ray3) = (hq_ray(3,ray1)+hq_ray(3,ray2))*0.5
         hq_ray(4,ray3) = (hq_ray(4,ray1)+hq_ray(4,ray2))*0.5
         gs_ray(ray3) = (gs_ray(ray1) + gs_ray(ray2))*0.5
         phase_tmp = phase_ray(ray1) + phase_ray(ray2)+0.1
         phase_ray(ray3) = phase_tmp*0.5

       enddo ! do i_add = 1,n_add

c compute the velocity coefficients at the new ray locations.

       call ktime_3d_compute_v_grad_5(n_ray,n_add,ia_rays
     1,z_ray,x_ray,y_ray
     1,e1x_ray,e1y_ray,e1z_ray
     1,e2x_ray,e2y_ray,e2z_ray
     1,nz_vel,z0_vel,dz_vel
     1,nx_vel,x0_vel,dx_vel
     1,ny_vel,y0_vel,dy_vel
     1,vcof
     1,v_ray,vz_ray,vx_ray,vy_ray,vq11_ray,vq12_ray,vq22_ray
     1,m_work,work
     1,i_err
     1)

c pad the added new rays

      do i_add= 1,n_add

        ray1 = ia_rays(1,i_add)
        ray2 = ia_rays(2,i_add)
        ray3 = ia_rays(3,i_add)

        px = px_ray(ray1) + px_ray(ray2)
        py = py_ray(ray1) + py_ray(ray2)
        pz = pz_ray(ray1) + pz_ray(ray2)
        norm3 = v_ray(ray3)*max(eps, sqrt(px*px + py*py + pz*pz))
        px_ray(ray3) = px/norm3
        py_ray(ray3) = py/norm3
        pz_ray(ray3) = pz/norm3

        vmax_ray(ray3) = max(vmax_ray(ray3),v_ray(ray3))

      enddo ! i_add = 1,n_add

      return

  999 continue
      print'(/,'' error in ktime_3d_add_1_ray_2'')'
      i_err = -1
      return

      end

c23456789012345678901234567890123456789012345678901234567890123456789012
      subroutine ktime_3d_raytrace_add_tubes_2(
     1 it_ray,ds_max
     1,m_ray,n_ray,n_inray,n_add,x_ray,y_ray,z_ray
     1,pz_ray,v_ray
     1,ia_rays,a_ray,b_ray
     1,m_tube,n_tube
     1,ir_tube,is_tube,it_tube
     1,it_flag,n_intube,it_pointer
     1,maxangle
     1,i_err
     1)
c  update the ray and ray tube index caused by the new introduced rays
      implicit none

      integer  it_ray
      real     ds_max

      integer  m_ray,n_ray,n_ray_2,n_inray,n_inray_2
      real     x_ray(m_ray)
      real     y_ray(m_ray)
      real     z_ray(m_ray)
      real     pz_ray(m_ray)
      real     v_ray(m_ray)

      real     a_ray(m_ray)
      real     b_ray(m_ray)
      integer  ia_rays(3,m_ray) ! 1--ray0, 2--ray1 ,3--new ray

      integer  m_tube,n_tube,n_intube,n_intube_2
      integer  ir_tube(3,m_tube)
      integer  is_tube(3,m_tube)
      integer  it_tube(3,m_tube)
      integer  it_flag(m_tube)
      integer  it_pointer(m_tube)

      integer   i_err

      real     dist(3),sidem
      integer  i_tube,j_tube,i_side,i1,i2,i3,i_sidem
      integer  j_side
      integer  n_add
      integer  n_tube_2
      integer  ray0,ray1
      integer  i_intube
      real     maxangle
      integer  add_flag
      real     b_tmp

      n_tube_2 = n_tube
      n_add = 0
      n_ray_2 = n_ray
      n_inray_2 = n_inray
      n_intube_2 = n_intube

      do i_tube =1, n_intube_2

       if (n_inray .lt. m_ray-10 .and. n_intube .lt. m_tube-10) then

         i1 = ir_tube(1,i_tube)
         i2 = ir_tube(2,i_tube)
         i3 = ir_tube(3,i_tube)

         add_flag = 0
         if ( pz_ray(i1)*v_ray(i1) .lt. maxangle ) 
     1         add_flag= add_flag+1
         if ( pz_ray(i2)*v_ray(i2) .lt. maxangle ) 
     1         add_flag= add_flag+1
         if ( pz_ray(i3)*v_ray(i3) .lt. maxangle ) 
     1         add_flag= add_flag+1

       if (add_flag .le. 1) then

         dist(1) = (x_ray(i2) - x_ray(i1)) * (x_ray(i2) - x_ray(i1))
     1           + (y_ray(i2) - y_ray(i1)) * (y_ray(i2) - y_ray(i1))
     1           + (z_ray(i2) - z_ray(i1)) * (z_ray(i2) - z_ray(i1))
         sidem = dist(1)
         i_sidem = 1

         dist(2) = (x_ray(i2) - x_ray(i3)) * (x_ray(i2) - x_ray(i3))
     1           + (y_ray(i2) - y_ray(i3)) * (y_ray(i2) - y_ray(i3))
     1           + (z_ray(i2) - z_ray(i3)) * (z_ray(i2) - z_ray(i3))
         if (dist(2) .gt. sidem+1) then
             sidem = dist(2)
             i_sidem=2
         endif !  if (dist(2) .gt. sidem) then

         dist(3) = (x_ray(i3) - x_ray(i1)) * (x_ray(i3) - x_ray(i1))
     1           + (y_ray(i3) - y_ray(i1)) * (y_ray(i3) - y_ray(i1))
     1           + (z_ray(i3) - z_ray(i1)) * (z_ray(i3) - z_ray(i1))
         if (dist(3) .gt. sidem+1) then
             sidem = dist(3)
             i_sidem=3
         endif !  if (dist(3) .gt. sidem) then

         if (sidem .gt. ds_max*ds_max ) then

             i_side = i_sidem
             j_side = is_tube(i_side,i_tube)
             j_tube = it_tube(i_side,i_tube)

             ray0 = ir_tube(i_side,i_tube)
             ray1 = ir_tube(mod(i_side,3)+1, i_tube)
             n_add = n_add + 1
             n_inray = n_inray + 1

             ia_rays(1,n_add) = ray0
             ia_rays(2,n_add) = ray1
             ia_rays(3,n_add) = n_inray

                     
             a_ray(n_inray)= (a_ray(ray0) + a_ray(ray1))*0.5
	     if (abs(b_ray(ray0)-b_ray(ray1)) .ge. 3.14) then
	       if (b_ray(ray0) .le. 0.0 .and. b_ray(ray1) .gt. 0.0) then
	          b_tmp = b_ray(ray0) + 6.2831853
                  b_ray(n_inray)= (b_tmp + b_ray(ray1))*0.5
	       else if (b_ray(ray1) .le. 0.0 .and. b_ray(ray0) 
     1         .gt. 0.0) then
	          b_tmp = b_ray(ray1) + 6.2831853
                  b_ray(n_inray)= (b_tmp + b_ray(ray0))*0.5
               else
                  b_ray(n_inray)= (b_ray(ray0)+b_ray(ray1))*0.5
	       endif !if (b_ray(ray0) .le. 0.0 .and. b_ray(ray1) .gt. 0.0) then
             else 
	       b_ray(n_inray)= (b_ray(ray0)+b_ray(ray1))*0.5
	     endif !if (abs(b_ray(ray0)-b_ray(ray1)) .ge. 3.14) then
          
             n_intube = n_intube +1

             i1 = mod(i_side,3) + 1
             i2 = 6 - i_side - i1
             ir_tube(i_side,n_intube) = n_inray
             ir_tube(i1,n_intube) = ir_tube(i1,i_tube)
             ir_tube(i2,n_intube) = ir_tube(i2,i_tube)
             ir_tube(i1,i_tube) = n_inray

             if (it_tube(i1,i_tube) .gt. 0) 
     1    it_tube(is_tube(i1,i_tube),it_tube(i1,i_tube))=n_intube
             if (it_tube(i1,i_tube) .gt. n_intube) 
     1    print*,'it_tube error at ', i_tube, i1
             it_tube(i1,n_intube)=it_tube(i1,i_tube)
             is_tube(i1,n_intube)=is_tube(i1,i_tube)

             it_tube(i2 ,n_intube) = i_tube
             is_tube(i2 ,n_intube) = i1
             it_tube(i1,i_tube) = n_intube
             is_tube(i1,i_tube) = i2

             if (j_tube .le. 0 .or. j_side .le. 0) then
	        it_tube(i_side,n_intube) = j_tube
	        is_tube(i_side,n_intube) = j_side
             endif !if (j_tube .le. 0 .or. j_side .le. 0) then

             if (j_tube .gt. 0 ) then

	       if (j_tube .gt. n_intube) 
     1    print*,'j_tube error at ',i_tube,j_tube
               n_intube = n_intube +1

               i1 = mod(j_side,3) + 1
               i2 = 6 - j_side - i1
               ir_tube(j_side,n_intube) = n_inray
               ir_tube(i1,n_intube) = ir_tube(i1,j_tube)
               ir_tube(i2,n_intube) = ir_tube(i2,j_tube)
               ir_tube(i1,j_tube) = n_inray

            if (it_tube(i1,j_tube) .gt. 0)
     1	    it_tube(is_tube(i1,j_tube),it_tube(i1,j_tube))=n_intube
            it_tube(i1,n_intube)=it_tube(i1,j_tube)
            is_tube(i1,n_intube)=is_tube(i1,j_tube)
                                                              
               it_tube(i2 ,n_intube) = j_tube
               is_tube(i2 ,n_intube) = i1
               it_tube(i1,j_tube) = n_intube
               is_tube(i1,j_tube) = i2

               it_tube(j_side,j_tube) = n_intube-1
               is_tube(j_side,j_tube) = i_side
               it_tube(j_side,n_intube) = i_tube
               is_tube(j_side,n_intube) = i_side

               it_tube(i_side,i_tube) = n_intube
               is_tube(i_side,i_tube) = j_side
               it_tube(i_side,n_intube-1) = j_tube
               is_tube(i_side,n_intube-1) = j_side

             endif !  if (j_tube .ne. 0 .and. it_flag(j_tube) .eq. 1) then

         endif!  if (sidem .gt. ds_max*ds_max ) then

       endif ! if (add_flag .le. 1) then
       endif !if (n_inray .lt. m_ray-10 .and. n_tube .lt. m_tube-10) then

      enddo !  do i_tube =1, n_tube_2

      return

      end
c23456789012345678901234567890123456789012345678901234567890123456789012
      subroutine ktime_3d_raytrace_add_rays_2(
     1 lu_out,it_ray,ds_max,xs,ys,zs
     1,i_ray_1,i_ray_2,mt_micro,dt_ray,t0_ray
     1,m_ray,n_ray,n_inray,a_ray,b_ray,x_ray,y_ray,z_ray
     1,s_ray
     1,px_ray,py_ray,pz_ray
     1,tx_ray,ty_ray,tz_ray
     1,e1x_ray,e1y_ray,e1z_ray
     1,e2x_ray,e2y_ray,e2z_ray
     1,m_tube,n_tube,ir_tube,is_tube,it_tube
     1,it_flag
     1,n_intube,it_pointer
     1,ia_rays,tc_ray
     1,qq_ray,hq_ray,gs_ray,phase_ray,ir_flag
     1,v_ray
     1,vq11_ray,vq12_ray,vq22_ray
     1,vx_ray,vy_ray,vz_ray
     1,vmax_ray,v_ray_tmp
     1,nx_vel,x0_vel,dx_vel
     1,ny_vel,y0_vel,dy_vel
     1,nz_vel,z0_vel,dz_vel
     1,vcof
     1,maxangle
     1,m_work,work
     1,i_err
     1)
ccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccc
c Interpolate new rays between pairs of rays that apart too much
ccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccc
c Input:
c ds_max        predefined maximum distance between pair of rays
c xs            source location
c ys            source location
c zs            source location
c mt_micro
c nt_micro
c dt_ray
c i_ray_1
c i_ray_2
c m_ray         number of maximum rays
c n_ray         number of total rays
c a_ray
c b_ray
c x_ray         ray position 
c y_ray         ray position 
c z_ray         ray position
c px_ray        ray slowness
c py_ray        ray slowness 
c pz_ray        ray slowness 
c v_ray         ray velocity 
c vx_ray        ray velocity gradient
c vy_ray        ray velocity gradient
c vz_ray        ray velocity gradient
c vcof          velocity coefficients on grids
c vq11_ray      second derivatives of velocity along ray
c vq12_ray      second derivatives of velocity along ray
c vq22_ray      second derivatives of velocity along ray
c qq_ray        dynamic quantities along ray
c hq_ray        dynamic quantities along ray
c gs_ray        geometrical spreading factor along ray
c n_inrays      number of rays inside boundary
c in_rays       pointers of rays inside boundary
c m_tube        number of maximum tubes
c n_tube        number of total tubes
c ir_tube       ray-vertices of tubes
c it_tube       pointers to adjacent tubes
c is_tube       pointers to sides of adjacent tubes
c n_intube      number of tubes inside table boundary
c in_tubes      pointers to tubes inside table boundary
c
c Output:
c x_ray         ray position 
c y_ray         ray position 
c z_ray         ray position 
c px_ray        ray slowness 
c py_ray        ray slowness 
c pz_ray        ray slowness 
c v_ray         ray velocity 
c vx_ray        ray velocity gradient
c vy_ray        ray velocity gradient
c vz_ray        ray velocity gradient
c vq11_ray      second derivatives of velocity along ray
c vq12_ray      second derivatives of velocity along ray
c vq22_ray      second derivatives of velocity along ray
c qq_ray        dynamic quantities along ray
c hq_ray        dynamic quantities along ray
c gs_ray        geometrical spreading factor along ray
c ir_tube       ray-vertices of tubes
c it_tube       pointers to adjacent tubes
c is_tube       pointers to sides of adjacent tubes
c n_intube      number of tubes inside table boundary
c in_tubes      pointers to tubes inside table boundary
c n_inrays      number of rays inside boundary
c in_rays       pointers of rays inside boundary
c
cccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccc
c Notes: Used internally in function ktime_3d_raytrace_0( )
c               This is the shell that performs interpolating new rays
c               when pairs of rays being apart too much and maintains 
c               the general relationship of rays,sides and triangles in
c               the triangular network.  The process is following:
c               1. check the distance between each pair of rays at time
c                  step i_ray_2, determine the tubes that need adding 
c                  new rays;
c               2. update tube connection information;
c               3. interpolate new rays at time step i_ray_1
c               4. ray-trace the adding rays down to i_ray_2
c               5. calculate the spreading centers of adding tubes at 
c                  time step i_ray_1
cccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccc

      implicit none

      integer   m_ray,it_ray,i_ray_1,i_ray_2,mt_micro
      real      dt_ray,t0_ray
      real      xs,ys,zs
 
      integer  ir_flag(m_ray)
 
      real     ds_max
      integer  n_ray,n_ray2,n_inray,n_inray_2
      real     a_ray(m_ray), b_ray(m_ray)
      real     x_ray(m_ray,2), y_ray(m_ray,2), z_ray(m_ray,2)
      real     s_ray(m_ray,2)
      real     px_ray(m_ray,2), py_ray(m_ray,2), pz_ray(m_ray,2)
      real     v_ray(m_ray,2)
      real     tx_ray(m_ray,2),ty_ray(m_ray,2),tz_ray(m_ray,2)
      real     e1x_ray(m_ray,2),e1y_ray(m_ray,2),e1z_ray(m_ray,2)
      real     e2x_ray(m_ray,2),e2y_ray(m_ray,2),e2z_ray(m_ray,2)
 
      real     vq11_ray(m_ray,2),vq12_ray(m_ray,2),vq22_ray(m_ray,2)
      real     tc_ray(m_ray,2),qq_ray(4,m_ray,2),hq_ray(4,m_ray,2)
      real     gs_ray(m_ray,2)
      integer  phase_ray(m_ray,2)
      real     vx_ray(m_ray,2),vy_ray(m_ray,2),vz_ray(m_ray,2)
      real     vmax_ray(m_ray),v_ray_tmp(m_ray) 
 
      integer  m_tube,n_tube,n_intube
      integer  ir_tube(3,m_tube)
      integer  is_tube(3,m_tube)
      integer  it_tube(3,m_tube)
      integer  ia_rays(3,m_ray)
      integer  it_flag(m_tube)
      integer  it_pointer(m_tube)
 
      integer   nx_vel
      real      x0_vel,dx_vel
 
      integer   ny_vel
      real      y0_vel,dy_vel
 
      integer   nz_vel
      real      z0_vel,dz_vel

      real      vcof(nz_vel,nx_vel,ny_vel,10)

      integer   m_work
      real      work(m_work)
      integer   i_err
      integer   lu_out
      integer   n_add,n_add_2
      integer   ray1,ray2,ray3,i_add
      real      x_tmp, y_tmp, z_tmp, r_tmp
      integer   n_ray_beg, n_ray_end

      real      maxangle 
c  compute the add index and flag for each tube at the current wavefront

      n_add_2 = 0
      n_ray2 = n_ray
      n_inray_2 = n_inray

      call  ktime_3d_raytrace_add_tubes_2(
     1 it_ray,ds_max
     1,m_ray,n_ray,n_inray,n_add
     1,x_ray(1,i_ray_2),y_ray(1,i_ray_2),z_ray(1,i_ray_2)
     1,pz_ray(1,i_ray_2),v_ray(1,i_ray_2)
     1,ia_rays,a_ray,b_ray
     1,m_tube,n_tube
     1,ir_tube,is_tube,it_tube
     1,it_flag,n_intube,it_pointer
     1,maxangle
     1,i_err
     1)
       if (i_err .ne. 0 ) goto 999

      print'('' Add new rays, n_add='',i9)'
     1,n_add

c  add rays at the previous wavefront
       if (n_add .gt. 0 )  then
        n_ray_beg=n_inray_2+1
        n_ray_end=n_inray
        call ktime_3d_raytrace_add_shoot(xs,ys,zs
     1,nx_vel,x0_vel,dx_vel
     1,ny_vel,y0_vel,dy_vel
     1,nz_vel,z0_vel,dz_vel
     1,vcof
     1,it_ray,t0_ray,mt_micro,dt_ray
     1,m_ray, n_ray_beg,n_ray_end
     1,a_ray ,b_ray ,s_ray
     1,x_ray ,y_ray ,z_ray 
     1,px_ray,py_ray,pz_ray
     1,tx_ray,ty_ray,tz_ray
     1,e1x_ray,e1y_ray,e1z_ray
     1,e2x_ray,e2y_ray,e2z_ray
     1,tc_ray
     1,qq_ray,hq_ray,gs_ray,phase_ray
     1,v_ray ,v_ray_tmp,vq11_ray
     1,vq12_ray,vq22_ray
     1,vx_ray,vy_ray,vz_ray
     1,vmax_ray
     1,n_add,ia_rays
     1,m_work,work
     1,i_err)

       endif !if (n_add .gt. 0 )  then

c  check the tube connections for consistency
c     call ktime_3d_raytrace_check_tcon(lu_out
c    1,m_ray,n_ray,a_ray,b_ray,x_ray,y_ray,z_ray,px_ray,py_ray,pz_ray
c    1,m_tube,n_tube,ir_tube,is_tube,it_tube,it_flag
c    1,i_err)
c     if (i_err .ne. 0) goto 999

c  check the tube directions to make sure the obey the right hand rule
c     call ktime_3d_raytrace_check_tdir(lu_out
c    1,m_ray,n_ray,a_ray,b_ray,x_ray,y_ray,z_ray,px_ray,py_ray,pz_ray
c    1,m_tube,n_tube,ir_tube,is_tube,it_tube,it_flag
c    1,i_err)
c     if (i_err .ne. 0) goto 999
                                                                  
 
      return

  988 continue
      print'(/,'' n_ray exceeds m_ray in add_rays'')'
      return
 
  999 continue
      print'(/,'' error in ktime_3d_raytrace_add_rays'')'
      i_err = -1
      return
 
      end
c23456789012345678901234567890123456789012345678901234567890123456789012
      subroutine ktime_3d_compute_v_grad_4(n_xyz,n_inray,ir_pointer
     1,z0,x0,y0
     1,e1x_ray,e1y_ray,e1z_ray
     1,e2x_ray,e2y_ray,e2z_ray
     1,nz_vel,z0_vel,dz_vel
     1,nx_vel,x0_vel,dx_vel
     1,ny_vel,y0_vel,dy_vel
     1,vcof
     1,vfun,dvdz,dvdx,dvdy,dvdq11,dvdq12,dvdq22
     1,m_work,work
     1,i_err
     1)
c  compute velocity derivative coefficients
      implicit none
 
      integer  n_xyz,n_inray,ir_pointer(n_inray),i_inray
      real     x0(n_xyz),y0(n_xyz),z0(n_xyz)
      real     e1x_ray(n_xyz),e1y_ray(n_xyz),e1z_ray(n_xyz)
      real     e2x_ray(n_xyz),e2y_ray(n_xyz),e2z_ray(n_xyz)
 
      integer  nx_vel
      real     x0_vel,dx_vel
 
      integer  ny_vel
      real     y0_vel,dy_vel
 
      integer  nz_vel
      real     z0_vel,dz_vel
 
      real     vcof(nz_vel,nx_vel,ny_vel,10)
 
      real     vfun(n_xyz)
      real     dvdx(n_xyz)
      real     dvdy(n_xyz)
      real     dvdz(n_xyz)
      real     dvdq11(n_xyz)
      real     dvdq12(n_xyz)
      real     dvdq22(n_xyz)
 
      integer  m_work
      real     work(m_work)
 
      integer  i_err
 
      integer  i_xyz
 
      integer  ix,iy,iz
      real     xf,yf,zf
 
      real     v_xy,v_xy_eps

      real     vxx,vyy,vzz,vxy,vxz,vyz
      real     e1x,e1y,e1z,e2x,e2y,e2z

      integer  i_call
      data     i_call/0/
      data     v_xy_eps/1.e-6/
      i_call = i_call + 1
 
c  compute v0, dvdx, dvdy, dvdz from velocity
 
      i_err = 0
 
      do i_xyz = 1 , n_inray
 
        ix = max(1,min(nx_vel-1,int((x0(i_xyz)-x0_vel)/dx_vel)+2))
 
        iy = max(1,min(ny_vel-1,int((y0(i_xyz)-y0_vel)/dy_vel)+2))
 
        iz = max(1,min(nz_vel-1,int((z0(i_xyz)-z0_vel)/dz_vel)+2))
 
        xf = max(0.,min(1.
     1,(x0(i_xyz)-(ix-2)*dx_vel-x0_vel)/dx_vel))
 
        yf = max(0.,min(1.
     1,(y0(i_xyz)-(iy-2)*dy_vel-y0_vel)/dy_vel))
 
        zf = max(0.,min(1.
     1,(z0(i_xyz)-(iz-2)*dz_vel-z0_vel)/dz_vel))
 
        vfun(i_xyz) =   yf * (xf * ( zf * vcof(iz,ix,iy,1)
     1+                   (1-zf)*vcof(max(1,iz-1),ix,iy,1))
     1+          (1-xf) * ( zf * vcof(iz,max(1,ix-1),iy,1)
     1+          (1-zf)*vcof(max(1,iz-1),max(1,ix-1),iy,1)))
     1+     (1-yf) *(xf * ( zf * vcof(iz,ix,max(1,iy-1),1)
     1+          (1-zf)*vcof(max(1,iz-1),ix,max(1,iy-1),1))
     1+ (1-xf) * ( zf * vcof(iz,max(1,ix-1),max(1,iy-1),1)
     1+ (1-zf)*vcof(max(1,iz-1),max(1,ix-1),max(1,iy-1),1)))
 
        dvdx(i_xyz) =   yf * (xf * ( zf * vcof(iz,ix,iy,2)
     1+                   (1-zf)*vcof(max(1,iz-1),ix,iy,2))
     1+          (1-xf) * ( zf * vcof(iz,max(1,ix-1),iy,2)
     1+          (1-zf)*vcof(max(1,iz-1),max(1,ix-1),iy,2)))
     1+     (1-yf) *(xf * ( zf * vcof(iz,ix,max(1,iy-1),2)
     1+          (1-zf)*vcof(max(1,iz-1),ix,max(1,iy-1),2))
     1+ (1-xf) * ( zf * vcof(iz,max(1,ix-1),max(1,iy-1),2)
     1+ (1-zf)*vcof(max(1,iz-1),max(1,ix-1),max(1,iy-1),2)))
 
 
        dvdy(i_xyz) =   yf * (xf * ( zf * vcof(iz,ix,iy,3)
     1+                   (1-zf)*vcof(max(1,iz-1),ix,iy,3))
     1+          (1-xf) * ( zf * vcof(iz,max(1,ix-1),iy,3)
     1+          (1-zf)*vcof(max(1,iz-1),max(1,ix-1),iy,3)))
     1+     (1-yf) *(xf * ( zf * vcof(iz,ix,max(1,iy-1),3)
     1+          (1-zf)*vcof(max(1,iz-1),ix,max(1,iy-1),3))
     1+ (1-xf) * ( zf * vcof(iz,max(1,ix-1),max(1,iy-1),3)
     1+ (1-zf)*vcof(max(1,iz-1),max(1,ix-1),max(1,iy-1),3)))
 
 
        dvdz(i_xyz) =   yf * (xf * ( zf * vcof(iz,ix,iy,4)
     1+                   (1-zf)*vcof(max(1,iz-1),ix,iy,4))
     1+          (1-xf) * ( zf * vcof(iz,max(1,ix-1),iy,4)
     1+          (1-zf)*vcof(max(1,iz-1),max(1,ix-1),iy,4)))
     1+     (1-yf) *(xf * ( zf * vcof(iz,ix,max(1,iy-1),4)
     1+          (1-zf)*vcof(max(1,iz-1),ix,max(1,iy-1),4))
     1+ (1-xf) * ( zf * vcof(iz,max(1,ix-1),max(1,iy-1),4)
     1+ (1-zf)*vcof(max(1,iz-1),max(1,ix-1),max(1,iy-1),4)))

        vxy =   yf * (xf * ( zf * vcof(iz,ix,iy,5)
     1+                   (1-zf)*vcof(max(1,iz-1),ix,iy,5))
     1+          (1-xf) * ( zf * vcof(iz,max(1,ix-1),iy,5)
     1+          (1-zf)*vcof(max(1,iz-1),max(1,ix-1),iy,5)))
     1+     (1-yf) *(xf * ( zf * vcof(iz,ix,max(1,iy-1),5)
     1+          (1-zf)*vcof(max(1,iz-1),ix,max(1,iy-1),5))
     1+ (1-xf) * ( zf * vcof(iz,max(1,ix-1),max(1,iy-1),5)
     1+ (1-zf)*vcof(max(1,iz-1),max(1,ix-1),max(1,iy-1),5)))

        vxz =   yf * (xf * ( zf * vcof(iz,ix,iy,6)
     1+                   (1-zf)*vcof(max(1,iz-1),ix,iy,6))
     1+          (1-xf) * ( zf * vcof(iz,max(1,ix-1),iy,6)
     1+          (1-zf)*vcof(max(1,iz-1),max(1,ix-1),iy,6)))
     1+     (1-yf) *(xf * ( zf * vcof(iz,ix,max(1,iy-1),6)
     1+          (1-zf)*vcof(max(1,iz-1),ix,max(1,iy-1),6))
     1+ (1-xf) * ( zf * vcof(iz,max(1,ix-1),max(1,iy-1),6)
     1+ (1-zf)*vcof(max(1,iz-1),max(1,ix-1),max(1,iy-1),6)))

        vyz =   yf * (xf * ( zf * vcof(iz,ix,iy,7)
     1+                   (1-zf)*vcof(max(1,iz-1),ix,iy,7))
     1+          (1-xf) * ( zf * vcof(iz,max(1,ix-1),iy,7)
     1+          (1-zf)*vcof(max(1,iz-1),max(1,ix-1),iy,7)))
     1+     (1-yf) *(xf * ( zf * vcof(iz,ix,max(1,iy-1),7)
     1+          (1-zf)*vcof(max(1,iz-1),ix,max(1,iy-1),7))
     1+ (1-xf) * ( zf * vcof(iz,max(1,ix-1),max(1,iy-1),7)
     1+ (1-zf)*vcof(max(1,iz-1),max(1,ix-1),max(1,iy-1),7)))

        vxx =   yf * (xf * ( zf * vcof(iz,ix,iy,8)
     1+                   (1-zf)*vcof(max(1,iz-1),ix,iy,8))
     1+          (1-xf) * ( zf * vcof(iz,max(1,ix-1),iy,8)
     1+          (1-zf)*vcof(max(1,iz-1),max(1,ix-1),iy,8)))
     1+     (1-yf) *(xf * ( zf * vcof(iz,ix,max(1,iy-1),8)
     1+          (1-zf)*vcof(max(1,iz-1),ix,max(1,iy-1),8))
     1+ (1-xf) * ( zf * vcof(iz,max(1,ix-1),max(1,iy-1),8)
     1+ (1-zf)*vcof(max(1,iz-1),max(1,ix-1),max(1,iy-1),8)))

        vyy =   yf * (xf * ( zf * vcof(iz,ix,iy,9)
     1+                   (1-zf)*vcof(max(1,iz-1),ix,iy,9))
     1+          (1-xf) * ( zf * vcof(iz,max(1,ix-1),iy,9)
     1+          (1-zf)*vcof(max(1,iz-1),max(1,ix-1),iy,9)))
     1+     (1-yf) *(xf * ( zf * vcof(iz,ix,max(1,iy-1),9)
     1+          (1-zf)*vcof(max(1,iz-1),ix,max(1,iy-1),9))
     1+ (1-xf) * ( zf * vcof(iz,max(1,ix-1),max(1,iy-1),9)
     1+ (1-zf)*vcof(max(1,iz-1),max(1,ix-1),max(1,iy-1),9)))

        vzz =   yf * (xf * ( zf * vcof(iz,ix,iy,10)
     1+                   (1-zf)*vcof(max(1,iz-1),ix,iy,10))
     1+          (1-xf) * ( zf * vcof(iz,max(1,ix-1),iy,10)
     1+          (1-zf)*vcof(max(1,iz-1),max(1,ix-1),iy,10)))
     1+     (1-yf) *(xf * ( zf * vcof(iz,ix,max(1,iy-1),10)
     1+          (1-zf)*vcof(max(1,iz-1),ix,max(1,iy-1),10))
     1+ (1-xf) * ( zf * vcof(iz,max(1,ix-1),max(1,iy-1),10)
     1+ (1-zf)*vcof(max(1,iz-1),max(1,ix-1),max(1,iy-1),10)))

	e1x= e1x_ray(i_xyz)
	e1y= e1y_ray(i_xyz)
	e1z= e1z_ray(i_xyz)
	e2x= e2x_ray(i_xyz)
	e2y= e2y_ray(i_xyz)
	e2z= e2z_ray(i_xyz)

        dvdq11(i_xyz) = vxx*e1x*e1x + vyy*e1y*e1y + vzz*e1z*e1z
     1+ 2.0 * ( vxy*e1x*e1y + vxz*e1x*e1z + vyz*e1y*e1z) 
 
        dvdq12(i_xyz) = vxx*e1x*e2x + vyy*e1y*e2y + vzz*e1z*e2z
     1+  vxy*(e2x*e1y + e1x*e2y) + vxz*(e1x*e2z + e2x*e1z)
     1+  vyz*(e1y*e2z + e2y*e1z) 
 
        dvdq22(i_xyz) = vxx*e2x*e2x + vyy*e2y*e2y + vzz*e2z*e2z
     1+ 2.0 * ( vxy*e2x*e2y + vxz*e2x*e2z + vyz*e2y*e2z) 

      enddo    ! do i_xyz = 1 , n_xyz
 
      return
 
  999 continue
      print'(/,'' error in ktime_3d_compute_v_grad'')'
      i_err = -1
      return

      end
c23456789012345678901234567890123456789012345678901234567890123456789012
      subroutine ktime_3d_compute_v_grad_5(n_xyz,n_add,ia_rays
     1,z0,x0,y0
     1,e1x_ray,e1y_ray,e1z_ray
     1,e2x_ray,e2y_ray,e2z_ray
     1,nz_vel,z0_vel,dz_vel
     1,nx_vel,x0_vel,dx_vel
     1,ny_vel,y0_vel,dy_vel
     1,vcof
     1,vfun,dvdz,dvdx,dvdy,dvdq11,dvdq12,dvdq22
     1,m_work,work
     1,i_err
     1)
c  compute velocity derivative coefficients
      implicit none
 
      integer  n_xyz,n_add,ia_rays(3,n_add),i_add
      real     x0(n_xyz),y0(n_xyz),z0(n_xyz)
      real     e1x_ray(n_xyz),e1y_ray(n_xyz),e1z_ray(n_xyz)
      real     e2x_ray(n_xyz),e2y_ray(n_xyz),e2z_ray(n_xyz)
 
      integer  nx_vel
      real     x0_vel,dx_vel
 
      integer  ny_vel
      real     y0_vel,dy_vel
 
      integer  nz_vel
      real     z0_vel,dz_vel
 
      real     vcof(nz_vel,nx_vel,ny_vel,10)
 
      real     vfun(n_xyz)
      real     dvdx(n_xyz)
      real     dvdy(n_xyz)
      real     dvdz(n_xyz)
      real     dvdq11(n_xyz)
      real     dvdq12(n_xyz)
      real     dvdq22(n_xyz)
 
      integer  m_work
      real     work(m_work)
 
      integer  i_err
 
      integer  i_xyz
 
      integer  ix,iy,iz
      real     xf,yf,zf
 
      real     v_xy,v_xy_eps

      real     vxx,vyy,vzz,vxy,vxz,vyz
      real     e1x,e1y,e1z,e2x,e2y,e2z

      integer  i_call
      data     i_call/0/
      data     v_xy_eps/1.e-6/
      i_call = i_call + 1
 
c  compute v0, dvdx, dvdy, dvdz from velocity
 
      i_err = 0
 
      do i_add = 1 , n_add
 
        i_xyz = ia_rays(3,i_add)
        ix = max(1,min(nx_vel-1,int((x0(i_xyz)-x0_vel)/dx_vel)+2))
 
        iy = max(1,min(ny_vel-1,int((y0(i_xyz)-y0_vel)/dy_vel)+2))
 
        iz = max(1,min(nz_vel-1,int((z0(i_xyz)-z0_vel)/dz_vel)+2))
 
        xf = max(0.,min(1.
     1,(x0(i_xyz)-(ix-2)*dx_vel-x0_vel)/dx_vel))
 
        yf = max(0.,min(1.
     1,(y0(i_xyz)-(iy-2)*dy_vel-y0_vel)/dy_vel))
 
        zf = max(0.,min(1.
     1,(z0(i_xyz)-(iz-2)*dz_vel-z0_vel)/dz_vel))
 
        vfun(i_xyz) =   yf * (xf * ( zf * vcof(iz,ix,iy,1)
     1+                   (1-zf)*vcof(max(1,iz-1),ix,iy,1))
     1+          (1-xf) * ( zf * vcof(iz,max(1,ix-1),iy,1)
     1+          (1-zf)*vcof(max(1,iz-1),max(1,ix-1),iy,1)))
     1+     (1-yf) *(xf * ( zf * vcof(iz,ix,max(1,iy-1),1)
     1+          (1-zf)*vcof(max(1,iz-1),ix,max(1,iy-1),1))
     1+ (1-xf) * ( zf * vcof(iz,max(1,ix-1),max(1,iy-1),1)
     1+ (1-zf)*vcof(max(1,iz-1),max(1,ix-1),max(1,iy-1),1)))
 
        dvdx(i_xyz) =   yf * (xf * ( zf * vcof(iz,ix,iy,2)
     1+                   (1-zf)*vcof(max(1,iz-1),ix,iy,2))
     1+          (1-xf) * ( zf * vcof(iz,max(1,ix-1),iy,2)
     1+          (1-zf)*vcof(max(1,iz-1),max(1,ix-1),iy,2)))
     1+     (1-yf) *(xf * ( zf * vcof(iz,ix,max(1,iy-1),2)
     1+          (1-zf)*vcof(max(1,iz-1),ix,max(1,iy-1),2))
     1+ (1-xf) * ( zf * vcof(iz,max(1,ix-1),max(1,iy-1),2)
     1+ (1-zf)*vcof(max(1,iz-1),max(1,ix-1),max(1,iy-1),2)))
 
 
        dvdy(i_xyz) =   yf * (xf * ( zf * vcof(iz,ix,iy,3)
     1+                   (1-zf)*vcof(max(1,iz-1),ix,iy,3))
     1+          (1-xf) * ( zf * vcof(iz,max(1,ix-1),iy,3)
     1+          (1-zf)*vcof(max(1,iz-1),max(1,ix-1),iy,3)))
     1+     (1-yf) *(xf * ( zf * vcof(iz,ix,max(1,iy-1),3)
     1+          (1-zf)*vcof(max(1,iz-1),ix,max(1,iy-1),3))
     1+ (1-xf) * ( zf * vcof(iz,max(1,ix-1),max(1,iy-1),3)
     1+ (1-zf)*vcof(max(1,iz-1),max(1,ix-1),max(1,iy-1),3)))
 
 
        dvdz(i_xyz) =   yf * (xf * ( zf * vcof(iz,ix,iy,4)
     1+                   (1-zf)*vcof(max(1,iz-1),ix,iy,4))
     1+          (1-xf) * ( zf * vcof(iz,max(1,ix-1),iy,4)
     1+          (1-zf)*vcof(max(1,iz-1),max(1,ix-1),iy,4)))
     1+     (1-yf) *(xf * ( zf * vcof(iz,ix,max(1,iy-1),4)
     1+          (1-zf)*vcof(max(1,iz-1),ix,max(1,iy-1),4))
     1+ (1-xf) * ( zf * vcof(iz,max(1,ix-1),max(1,iy-1),4)
     1+ (1-zf)*vcof(max(1,iz-1),max(1,ix-1),max(1,iy-1),4)))

        vxy =   yf * (xf * ( zf * vcof(iz,ix,iy,5)
     1+                   (1-zf)*vcof(max(1,iz-1),ix,iy,5))
     1+          (1-xf) * ( zf * vcof(iz,max(1,ix-1),iy,5)
     1+          (1-zf)*vcof(max(1,iz-1),max(1,ix-1),iy,5)))
     1+     (1-yf) *(xf * ( zf * vcof(iz,ix,max(1,iy-1),5)
     1+          (1-zf)*vcof(max(1,iz-1),ix,max(1,iy-1),5))
     1+ (1-xf) * ( zf * vcof(iz,max(1,ix-1),max(1,iy-1),5)
     1+ (1-zf)*vcof(max(1,iz-1),max(1,ix-1),max(1,iy-1),5)))

        vxz =   yf * (xf * ( zf * vcof(iz,ix,iy,6)
     1+                   (1-zf)*vcof(max(1,iz-1),ix,iy,6))
     1+          (1-xf) * ( zf * vcof(iz,max(1,ix-1),iy,6)
     1+          (1-zf)*vcof(max(1,iz-1),max(1,ix-1),iy,6)))
     1+     (1-yf) *(xf * ( zf * vcof(iz,ix,max(1,iy-1),6)
     1+          (1-zf)*vcof(max(1,iz-1),ix,max(1,iy-1),6))
     1+ (1-xf) * ( zf * vcof(iz,max(1,ix-1),max(1,iy-1),6)
     1+ (1-zf)*vcof(max(1,iz-1),max(1,ix-1),max(1,iy-1),6)))

        vyz =   yf * (xf * ( zf * vcof(iz,ix,iy,7)
     1+                   (1-zf)*vcof(max(1,iz-1),ix,iy,7))
     1+          (1-xf) * ( zf * vcof(iz,max(1,ix-1),iy,7)
     1+          (1-zf)*vcof(max(1,iz-1),max(1,ix-1),iy,7)))
     1+     (1-yf) *(xf * ( zf * vcof(iz,ix,max(1,iy-1),7)
     1+          (1-zf)*vcof(max(1,iz-1),ix,max(1,iy-1),7))
     1+ (1-xf) * ( zf * vcof(iz,max(1,ix-1),max(1,iy-1),7)
     1+ (1-zf)*vcof(max(1,iz-1),max(1,ix-1),max(1,iy-1),7)))

        vxx =   yf * (xf * ( zf * vcof(iz,ix,iy,8)
     1+                   (1-zf)*vcof(max(1,iz-1),ix,iy,8))
     1+          (1-xf) * ( zf * vcof(iz,max(1,ix-1),iy,8)
     1+          (1-zf)*vcof(max(1,iz-1),max(1,ix-1),iy,8)))
     1+     (1-yf) *(xf * ( zf * vcof(iz,ix,max(1,iy-1),8)
     1+          (1-zf)*vcof(max(1,iz-1),ix,max(1,iy-1),8))
     1+ (1-xf) * ( zf * vcof(iz,max(1,ix-1),max(1,iy-1),8)
     1+ (1-zf)*vcof(max(1,iz-1),max(1,ix-1),max(1,iy-1),8)))

        vyy =   yf * (xf * ( zf * vcof(iz,ix,iy,9)
     1+                   (1-zf)*vcof(max(1,iz-1),ix,iy,9))
     1+          (1-xf) * ( zf * vcof(iz,max(1,ix-1),iy,9)
     1+          (1-zf)*vcof(max(1,iz-1),max(1,ix-1),iy,9)))
     1+     (1-yf) *(xf * ( zf * vcof(iz,ix,max(1,iy-1),9)
     1+          (1-zf)*vcof(max(1,iz-1),ix,max(1,iy-1),9))
     1+ (1-xf) * ( zf * vcof(iz,max(1,ix-1),max(1,iy-1),9)
     1+ (1-zf)*vcof(max(1,iz-1),max(1,ix-1),max(1,iy-1),9)))

        vzz =   yf * (xf * ( zf * vcof(iz,ix,iy,10)
     1+                   (1-zf)*vcof(max(1,iz-1),ix,iy,10))
     1+          (1-xf) * ( zf * vcof(iz,max(1,ix-1),iy,10)
     1+          (1-zf)*vcof(max(1,iz-1),max(1,ix-1),iy,10)))
     1+     (1-yf) *(xf * ( zf * vcof(iz,ix,max(1,iy-1),10)
     1+          (1-zf)*vcof(max(1,iz-1),ix,max(1,iy-1),10))
     1+ (1-xf) * ( zf * vcof(iz,max(1,ix-1),max(1,iy-1),10)
     1+ (1-zf)*vcof(max(1,iz-1),max(1,ix-1),max(1,iy-1),10)))

	e1x= e1x_ray(i_xyz)
	e1y= e1y_ray(i_xyz)
	e1z= e1z_ray(i_xyz)
	e2x= e2x_ray(i_xyz)
	e2y= e2y_ray(i_xyz)
	e2z= e2z_ray(i_xyz)

        dvdq11(i_xyz) = vxx*e1x*e1x + vyy*e1y*e1y + vzz*e1z*e1z
     1+ 2.0 * ( vxy*e1x*e1y + vxz*e1x*e1z + vyz*e1y*e1z) 
 
        dvdq12(i_xyz) = vxx*e1x*e2x + vyy*e1y*e2y + vzz*e1z*e2z
     1+  vxy*(e2x*e1y + e1x*e2y) + vxz*(e1x*e2z + e2x*e1z)
     1+  vyz*(e1y*e2z + e2y*e1z) 
 
        dvdq22(i_xyz) = vxx*e2x*e2x + vyy*e2y*e2y + vzz*e2z*e2z
     1+ 2.0 * ( vxy*e2x*e2y + vxz*e2x*e2z + vyz*e2y*e2z) 

	enddo    ! do i_xyz = 1 , n_xyz
 
      return
 
  999 continue
      print'(/,'' error in ktime_3d_compute_v_grad'')'
      i_err = -1
      return

      end
c23456789012345678901234567890123456789012345678901234567890123456789012
      subroutine ktime_3d_compute_v_grad_1(
     1 xs,ys,zs
     1,n_xyz
     1,z0,x0,y0
     1,pz,px,py
     1,nz_vel,z0_vel,dz_vel
     1,nx_vel,x0_vel,dx_vel
     1,ny_vel,y0_vel,dy_vel
     1,vcof
     1,vfun,dvdz,dvdx,dvdy,dvdq11,dvdq12,dvdq22
     1,m_work,work
     1,i_err
     1)
c--------------------------------------------------------------
c     Purpose:
c              Special constant velocity gradient version
c              Find velocity and some of the derivatives from the
c              model.
c--------------------------------------------------------------
c     Input :
c             z0 - z location
c             x0 - x location
c             y0 - y location
c             pz - z ray parameter
c             px - x ray parameter
c             py - y ray parameter
c--------------------------------------------------------------
c     Output :
c             vfun - velocity function
c             dvdz - 1st derivative with respect to global z.
c             dvdx - 1st derivative with respect to global x
c             dvdy - 1st derivative with respect to global y
c             dvdq - 2nd derivative with respect to ray centred
c                      coordinate.
c  right now set v0,xc,yc,zc,vx,vy,vz using the first nodes of vcof
c  v(x,y,z) = v0 + vx*(x-xc) + vy*(y-yc) + vz*(z-zc)
c  dvdx = vx
c  dvdy = vy
c  dvdz = vz
c  dvdq = 0.
c--------------------------------------------------------------
 
      implicit none

      real     xs,ys,zs
 
      integer  n_xyz
      real     x0(n_xyz),y0(n_xyz),z0(n_xyz)
      real     px(n_xyz),py(n_xyz),pz(n_xyz)
 
      integer  nx_vel
      real     x0_vel,dx_vel
 
      integer  ny_vel
      real     y0_vel,dy_vel
 
      integer  nz_vel
      real     z0_vel,dz_vel
 
      real     vcof(nz_vel,nx_vel,ny_vel,10)

      real     vfun(n_xyz)
      real     dvdx(n_xyz)
      real     dvdy(n_xyz)
      real     dvdz(n_xyz)
      real     dvdq11(n_xyz)
      real     dvdq12(n_xyz)
      real     dvdq22(n_xyz)

      integer  m_work
      real     work(m_work)
 
      integer  i_err
 
      integer  i_xyz

      integer  ix,iy,iz
      real     xf,yf,zf
      real     cq(3,3)   !   dx_i/dq_j
 
      real     v_xy,v_xy_eps

      integer  i_call
      data     i_call/0/
      data     v_xy_eps/1.e-6/
      i_call = i_call + 1
 
c  compute v0, dvdx, dvdy, dvdz from velocity

      i_err = 0

      do i_xyz = 1 , n_xyz

        ix = max(1,min(nx_vel-1,int((x0(i_xyz)-x0_vel)/dx_vel)+2))

        iy = max(1,min(ny_vel-1,int((y0(i_xyz)-y0_vel)/dy_vel)+2))
 
        iz = max(1,min(nz_vel-1,int((z0(i_xyz)-z0_vel)/dz_vel)+2))
 
        xf = max(0.,min(1.
     1,(x0(i_xyz)-(ix-2)*dx_vel-x0_vel)/dx_vel))

        yf = max(0.,min(1.
     1,(y0(i_xyz)-(iy-2)*dy_vel-y0_vel)/dy_vel))
 
        zf = max(0.,min(1.
     1,(z0(i_xyz)-(iz-2)*dz_vel-z0_vel)/dz_vel))

        vfun(i_xyz) =   yf * (xf * ( zf * vcof(iz,ix,iy,1)
     1+                   (1-zf)*vcof(max(1,iz-1),ix,iy,1)) 
     1+          (1-xf) * ( zf * vcof(iz,max(1,ix-1),iy,1) 
     1+          (1-zf)*vcof(max(1,iz-1),max(1,ix-1),iy,1)))
     1+     (1-yf) *(xf * ( zf * vcof(iz,ix,max(1,iy-1),1) 
     1+          (1-zf)*vcof(max(1,iz-1),ix,max(1,iy-1),1)) 
     1+ (1-xf) * ( zf * vcof(iz,max(1,ix-1),max(1,iy-1),1) 
     1+ (1-zf)*vcof(max(1,iz-1),max(1,ix-1),max(1,iy-1),1)))

        dvdx(i_xyz) =   yf * (xf * ( zf * vcof(iz,ix,iy,2)
     1+                   (1-zf)*vcof(max(1,iz-1),ix,iy,2))
     1+          (1-xf) * ( zf * vcof(iz,max(1,ix-1),iy,2)
     1+          (1-zf)*vcof(max(1,iz-1),max(1,ix-1),iy,2)))
     1+     (1-yf) *(xf * ( zf * vcof(iz,ix,max(1,iy-1),2)
     1+          (1-zf)*vcof(max(1,iz-1),ix,max(1,iy-1),2))
     1+ (1-xf) * ( zf * vcof(iz,max(1,ix-1),max(1,iy-1),2)
     1+ (1-zf)*vcof(max(1,iz-1),max(1,ix-1),max(1,iy-1),2)))


        dvdy(i_xyz) =   yf * (xf * ( zf * vcof(iz,ix,iy,3)
     1+                   (1-zf)*vcof(max(1,iz-1),ix,iy,3))
     1+          (1-xf) * ( zf * vcof(iz,max(1,ix-1),iy,3)
     1+          (1-zf)*vcof(max(1,iz-1),max(1,ix-1),iy,3)))
     1+     (1-yf) *(xf * ( zf * vcof(iz,ix,max(1,iy-1),3)
     1+          (1-zf)*vcof(max(1,iz-1),ix,max(1,iy-1),3))
     1+ (1-xf) * ( zf * vcof(iz,max(1,ix-1),max(1,iy-1),3)
     1+ (1-zf)*vcof(max(1,iz-1),max(1,ix-1),max(1,iy-1),3)))


        dvdz(i_xyz) =   yf * (xf * ( zf * vcof(iz,ix,iy,4)
     1+                   (1-zf)*vcof(max(1,iz-1),ix,iy,4))
     1+          (1-xf) * ( zf * vcof(iz,max(1,ix-1),iy,4)
     1+          (1-zf)*vcof(max(1,iz-1),max(1,ix-1),iy,4)))
     1+     (1-yf) *(xf * ( zf * vcof(iz,ix,max(1,iy-1),4)
     1+          (1-zf)*vcof(max(1,iz-1),ix,max(1,iy-1),4))
     1+ (1-xf) * ( zf * vcof(iz,max(1,ix-1),max(1,iy-1),4)
     1+ (1-zf)*vcof(max(1,iz-1),max(1,ix-1),max(1,iy-1),4)))

        v_xy = 1. / max(v_xy_eps,
     1sqrt(px(i_xyz)*px(i_xyz)+py(i_xyz)*py(i_xyz))) ! alpha cann't be 0 or pi 
        
        cq(1,1) = v_xy * py(i_xyz)
        cq(2,1) = -v_xy * px(i_xyz)
        cq(3,1) = 0
        cq(1,2) = vcof(iz,ix,iy,1) *pz(i_xyz) *v_xy *px(i_xyz)
        cq(2,2) = vcof(iz,ix,iy,1) *pz(i_xyz) *v_xy *py(i_xyz)
        cq(3,2) = -vcof(iz,ix,iy,1) / v_xy

        dvdq11(i_xyz) = vcof(iz,ix,iy,8)*cq(1,1)*cq(1,1)
     1+vcof(iz,ix,iy,9)*cq(2,1)*cq(2,1) 
     1+vcof(iz,ix,iy,10)*cq(3,1)*cq(3,1)
     1+ 2 * ( vcof(iz,ix,iy,5)*cq(1,1)*cq(2,1) 
     1+vcof(iz,ix,iy,6)*cq(1,1)*cq(3,1)
     1+vcof(iz,ix,iy,7)*cq(2,1)*cq(3,1)) 

        dvdq12(i_xyz) = vcof(iz,ix,iy,8)*cq(1,1)*cq(1,2)
     1+vcof(iz,ix,iy,9)*cq(2,1)*cq(2,2) 
     1+vcof(iz,ix,iy,10)*cq(3,1)*cq(3,2)     
     1+vcof(iz,ix,iy,5)* (cq(1,2)*cq(2,1) + cq(1,1)*cq(2,2))
     1+vcof(iz,ix,iy,6)* (cq(1,3)*cq(3,2) + cq(1,2)*cq(3,1))
     1+vcof(iz,ix,iy,7)* (cq(2,1)*cq(3,2) + cq(2,2)*cq(3,1))

        dvdq22(i_xyz) = vcof(iz,ix,iy,8)*cq(1,2)*cq(1,2)
     1+vcof(iz,ix,iy,9)*cq(2,2)*cq(2,2) 
     1+vcof(iz,ix,iy,10)*cq(3,2)*cq(3,2)     
     1+ 2 * ( vcof(iz,ix,iy,5)*cq(1,2)*cq(2,2) 
     1+vcof(iz,ix,iy,6)*cq(1,2)*cq(3,2)
     1+vcof(iz,ix,iy,7)*cq(2,2)*cq(3,2)) 
   
 
      enddo    ! do i_xyz = 1 , n_xyz

      return
 
  999 continue
      print'(/,'' error in ktime_3d_compute_v_grad'')'
      i_err = -1
      return
      end 
c23456789012345678901234567890123456789012345678901234567890123456789012
      subroutine ktime_3d_compute_v_grad_coef(
     1 nz_vel,z0_vel,dz_vel
     1,nx_vel,x0_vel,dx_vel
     1,ny_vel,y0_vel,dy_vel
     1,vel
     1,nz_vel_rt,z0_vel_rt,dz_vel_rt
     1,nx_vel_rt,x0_vel_rt,dx_vel_rt
     1,ny_vel_rt,y0_vel_rt,dy_vel_rt
     1,vcof)
c-----------------------------------------------------------------
c     Purpose: Compute the velocity gradient as a pre-process for
c              linear interporlation.
c-----------------------------------------------------------------------
c
      implicit  none
 
      integer  nz_vel
      real     z0_vel,dz_vel
 
      integer  nx_vel
      real     x0_vel,dx_vel
 
      integer  ny_vel
      real     y0_vel,dy_vel
 
      real     vel(nz_vel,nx_vel,ny_vel)

      integer  nz_vel_rt
      real     z0_vel_rt,dz_vel_rt
 
      integer  nx_vel_rt
      real     x0_vel_rt,dx_vel_rt
 
      integer  ny_vel_rt
      real     y0_vel_rt,dy_vel_rt

      real     vcof(nz_vel_rt,nx_vel_rt,ny_vel_rt,10)
 
      integer  ix,ix_1,ix_2,ix_3
      real     dx
 
      integer  iy,iy_1,iy_2,iy_3
      real     dy
 
      integer  iz,iz_1,iz_2,iz_3
      real     dz
 
      integer  ix0,iy0
 
      ix0 = (x0_vel_rt - x0_vel)/dx_vel_rt
      iy0 = (y0_vel_rt - y0_vel)/dy_vel_rt
 
      do iz = 1, nz_vel_rt
 
        do ix = 1, nx_vel_rt
 
           do iy = 1, ny_vel_rt

             vcof(iz,ix,iy,1) = vel(iz,ix+ix0,iy+iy0) 

c  dv / dx
             ix_1 = max(        1,ix-1)
             ix_2 = min(nx_vel_rt,ix+1)
             dx = max(1,ix_2-ix_1) * dx_vel
             vcof(iz,ix,iy,2) = (vel(iz,ix_2+ix0,iy+iy0) 
     1                         - vel(iz,ix_1+ix0,iy+iy0)) / dx
 
c  dv / dy
             iy_1 = max(        1,iy-1)
             iy_2 = min(ny_vel_rt,iy+1)
             dy = max(1,iy_2-iy_1) * dy_vel
             vcof(iz,ix,iy,3) = (vel(iz,ix+ix0,iy_2+iy0) 
     1                        -  vel(iz,ix+ix0,iy_1+iy0)) / dy
 
c  dv / dz
             iz_1 = max(        1,iz-1)
             iz_2 = min(nz_vel_rt,iz+1)
             dz = max(1,iz_2-iz_1) * dz_vel
             vcof(iz,ix,iy,4) = (vel(iz_2,ix+ix0,iy+iy0) 
     1                        -  vel(iz_1,ix+ix0,iy+iy0)) / dz
 
c  d2v / dxdy
             vcof(iz,ix,iy,5) = (vel(iz,ix_2+ix0,iy_2+iy0) 
     1+ vel(iz,ix_1+ix0,iy_1+iy0)
     1- vel(iz,ix_2+ix0,iy_1+iy0) - vel(iz,ix_1+ix0,iy_2+iy0) ) / dx /dy
 
c  d2v / dxdz
             vcof(iz,ix,iy,6) = (vel(iz_2,ix_2+ix0,iy+iy0) 
     1+ vel(iz_1,ix_1+ix0,iy+iy0)
     1-vel(iz_1,ix_2+ix0,iy+iy0) - vel(iz_2,ix_1+ix0,iy+iy0) ) / dx /dz
 
c  d2v / dzdy
             vcof(iz,ix,iy,7) = (vel(iz_2,ix+ix0,iy_2+iy0) 
     1 + vel(iz_1,ix+ix0,iy_1+iy0)
     1-vel(iz_2,ix+ix0,iy_1+iy0) - vel(iz_1,ix+ix0,iy_2+iy0) ) / dz /dy
 
c  d2v / dx2
             ix_1 = min(nx_vel_rt-2,  max(     1,ix-1))
             ix_2 = ix_1 + 1
             ix_3 = ix_1 + 2
             vcof(iz,ix,iy,8) = (vel(iz,ix_3+ix0,iy+iy0)  
     1- 2 *vel(iz,ix_2+ix0,iy+iy0)
     1+ vel(iz,ix_1+ix0,iy+iy0)) / dx_vel /dx_vel
 
c  d2v / dy2
             iy_1 = min(ny_vel_rt-2,  max(     1,iy-1))
             iy_2 = iy_1 + 1
             iy_3 = iy_1 + 2
             vcof(iz,ix,iy,9) = (vel(iz,ix+ix0,iy_3+iy0) 
     1 - 2 *vel(iz,ix+ix0,iy_2+iy0)
     1+ vel(iz,ix+ix0,iy_1+iy0)) / dy_vel /dy_vel
 
c  d2v / dz2
             iz_1 = min(nz_vel_rt-2,  max(     1,iz-1))
             iz_2 = iz_1 + 1
             iz_3 = iz_1 + 2
             vcof(iz,ix,iy,10) = (vel(iz_3,ix+ix0,iy+iy0) 
     1- 2 *vel(iz_2,ix+ix0,iy+iy0)
     1+ vel(iz_1,ix+ix0,iy+iy0)) / dz_vel /dz_vel
 
           enddo ! do iy = 1, ny_vel
 
        enddo !do ix = 1, nx_vel

      enddo !do iz = 1, nz_vel
 
      return
 
      end

c23456789012345678901234567890123456789012345678901234567890123456789012
      subroutine ktime_3d_raytrace_init_addray(lu_out
     1,xs,ys,zs,vs,t0_ray
     1,n_add,ia_rays
     1,m_ray,n_ray_beg, n_ray_end
     1, a_ray, b_ray, s_ray
     1, v_ray
     1, x_ray, y_ray, z_ray
     1,px_ray,py_ray,pz_ray
     1,tx_ray,ty_ray,tz_ray
     1,e1x_ray,e1y_ray,e1z_ray
     1,e2x_ray,e2y_ray,e2z_ray
     1,qq_ray,hq_ray,gs_ray
     1,vx_ray,vy_ray,vz_ray
     1,vq11_ray,vq12_ray,vq22_ray
     1,vmax_ray,tc_ray,phase_ray
     1,nz_vel,z0_vel,dz_vel
     1,nx_vel,x0_vel,dx_vel
     1,ny_vel,y0_vel,dy_vel
     1,vel
     1,m_work,work
     1,i_err)
ccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccc
c  compute an inital list of rays a,b 
c  and pointers to the associated ray tubes
c
c  Input:
c  lu_out = logical unit to write ray and ray tube information to
c             lu_out < 0 means to output is written
c  xs     = source x location
c  ys     = source y location
c  zs     = source z location
c  ds_ray = maxiomum spatial separation between rays 
c             in either the a or b directions
c  r_ray  = nominal radius at which separation is measured 
c             10. * dsray is good
c             the actual value used will be max(ds_ray,r_ray)
c  m_ray  = maximum number of rays
c  m_tube = maximum number of ray tubes
c
c  Output:
c  n_ray  = actual number of rays established
c  a_ray  = real array of a angles established dimensioned m_ray
c  b_ray  = real array of b angles established dimensioned m_ray
c  x_ray  = real array of x values established dimensioned m_ray
c  y_ray  = real array of y values established dimensioned m_ray
c  z_ray  = real array of z values established dimensioned m_ray
c  the x,y,z values are set at radius r_ray
c  the will need to be reset to xs,ys,zs when used
c
c  px_ray  = real array of px values established dimensioned m_ray
c  py_ray  = real array of py values established dimensioned m_ray
c  pz_ray  = real array of pz values established dimensioned m_ray
c  n_tube = actual number of ray tubes established
c  i_err = error flag 0 = o.k. -1 = error
c
ccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccc
      implicit none

      integer  lu_out
      real     xs,ys,zs
      real     r_ray_0,t0_ray

      integer  n_add, ia_rays(3,n_add)

      integer  m_ray,n_ray_beg,n_ray_end
      real      a_ray(m_ray)
      real      b_ray(m_ray)
      real      s_ray(m_ray)
      real      x_ray(m_ray)
      real      y_ray(m_ray)
      real      z_ray(m_ray)
      real     px_ray(m_ray)
      real     py_ray(m_ray)
      real     pz_ray(m_ray)
      real     tx_ray(m_ray),ty_ray(m_ray),tz_ray(m_ray)
      real     e1x_ray(m_ray),e1y_ray(m_ray),e1z_ray(m_ray)
      real     e2x_ray(m_ray),e2y_ray(m_ray),e2z_ray(m_ray)
      real      v_ray(m_ray)
      real     vx_ray(m_ray)
      real     vy_ray(m_ray)
      real     vz_ray(m_ray)
      real     vmax_ray(m_ray)
      real     qq_ray(4,m_ray)
      real     hq_ray(4,m_ray)
      real     gs_ray(m_ray)
      real     vq11_ray(m_ray)
      real     vq12_ray(m_ray)
      real     vq22_ray(m_ray)
      real     tc_ray(m_ray)
      integer  phase_ray(m_ray)

      integer  nx_vel
      real     x0_vel,dx_vel

      integer  ny_vel
      real     y0_vel,dy_vel

      integer  nz_vel
      real     z0_vel,dz_vel

c  2d has bicubic spline coefficients in vcof 3d has velocitya
      real     vel(nz_vel,nx_vel,ny_vel,10)

      integer  m_work
      real     work(m_work)

      integer  i_err

      real     vs,vxs,vys,vzs,vq11s,vq12s,vq22s
      real     gss,qq0
      integer  i_ray, i_add

      real     sa,ca,sb,cb
      i_err = 0

c  compute the velocity at the source location

      call ktime_3d_compute_v_grad_1(
     1 xs,ys,zs
     1,1
     1, zs,xs,ys
     1,1.,1.0,1.
     1,nz_vel,z0_vel,dz_vel
     1,nx_vel,x0_vel,dx_vel
     1,ny_vel,y0_vel,dy_vel
     1,vel
     1,vs,vzs,vxs,vys,vq11s,vq12s,vq22s
     1,m_work,work
     1,i_err
     1)
      if (i_err .ne. 0) goto 999

      r_ray_0 = vs * t0_ray
      qq0 = vs*t0_ray

      do i_ray =n_ray_beg,n_ray_end

          sa = sin(a_ray(i_ray))
          ca = cos(a_ray(i_ray))
          sb = sin(b_ray(i_ray))
          cb = cos(b_ray(i_ray))

          x_ray(i_ray) = r_ray_0 * sa * cb + xs
          y_ray(i_ray) = r_ray_0 * sa * sb + ys
          z_ray(i_ray) = r_ray_0 * ca +zs

          tx_ray(i_ray) = sa * cb  ! tx
          ty_ray(i_ray) = sa * sb  ! ty
          tz_ray(i_ray) = ca       ! tz

	  px_ray(i_ray) = tx_ray(i_ray) / vs ! px
          py_ray(i_ray) = ty_ray(i_ray) / vs ! py
          pz_ray(i_ray) = tz_ray(i_ray) / vs ! pz
 
          e1x_ray(i_ray) = ca * cb
	  e1y_ray(i_ray) = ca * sb
	  e1z_ray(i_ray) = -sa

          e2x_ray(i_ray) = -sb
	  e2y_ray(i_ray) = cb
	  e2z_ray(i_ray) = 0

          s_ray(i_ray) = r_ray_0
          tc_ray(i_ray) = 1.0

          v_ray(i_ray) = vs
          vx_ray(i_ray) = vxs
          vy_ray(i_ray) = vys
          vz_ray(i_ray) = vzs
          vq11_ray(i_ray) = vq11s
          vq12_ray(i_ray) = vq12s
          vq22_ray(i_ray) = vq22s
          vmax_ray(i_ray) = vs

          qq_ray(1,i_ray) = qq0            !dynamic raytracing 
          qq_ray(2,i_ray) = 0              !dynamic raytracing 
          qq_ray(3,i_ray) = 0              !dynamic raytracing 
          qq_ray(4,i_ray) = qq0            !dynamic raytracing 

          hq_ray(1,i_ray) = 1./vs             !dynamic raytracing 
          hq_ray(2,i_ray) = 0              !dynamic raytracing 
          hq_ray(3,i_ray) = 0              !dynamic raytracing 
          hq_ray(4,i_ray) = 1./vs             !dynamic raytracing 
        
          gs_ray(i_ray)   = qq0*qq0        !geometrical spreading
          phase_ray(i_ray)   = 0        

      enddo !do i_add =1, n_add

      return

  999 continue
      print'(/,'' error in ktime_3d_raytrace_init_rays'')'
      i_err = -1
      return

      end

c23456789012345678901234567890123456789012345678901234567890123456789012
      subroutine ktime_3d_raytrace_shoot_n_add(
     1 xs,ys,zs
     1,mt_ray,dt_ray
     1,n_ray,n_ray_beg,n_ray_end,n_add,ia_rays
     1,a_ray,b_ray
     1, x_ray_1, y_ray_1, z_ray_1
     1, s_ray_1
     1,px_ray_1,py_ray_1,pz_ray_1
     1,tx_ray_1,ty_ray_1,tz_ray_1
     1,e1x_ray_1,e1y_ray_1,e1z_ray_1
     1,e2x_ray_1,e2y_ray_1,e2z_ray_1
     1,tc_ray_1,qq_ray_1,hq_ray_1,gs_ray_1,phase_ray_1
     1,  v_ray_1,v_ray_tmp,vq11_ray_1
     1,vq12_ray_1,vq22_ray_1
     1,vx_ray_1,vy_ray_1,vz_ray_1,vmax_ray
     1, x_ray_2, y_ray_2, z_ray_2
     1, s_ray_2
     1,px_ray_2,py_ray_2,pz_ray_2
     1,tx_ray_2,ty_ray_2,tz_ray_2
     1,e1x_ray_2,e1y_ray_2,e1z_ray_2
     1,e2x_ray_2,e2y_ray_2,e2z_ray_2
     1,tc_ray_2,qq_ray_2,hq_ray_2,gs_ray_2,phase_ray_2
     1,  v_ray_2,vq11_ray_2
     1,vq12_ray_2,vq22_ray_2
     1,vx_ray_2,vy_ray_2,vz_ray_2
     1,nx_vel,x0_vel,dx_vel
     1,ny_vel,y0_vel,dy_vel
     1,nz_vel,z0_vel,dz_vel
     1,vcof
     1,m_work,work
     1,i_err,t_ray_1
     1)
cccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccc
c  trace rays a max of mt_ray time steps through a gridded velocity model
c  start at x_ray_1,y_ray_1,z_ray_1
c  end   at x_ray_2,y_ray_2,z_ray_2
c  may be inplace
c
c  input :
c        gs_flag 
c        mt_ray,nt_ray,dt_ray
c        n_ray,n_inray,in_rays
c        x_ray_1, y_ray_1, z_ray_1
c        px_ray_1,py_ray_1,pz_ray_1
c        v_ray_1,vq11_ray_1,vq12_ray_1,vq22_ray_1
c        vx_ray_1,vy_ray_1,vz_ray_1
c        nx_vel,x0_vel,dx_vel
c        ny_vel,y0_vel,dy_vel
c        nz_vel,z0_vel,dz_vel
c        vcof
c
c  output :
c        x_ray_2, y_ray_2, z_ray_2
c        px_ray_2,py_ray_2,pz_ray_2
c        v_ray_2,vq11_ray_2,vq12_ray_2,vq22_ray_2
c        vx_ray_2,vy_ray_2,vz_ray_2
cccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccc
      implicit  none

      real      xs,ys,zs
 
      integer   mt_ray,nt_ray
      real      dt_ray
 
      integer   n_ray,n_ray_beg,n_ray_end
      integer   n_add,ia_rays(3,n_add),i_add
 
      real       a_ray(n_ray), b_ray(n_ray)
      real       x_ray_1(n_ray), y_ray_1(n_ray), z_ray_1(n_ray)
      real      px_ray_1(n_ray),py_ray_1(n_ray),pz_ray_1(n_ray)
      real      tx_ray_1(n_ray),ty_ray_1(n_ray),tz_ray_1(n_ray)
      real      e1x_ray_1(n_ray),e1y_ray_1(n_ray),e1z_ray_1(n_ray)
      real      e2x_ray_1(n_ray),e2y_ray_1(n_ray),e2z_ray_1(n_ray)
      real       s_ray_1(n_ray)
      real      v_ray_1(n_ray),v_ray_tmp(n_ray),vq11_ray_1(n_ray)
      real      vq12_ray_1(n_ray),vq22_ray_1(n_ray)
      real      tc_ray_1(n_ray),qq_ray_1(4,n_ray),hq_ray_1(4,n_ray)
      real      gs_ray_1(n_ray)
      integer   phase_ray_1(n_ray)
      real      vx_ray_1(n_ray),vy_ray_1(n_ray),vz_ray_1(n_ray)
      real      vmax_ray(n_ray)
 
      real       x_ray_2(n_ray), y_ray_2(n_ray), z_ray_2(n_ray)
      real      px_ray_2(n_ray),py_ray_2(n_ray),pz_ray_2(n_ray)
      real      tx_ray_2(n_ray),ty_ray_2(n_ray),tz_ray_2(n_ray)
      real      e1x_ray_2(n_ray),e1y_ray_2(n_ray),e1z_ray_2(n_ray)
      real      e2x_ray_2(n_ray),e2y_ray_2(n_ray),e2z_ray_2(n_ray)
      real       s_ray_2(n_ray)
      real      v_ray_2(n_ray),vq11_ray_2(n_ray)
      real      vq12_ray_2(n_ray),vq22_ray_2(n_ray)
      real      tc_ray_2(n_ray),qq_ray_2(4,n_ray),hq_ray_2(4,n_ray)
      real      gs_ray_2(n_ray)
      integer   phase_ray_2(n_ray)
      real      vx_ray_2(n_ray),vy_ray_2(n_ray),vz_ray_2(n_ray)
 
      integer   nx_vel
      real      x0_vel,dx_vel
 
      integer   ny_vel
      real      y0_vel,dy_vel
 
      integer   nz_vel
      real      z0_vel,dz_vel

      real      vcof(nz_vel,nx_vel,ny_vel,10)
 
      integer   m_work
      real      work(m_work)
 
      integer   i_err
      real      t_ray_1,t_const
 
      integer   it_ray
 
      integer  i_call

      integer  i_ray, i_inray

      real     t_a,amp_a, amp_c, r1,r2, a1,a2,trans,qq
      real     a2_ct,a2_cp,q11,q12,q21,q22,hq11,hq22
      real     e1x,e1y,e1z,e2x,e2y,e2z,tx,ty,tz
     
      real      tc_cos1,tc_cos2,tmp,vx,vy,vz
      real      tmp1,tmp2,tmp3,tmp4
      data     i_call/0/

      i_call = i_call + 1
 
      i_err = 0

c  trace rays mt_ray steps of length dt_ray
c  the rays start in arrays x_ray_1. etc.
c  and end in arrays x_ray_2, etc.
c  these may be the same arrays

      do it_ray = 1 , mt_ray

        t_const = t_ray_1 + it_ray*dt_ray
c  at the first step go from x_ray_1 to x_ray_2
        if (it_ray .eq. 1) then
 
          call ktime_3d_raytrace_shoot_1_add(
     1 xs,ys,zs
     1,dt_ray
     1,n_ray,n_ray_beg,n_ray_end,n_add,ia_rays
     1, x_ray_1, y_ray_1, z_ray_1
     1, s_ray_1, a_ray
     1,px_ray_1,py_ray_1,pz_ray_1
     1,tx_ray_1,ty_ray_1,tz_ray_1
     1,e1x_ray_1,e1y_ray_1,e1z_ray_1
     1,e2x_ray_1,e2y_ray_1,e2z_ray_1
     1,tc_ray_1,qq_ray_1,hq_ray_1,gs_ray_1,phase_ray_1
     1,  v_ray_1,v_ray_tmp,vq11_ray_1
     1,vq12_ray_1,vq22_ray_1
     1,vx_ray_1,vy_ray_1,vz_ray_1
     1, x_ray_2, y_ray_2, z_ray_2
     1, s_ray_2
     1,px_ray_2,py_ray_2,pz_ray_2
     1,tx_ray_2,ty_ray_2,tz_ray_2
     1,e1x_ray_2,e1y_ray_2,e1z_ray_2
     1,e2x_ray_2,e2y_ray_2,e2z_ray_2
     1,tc_ray_2,qq_ray_2,hq_ray_2,gs_ray_2,phase_ray_2
     1,  v_ray_2,vq11_ray_2
     1,vq12_ray_2,vq22_ray_2
     1,vx_ray_2,vy_ray_2,vz_ray_2
     1,vmax_ray
     1,nx_vel,x0_vel,dx_vel
     1,ny_vel,y0_vel,dy_vel
     1,nz_vel,z0_vel,dz_vel
     1,vcof
     1,m_work,work
     1,i_err,t_const
     1)
 
c  at the first step go from x_ray_2 to x_ray_2
        else    ! if (it_ray .eq. 1) then
 
          call ktime_3d_raytrace_shoot_1_add(
     1 xs,ys,zs
     1,dt_ray
     1,n_ray,n_ray_beg,n_ray_end,n_add,ia_rays
     1, x_ray_2, y_ray_2, z_ray_2
     1, s_ray_2, a_ray
     1,px_ray_2,py_ray_2,pz_ray_2
     1,tx_ray_2,ty_ray_2,tz_ray_2
     1,e1x_ray_2,e1y_ray_2,e1z_ray_2
     1,e2x_ray_2,e2y_ray_2,e2z_ray_2
     1,tc_ray_2,qq_ray_2,hq_ray_2,gs_ray_2,phase_ray_2
     1,  v_ray_2,v_ray_tmp,vq11_ray_2
     1,vq12_ray_2,vq22_ray_2
     1,vx_ray_2,vy_ray_2,vz_ray_2
     1, x_ray_2, y_ray_2, z_ray_2
     1, s_ray_2
     1,px_ray_2,py_ray_2,pz_ray_2
     1,tx_ray_2,ty_ray_2,tz_ray_2
     1,e1x_ray_2,e1y_ray_2,e1z_ray_2
     1,e2x_ray_2,e2y_ray_2,e2z_ray_2
     1,tc_ray_2,qq_ray_2,hq_ray_2,gs_ray_2,phase_ray_2
     1,  v_ray_2,vq11_ray_2
     1,vq12_ray_2,vq22_ray_2
     1,vx_ray_2,vy_ray_2,vz_ray_2
     1,vmax_ray
     1,nx_vel,x0_vel,dx_vel
     1,ny_vel,y0_vel,dy_vel
     1,nz_vel,z0_vel,dz_vel
     1,vcof
     1,m_work,work
     1,i_err,t_const
     1)
 
        endif    ! if (it_ray .eq. 1) then
        if (i_err .ne. 0) goto 999
 
        nt_ray = it_ray
 
      enddo    ! do it_ray = 1 , mt_ray

c compute tc_ray_2

      do i_ray = n_ray_beg, n_ray_end

        vx = (vx_ray_1(i_ray)+vx_ray_2(i_ray))*0.5   
        vy = (vy_ray_1(i_ray)+vy_ray_2(i_ray))*0.5   
        vz = (vz_ray_1(i_ray)+vz_ray_2(i_ray))*0.5   

        tc_cos1 = px_ray_1(i_ray)*vx + py_ray_1(i_ray)*vy
     1          + pz_ray_1(i_ray)*vz

        tc_cos2 = px_ray_2(i_ray)*vx + py_ray_2(i_ray)*vy
     1          + pz_ray_2(i_ray)*vz

        tmp = v_ray_2(i_ray)*tc_cos1 + v_ray_1(i_ray)*tc_cos2

        if (abs(tmp) .le. 0.001) then

            tc_ray_2(i_ray) = tc_ray_1(i_ray)

        else

            tc_ray_2(i_ray) = tc_ray_1(i_ray) * 2.0 
     1      *v_ray_2(i_ray)*tc_cos1/tmp

        endif !if (abs(tmp) .le. eps) then

      enddo !  do i_ray = 1, n_inray

      return
 
  999 continue
      print'('' error in ktime_3d_raytrace_shoot_n_step'')'
      i_err = -1
      return
 
      end
 
c23456789012345678901234567890123456789012345678901234567890123456789012
      subroutine ktime_3d_raytrace_shoot_1_add(
     1 xs,ys,zs
     1,dt_ray
     1,n_ray,n_ray_beg,n_ray_end,n_add,ia_rays
     1, x_ray_1, y_ray_1, z_ray_1
     1, s_ray_1, a_ray
     1,px_ray_1,py_ray_1,pz_ray_1
     1,tx_ray_1,ty_ray_1,tz_ray_1
     1,e1x_ray_1,e1y_ray_1,e1z_ray_1
     1,e2x_ray_1,e2y_ray_1,e2z_ray_1
     1,tc_ray_1,qq_ray_1,hq_ray_1,gs_ray_1,phase_ray_1
     1,  v_ray_1,v_ray_tmp,vq11_ray_1
     1,vq12_ray_1,vq22_ray_1
     1,vx_ray_1,vy_ray_1,vz_ray_1
     1, x_ray_2, y_ray_2, z_ray_2
     1, s_ray_2
     1,px_ray_2,py_ray_2,pz_ray_2
     1,tx_ray_2,ty_ray_2,tz_ray_2
     1,e1x_ray_2,e1y_ray_2,e1z_ray_2
     1,e2x_ray_2,e2y_ray_2,e2z_ray_2
     1,tc_ray_2,qq_ray_2,hq_ray_2,gs_ray_2,phase_ray_2
     1,  v_ray_2,vq11_ray_2
     1,vq12_ray_2,vq22_ray_2
     1,vx_ray_2,vy_ray_2,vz_ray_2
     1,vmax_ray
     1,nx_vel,x0_vel,dx_vel
     1,ny_vel,y0_vel,dy_vel
     1,nz_vel,z0_vel,dz_vel
     1,vcof
     1,m_work,work
     1,i_err,t_const
     1)
ccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccc
c  trace rays a SINGLE time step through a gridded velocity model
c  start at x_ray_1,y_ray_1,z_ray_1
c  end   at x_ray_2,y_ray_2,z_ray_2
c  may be inplace
c
c  input :
c        gs_flag
c        mt_ray,nt_ray,dt_ray
c        n_ray,n_inray,in_rays
c        x_ray_1, y_ray_1, z_ray_1
c        px_ray_1,py_ray_1,pz_ray_1
c        v_ray_1,vq11_ray_1,vq12_ray_1,vq22_ray_1
c        vx_ray_1,vy_ray_1,vz_ray_1
c        nx_vel,x0_vel,dx_vel
c        ny_vel,y0_vel,dy_vel
c        nz_vel,z0_vel,dz_vel
c        vcof
c
c  output :
c        x_ray_2, y_ray_2, z_ray_2
c        px_ray_2,py_ray_2,pz_ray_2
c        v_ray_2,vq11_ray_2,vq12_ray_2,vq22_ray_2
c        vx_ray_2,vy_ray_2,vz_ray_2
cccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccc
      implicit  none

      real      xs,ys,zs
 
      real      dt_ray,t_const
 
      integer   n_ray,n_ray_beg,n_ray_end
      integer   n_add,ia_rays(3,n_add),i_add
 
      real       x_ray_1(n_ray), y_ray_1(n_ray), z_ray_1(n_ray)
      real      px_ray_1(n_ray),py_ray_1(n_ray),pz_ray_1(n_ray)
      real      tx_ray_1(n_ray),ty_ray_1(n_ray),tz_ray_1(n_ray)
      real      e1x_ray_1(n_ray),e1y_ray_1(n_ray),e1z_ray_1(n_ray)
      real      e2x_ray_1(n_ray),e2y_ray_1(n_ray),e2z_ray_1(n_ray)
      real       s_ray_1(n_ray), a_ray(n_ray), ca
      real       v_ray_1(n_ray),v_ray_tmp(n_ray),vq11_ray_1(n_ray)
      real       vq12_ray_1(n_ray),vq22_ray_1(n_ray)
      real      tc_ray_1(n_ray),qq_ray_1(4,n_ray),hq_ray_1(4,n_ray)
      real      gs_ray_1(n_ray)
      integer   phase_ray_1(n_ray)
      real      vx_ray_1(n_ray),vy_ray_1(n_ray),vz_ray_1(n_ray)
 
      real       x_ray_2(n_ray), y_ray_2(n_ray), z_ray_2(n_ray)
      real      px_ray_2(n_ray),py_ray_2(n_ray),pz_ray_2(n_ray)
      real      tx_ray_2(n_ray),ty_ray_2(n_ray),tz_ray_2(n_ray)
      real      e1x_ray_2(n_ray),e1y_ray_2(n_ray),e1z_ray_2(n_ray)
      real      e2x_ray_2(n_ray),e2y_ray_2(n_ray),e2z_ray_2(n_ray)
      real       s_ray_2(n_ray)
      real       v_ray_2(n_ray),vq11_ray_2(n_ray)
      real       vq12_ray_2(n_ray),vq22_ray_2(n_ray)
      real      tc_ray_2(n_ray),qq_ray_2(4,n_ray),hq_ray_2(4,n_ray)
      real      gs_ray_2(n_ray)
      integer   phase_ray_2(n_ray)
      real      vx_ray_2(n_ray),vy_ray_2(n_ray),vz_ray_2(n_ray)
      real      vmax_ray(n_ray)

      real      vv_ray 
      integer   nx_vel
      real      x0_vel,dx_vel
 
      integer   ny_vel
      real      y0_vel,dy_vel
 
      integer   nz_vel
      real      z0_vel,dz_vel

      real      vcof(nz_vel,nx_vel,ny_vel,10)

      real      gs_ray_tmp
 
      integer   m_work
      real      work(m_work)
 
      integer   i_err
 
      integer   i_ray
      real      pxyz
      real      dx,dy,dz,r
      real      tc_cos1,tc_cos2,tmp
      real      eps

      real      dce1,dce2,dct,dtx,dty,dtz,txyz
      real      de1x,de1y,de1z, de2x,de2y,de2z
      real      tmp1,tmp2,tmp3,tmp4

      integer  i_call
      data      eps/1e-6/
      data     i_call/0/
      i_call = i_call + 1
 
      i_err = 0
 
c  compute the new x,y,z coords useing the current velocity and ray parameters
c  s_ray is the ray path length
 
      do i_ray = n_ray_beg,n_ray_end

        vv_ray = v_ray_1(i_ray)*v_ray_1(i_ray)
 
        dx = dt_ray * px_ray_1(i_ray) * vv_ray
        x_ray_2(i_ray) = x_ray_1(i_ray) + dx
 
        dy = dt_ray * py_ray_1(i_ray) * vv_ray
        y_ray_2(i_ray) = y_ray_1(i_ray) + dy
 
        dz = dt_ray * pz_ray_1(i_ray) * vv_ray
        z_ray_2(i_ray) = z_ray_1(i_ray) + dz
 
        s_ray_2(i_ray) = s_ray_1(i_ray) + sqrt(dx**2+dy**2+dz**2)

	dce1 = vx_ray_1(i_ray)*e1x_ray_1(i_ray) 
     1       + vy_ray_1(i_ray)*e1y_ray_1(i_ray)
     1       + vz_ray_1(i_ray)*e1z_ray_1(i_ray)
  	dct  = vx_ray_1(i_ray)*tx_ray_1(i_ray) 
     1       + vy_ray_1(i_ray)*ty_ray_1(i_ray)
     1       + vz_ray_1(i_ray)*tz_ray_1(i_ray)

        de1x = tx_ray_1(i_ray)*dce1 
        de1y = ty_ray_1(i_ray)*dce1 
        de1z = tz_ray_1(i_ray)*dce1 

        dtx = -vx_ray_1(i_ray) + tx_ray_1(i_ray)*dct
        dty = -vy_ray_1(i_ray) + ty_ray_1(i_ray)*dct
        dtz = -vz_ray_1(i_ray) + tz_ray_1(i_ray)*dct

        tx_ray_2(i_ray) = tx_ray_1(i_ray) + dtx*dt_ray
        ty_ray_2(i_ray) = ty_ray_1(i_ray) + dty*dt_ray
        tz_ray_2(i_ray) = tz_ray_1(i_ray) + dtz*dt_ray

        txyz = sqrt(tx_ray_2(i_ray)**2+ty_ray_2(i_ray)**2
     1	        +tz_ray_2(i_ray)**2)

        tx_ray_2(i_ray) = tx_ray_2(i_ray) / txyz
        ty_ray_2(i_ray) = ty_ray_2(i_ray) / txyz
        tz_ray_2(i_ray) = tz_ray_2(i_ray) / txyz

	e1x_ray_2(i_ray) = e1x_ray_1(i_ray) + de1x*dt_ray
	e1y_ray_2(i_ray) = e1y_ray_1(i_ray) + de1y*dt_ray
	e1z_ray_2(i_ray) = e1z_ray_1(i_ray) + de1z*dt_ray
        
        txyz = sqrt(e1x_ray_2(i_ray)**2+e1y_ray_2(i_ray)**2
     1	        +e1z_ray_2(i_ray)**2)

        e1x_ray_2(i_ray) = e1x_ray_2(i_ray) / txyz
        e1y_ray_2(i_ray) = e1y_ray_2(i_ray) / txyz
        e1z_ray_2(i_ray) = e1z_ray_2(i_ray) / txyz

	e2x_ray_2(i_ray) = ty_ray_2(i_ray)*e1z_ray_2(i_ray) 
     1	- tz_ray_2(i_ray)*e1y_ray_2(i_ray) 
	e2y_ray_2(i_ray) = tz_ray_2(i_ray)*e1x_ray_2(i_ray) 
     1	- tx_ray_2(i_ray)*e1z_ray_2(i_ray) 
	e2z_ray_2(i_ray) = tx_ray_2(i_ray)*e1y_ray_2(i_ray) 
     1	- ty_ray_2(i_ray)*e1x_ray_2(i_ray) 

        tmp2=e1x_ray_2(i_ray)*tx_ray_2(i_ray)  +e1y_ray_2(i_ray)
     1   *ty_ray_2(i_ray) +e1z_ray_2(i_ray)*tz_ray_2(i_ray)

        if (abs(tmp2) .ge. 0.01 ) then
        
	  e1x_ray_2(i_ray) = e2y_ray_2(i_ray)*tz_ray_2(i_ray)
     1                     - ty_ray_2(i_ray)*e2z_ray_2(i_ray)
	  e1y_ray_2(i_ray) = tx_ray_2(i_ray)*e2z_ray_2(i_ray)
     1                     - e2x_ray_2(i_ray)*tz_ray_2(i_ray)
	  e1z_ray_2(i_ray) = e2x_ray_2(i_ray)*ty_ray_2(i_ray)
     1                     - tx_ray_2(i_ray)*e2y_ray_2(i_ray)

	endif !if (abs(tmp2) .ge. 0.01 ) then

      enddo    ! do i_add = 1 , n_add
 
c  compute the velocity at this new x,y,z location
c  use the current px,py,pz values

      call ktime_3d_compute_v_grad_5(
     1 n_ray,n_add,ia_rays
     1, z_ray_2, x_ray_2, y_ray_2
     1,e1x_ray_2,e1y_ray_2,e1z_ray_2
     1,e2x_ray_2,e2y_ray_2,e2z_ray_2
     1,nz_vel,z0_vel,dz_vel
     1,nx_vel,x0_vel,dx_vel
     1,ny_vel,y0_vel,dy_vel
     1,vcof
     1,v_ray_2,vz_ray_2,vx_ray_2,vy_ray_2,vq11_ray_2
     1,vq12_ray_2,vq22_ray_2
     1,m_work,work
     1,i_err
     1)
      if (i_err .ne. 0) goto 999

c  compute the ray parameter at this new x,z location
      do i_ray = n_ray_beg,n_ray_end
 
        px_ray_2(i_ray) = px_ray_1(i_ray)
     1- dt_ray * vx_ray_2(i_ray) / v_ray_2(i_ray)
 
        py_ray_2(i_ray) = py_ray_1(i_ray)
     1- dt_ray * vy_ray_2(i_ray) / v_ray_2(i_ray)
 
        pz_ray_2(i_ray) = pz_ray_1(i_ray)
     1- dt_ray * vz_ray_2(i_ray) / v_ray_2(i_ray)

        pxyz = v_ray_2(i_ray)
     1 * sqrt(px_ray_2(i_ray)**2+py_ray_2(i_ray)**2+pz_ray_2(i_ray)**2)
 
        px_ray_2(i_ray) = px_ray_2(i_ray) / pxyz
        py_ray_2(i_ray) = py_ray_2(i_ray) / pxyz
        pz_ray_2(i_ray) = pz_ray_2(i_ray) / pxyz

        vmax_ray(i_ray) = max(vmax_ray(i_ray),v_ray_2(i_ray))

      enddo    ! do i_add = 1 , n_add

c  compute the new spreading factor qx,qy,qz
c  normalize the ray parameters
      do i_ray = n_ray_beg,n_ray_end

        vv_ray = v_ray_2(i_ray) * v_ray_2(i_ray)

        qq_ray_2(1,i_ray) = qq_ray_1(1,i_ray) + dt_ray
     1* hq_ray_1(1,i_ray) * vv_ray 
 
        qq_ray_2(2,i_ray) = qq_ray_1(2,i_ray) + dt_ray
     1* hq_ray_1(2,i_ray) * vv_ray
 
        qq_ray_2(3,i_ray) = qq_ray_1(3,i_ray) + dt_ray
     1* hq_ray_1(3,i_ray) * vv_ray
 
        qq_ray_2(4,i_ray) = qq_ray_1(4,i_ray) + dt_ray
     1* hq_ray_1(4,i_ray) * vv_ray
 
        hq_ray_2(1,i_ray) = hq_ray_1(1,i_ray) - dt_ray
     1* ( vq11_ray_2(i_ray) * qq_ray_1(1,i_ray)
     1+   vq12_ray_2(i_ray) * qq_ray_1(2,i_ray) ) /v_ray_2(i_ray)
 
        hq_ray_2(2,i_ray) = hq_ray_1(2,i_ray) - dt_ray
     1* ( vq12_ray_2(i_ray) * qq_ray_1(1,i_ray)
     1+   vq22_ray_2(i_ray) * qq_ray_1(2,i_ray) ) /v_ray_2(i_ray)
 
        hq_ray_2(3,i_ray) = hq_ray_1(3,i_ray) - dt_ray
     1* ( vq11_ray_2(i_ray) * qq_ray_1(3,i_ray)
     1+   vq12_ray_2(i_ray) * qq_ray_1(4,i_ray) ) /v_ray_2(i_ray)
 
        hq_ray_2(4,i_ray) = hq_ray_1(4,i_ray) - dt_ray
     1* ( vq12_ray_2(i_ray) * qq_ray_1(3,i_ray)
     1+   vq22_ray_2(i_ray) * qq_ray_1(4,i_ray) ) /v_ray_2(i_ray)
 
c  compute the new spreading
 
        gs_ray_tmp = gs_ray_1(i_ray)
        gs_ray_2(i_ray) =  qq_ray_2(1,i_ray) * qq_ray_2(4,i_ray)
     1-                  qq_ray_2(2,i_ray) * qq_ray_2(3,i_ray) 

	if (gs_ray_tmp .ge. 0.0 .and. gs_ray_2(i_ray) .le. 0.0
     1 .or. gs_ray_tmp .le. 0.0 .and. gs_ray_2(i_ray) .ge. 0.0 )
     1   then
           phase_ray_2(i_ray) = phase_ray_1(i_ray) + 1
        else
           phase_ray_2(i_ray) = phase_ray_1(i_ray)
        endif !if (gs_ray_1(i_ray) .ge. 0.0 .and. gs_ray_2(i_ray) .le. 0.0

      enddo    ! do i_inray = 1 , n_inray

      return
 
  998 continue
      print'('' error in ktime_3d_raytrace_shoot_1_add''
     1,/,'' need more work memory need='',i8,'' and have='',i8)'
     1,n_ray*3,m_work
      goto 999
 
  999 continue
      print'('' error in ktime_3d_raytrace_shoot_1_add'')'
      i_err = -1
      return
 
      end
 
c23456789012345678901234567890123456789012345678901234567890123456789012
      subroutine ktime_3d_raytrace_add_shoot(xs,ys,zs
     1,nx_vel,x0_vel,dx_vel
     1,ny_vel,y0_vel,dy_vel
     1,nz_vel,z0_vel,dz_vel
     1,vel
     1,nt_ray,t0_ray,mt_micro,dt_ray
     1,m_ray, n_ray_beg, n_ray_end
     1,a_ray ,b_ray ,s_ray
     1,x_ray ,y_ray ,z_ray 
     1,px_ray,py_ray,pz_ray
     1,tx_ray,ty_ray,tz_ray
     1,e1x_ray,e1y_ray,e1z_ray
     1,e2x_ray,e2y_ray,e2z_ray
     1,tc_ray
     1,qq_ray,hq_ray,gs_ray,phase_ray
     1,v_ray ,v_ray_tmp,vq11_ray
     1,vq12_ray,vq22_ray
     1,vx_ray,vy_ray,vz_ray
     1,vmax_ray
     1,n_add,ia_rays
     1,m_work,work
     1,i_err)
c  shell for adding new rays

      implicit none

      real     xs,ys,zs

      real     nullvalue 

      integer  nx_vel
      real     x0_vel,dx_vel

      integer  ny_vel
      real     y0_vel,dy_vel

      integer  nz_vel
      real     z0_vel,dz_vel

c  2d has bicubic spline coefficients in vcof 3d has velocity
      real     vel(nz_vel,nx_vel,ny_vel,10)

      integer  mt_micro
      integer  nt_ray
      real     t0_ray,dt_ray

      integer  m_ray, n_ray_beg, n_ray_end

      real      a_ray(m_ray)
      real      b_ray(m_ray)
      real      s_ray(m_ray,2)
      real      x_ray(m_ray,2), y_ray(m_ray,2), z_ray(m_ray,2)
      real     px_ray(m_ray,2), py_ray(m_ray,2),pz_ray(m_ray,2)
      real     tx_ray(m_ray,2),ty_ray(m_ray,2),tz_ray(m_ray,2)
      real     e1x_ray(m_ray,2),e1y_ray(m_ray,2),e1z_ray(m_ray,2)
      real     e2x_ray(m_ray,2),e2y_ray(m_ray,2),e2z_ray(m_ray,2)
      real      v_ray(m_ray,2), v_ray_tmp(m_ray)
      real     tc_ray(m_ray,2)
      real     qq_ray(4,m_ray,2)
      real     hq_ray(4,m_ray,2) 
      real     gs_ray(m_ray,2)
      integer  phase_ray(m_ray,2)
      real     vq11_ray(m_ray,2)
      real     vq12_ray(m_ray,2)
      real     vq22_ray(m_ray,2)
      real     vx_ray(m_ray,2), vy_ray(m_ray,2), vz_ray(m_ray,2)
      real     vmax_ray(1)
      real     vs

      integer  n_add,ia_rays(3,n_add)

      integer  m_work
      real     work(m_work)

      integer  i_err

      integer  it_ray
      integer  nt_micro
      real     t1_ray,t_ray_1,t_ray_2

      real     ds_ray

      integer  na_add

      integer  i,j,k

      integer  i_ray_1,i_ray_2

      real     t_cpu_1,t_cpu_2

      integer  lu_out
      integer  i_call
      data     i_call/0/

c initalize ray information

      call ktime_3d_raytrace_init_addray(lu_out
     1,xs,ys,zs,vs,t0_ray
     1,n_add,ia_rays
     1,m_ray,n_ray_beg,n_ray_end
     1, a_ray, b_ray, s_ray(1,1), v_ray(1,1)
     1, x_ray(1,1), y_ray(1,1), z_ray(1,1)
     1,px_ray(1,1),py_ray(1,1),pz_ray(1,1)
     1,tx_ray(1,1),ty_ray(1,1),tz_ray(1,1)
     1,e1x_ray(1,1),e1y_ray(1,1),e1z_ray(1,1)
     1,e2x_ray(1,1),e2y_ray(1,1),e2z_ray(1,1)
     1,qq_ray(1,1,1),hq_ray(1,1,1),gs_ray(1,1)
     1,vx_ray(1,1),vy_ray(1,1),vz_ray(1,1)
     1,vq11_ray(1,1),vq12_ray(1,1),vq22_ray(1,1)
     1,vmax_ray,tc_ray(1,1),phase_ray(1,1)
     1,nz_vel,z0_vel,dz_vel
     1,nx_vel,x0_vel,dx_vel
     1,ny_vel,y0_vel,dy_vel
     1,vel
     1,m_work,work
     1,i_err)

      i_ray_2 = 1
c  cycle over macro time steps
c  within each macro step there will be mt_micro steps dt_ray long
      do it_ray = 1 , nt_ray

c  there are two memory shells
c  one for the current and one for the next macro time step
c  swap the memory shell pointers

        i_ray_1 = i_ray_2            ! current time step pointer
        i_ray_2 = mod(i_ray_2,2) + 1 ! next    time step pointer

c  the current and next time value
        t_ray_1 = (it_ray - 1) * (mt_micro * dt_ray) + t0_ray
        t_ray_2 = (it_ray - 0) * (mt_micro * dt_ray) + t0_ray

c  the current ray is    in i_ray_1
c  the next step will be in i_ray_2

        call ktime_3d_raytrace_shoot_n_add(
     1 xs,ys,zs
     1,mt_micro,dt_ray
     1,m_ray,n_ray_beg,n_ray_end,n_add,ia_rays
     1,a_ray,b_ray
     1, x_ray(1,i_ray_1), y_ray(1,i_ray_1), z_ray(1,i_ray_1)
     1, s_ray(1,i_ray_1)
     1,px_ray(1,i_ray_1),py_ray(1,i_ray_1),pz_ray(1,i_ray_1)
     1,tx_ray(1,i_ray_1),ty_ray(1,i_ray_1),tz_ray(1,i_ray_1)
     1,e1x_ray(1,i_ray_1),e1y_ray(1,i_ray_1),e1z_ray(1,i_ray_1)
     1,e2x_ray(1,i_ray_1),e2y_ray(1,i_ray_1),e2z_ray(1,i_ray_1)
     1,tc_ray(1,i_ray_1),qq_ray(1,1,i_ray_1),hq_ray(1,1,i_ray_1)
     1,gs_ray(1,i_ray_1),phase_ray(1,i_ray_1)
     1,  v_ray(1,i_ray_1),v_ray_tmp,vq11_ray(1,i_ray_1)
     1,vq12_ray(1,i_ray_1),vq22_ray(1,i_ray_1)
     1,vx_ray(1,i_ray_1),vy_ray(1,i_ray_1),vz_ray(1,i_ray_1)
     1,vmax_ray
     1, x_ray(1,i_ray_2), y_ray(1,i_ray_2), z_ray(1,i_ray_2)
     1, s_ray(1,i_ray_2)
     1,px_ray(1,i_ray_2),py_ray(1,i_ray_2),pz_ray(1,i_ray_2)
     1,tx_ray(1,i_ray_2),ty_ray(1,i_ray_2),tz_ray(1,i_ray_2)
     1,e1x_ray(1,i_ray_2),e1y_ray(1,i_ray_2),e1z_ray(1,i_ray_2)
     1,e2x_ray(1,i_ray_2),e2y_ray(1,i_ray_2),e2z_ray(1,i_ray_2)
     1,tc_ray(1,i_ray_2),qq_ray(1,1,i_ray_2),hq_ray(1,1,i_ray_2)
     1,gs_ray(1,i_ray_2),phase_ray(1,i_ray_2)
     1,  v_ray(1,i_ray_2),vq11_ray(1,i_ray_2)
     1,vq12_ray(1,i_ray_2),vq22_ray(1,i_ray_2)
     1,vx_ray(1,i_ray_2),vy_ray(1,i_ray_2),vz_ray(1,i_ray_2)
     1,nx_vel,x0_vel,dx_vel
     1,ny_vel,y0_vel,dy_vel
     1,nz_vel,z0_vel,dz_vel
     1,vel
     1,m_work,work
     1,i_err,t_ray_1
     1)

      enddo !do it_ray = 1 , nt_ray
      if (i_err .ne. 0) goto 999

      return
  999 continue
      print'('' error in ktime_3d_raytrace_add_shoot'')'
      i_err = -1
      return

      end


