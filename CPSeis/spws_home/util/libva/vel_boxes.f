
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


ccccccccccccc   vel_boxes.f   ccccccccccccccccc


c------------subroutines called (or referred to) from C-------------------------


cccc        start_vel_boxes (vel2,am_in_va2)

cccc      get_vbstruct_pointer (vb2)

cccc         read_register (box)              read_apply (dummy,error)
cccc         save_register (box)              save_apply (dummy,error)
cccc         pick_register (box)              pick_apply (dummy,error)
cccc          fun_register (box)               fun_apply (dummy,error)
cccc          set_register (box)               set_apply (dummy,error)
cccc         vfid_register (box)              vfid_apply (dummy,error)
cccc         head_register (box)              head_apply (dummy,error)
cccc          res_register (box)               res_apply (dummy,error)
cccc          lat_register (box)               lat_apply (dummy,error)
cccc         lat2_register (box)              lat2_apply (dummy,error)
cccc          ray_register (box)               ray_apply (dummy,error)
cccc          del_register (box)               del_apply (dummy,error)
cccc         mult_register (box)              mult_apply (dummy,error)
cccc         misc_register (box)              misc_apply (dummy,error)
cccc      offmute_register (box)           offmute_apply (dummy,error)

cccc         read_trap (box,ident,index,   text,nread,endkey)
cccc         save_trap (box,ident,index,   text,nread,endkey)
cccc         pick_trap (box,ident,index,   text,nread,endkey)
cccc          fun_trap (box,ident,index,   text,nread,endkey)
cccc          set_trap (box,ident,index,   text,nread,endkey)
cccc         vfid_trap (box,ident,index,   text,nread,endkey)
cccc         head_trap (box,ident,index,   text,nread,endkey)
cccc          res_trap (box,ident,index,   text,nread,endkey)
cccc          lat_trap (box,ident,index,   text,nread,endkey)
cccc         lat2_trap (box,ident,index,   text,nread,endkey)
cccc          ray_trap (box,ident,index,   text,nread,endkey)
cccc          del_trap (box,ident,index,   text,nread,endkey)
cccc         mult_trap (box,ident,index,   text,nread,endkey)
cccc         misc_trap (box,ident,index,   text,nread,endkey)
cccc      offmute_trap (box,ident,index,   text,nread,endkey)

cccc      pick_prev    (dummy)
cccc      pick_next    (dummy)
cccc      pick_undo    (dummy)
cccc      fun_deselect (dummy)



c---------------subroutines called only from Fortran----------------------------


cccc   set_res_switches (type2,istepflag,iterpflag,         <list of switches>)
cccc   set_lat_switches       (istepflag,iterpflag,         <list of switches>)
cccc   set_lat2_switches(type2,ichngflag,iterpflag,idirflag,<list of switches>)

cccc   recalculate_lat2 (iwhich,r_xfirst,r_xinc,r_xlast,r_nx,endkey)

cccc      get_switches_from_type  (type,   sw,ks1,ks3,ks7)
cccc      get_type_from_switches  (sw,               type)

cccc      vel_immediate           (box,word,ifun,nfun,step)
cccc      set_velfun_type         (i,want)
cccc      increment_velfun_type   (i)                 <-- entry point
cccc      show_velfun_type        (want)

cccc      register_active_velfun

cccc      get_choice (ident,ifirst,ilast,ichoice,choice)



c-------------------------------------------------------------------------------



      subroutine start_vel_boxes (vel2,am_in_va2)
c     initialize windowbox_specific data in common block in vel_boxes.inc.
c     also save vel = pointer to main program data structure.

      implicit none
      integer vel2,am_in_va2,location
      include 'vel_boxes.inc'

      data ireadbox,isavebox,ipickbox,ifunbox/0,0,0,0/
      data isetbox,ivfidbox,iheadbox/0,0,0/
      data iresbox,ilatbox,ilat2box,iraybox,idelbox/0,0,0,0,0/
      data imultbox,imiscbox,ioffmutebox,iinfobox/0,0,0,0/
      data read_filename,save_filename/' ',' '/
      data autosave/1/

      am_in_va = am_in_va2
      vel=location(vel2)
c     call f_autoname_workfile
      work_filename=' '
      return
      end
   


c-------------------------------------------------------------------------------



      subroutine get_vbstruct_pointer (vb2)
c     get the pointer to VbStruct from the common block and return to c.

      implicit none
      integer vb2,location
      include 'vel_boxes.inc'

      vb2=location(vel)    ! vel is first variable in common block vb.
      return
      end




c-------------------------------------------------------------------------------



      subroutine read_register (box)

      implicit none
      integer box,ident,index,nread                 ! arguments
      character*(*) text,endkey                     ! arguments
      integer dummy,old(*),buffer(*),error          ! arguments
      include 'vel_boxes.inc'
      character*80 msg,filename
      integer ierr,iexist
      integer sa,ichoice,choice(2)

ccc---------beginning of stuff needed for IBM.
        character*37 pr_replace,pr_add
        character*3  pr_nhx,pr_nhy
        character*12 pr_order
        character*20 pr_filename  
        character*8  pr_autosave  
        save         pr_replace,pr_add
        save         pr_nhx,pr_nhy
        save         pr_order
        save         pr_filename  
        save         pr_autosave  
        integer p0,p1,p2,p3,p4,m1,m2,m3,m4,m5 
        save    p0,p1,p2,p3,p4,m1,m2,m3,m4,m5 
        data    p0,p1,p2,p3,p4,m1,m2,m3,m4,m5/
     $           0, 1, 2, 3, 4,-1,-2,-3,-4,-5/
        data pr_replace /'Replace velocity functions in memory:'/
        data pr_add     /'Add to velocity functions in memory: '/
        data pr_nhx     /'NHX'/
        data pr_nhy     /'NHY'/
        data pr_order   /'   NMO ORDER'/
        data pr_filename/'Filename to read ...'/
        data pr_autosave/'autosave'/
ccc---------end of stuff needed for IBM.

      save    sa,ichoice,choice
      data    sa/1/

c     if (am_in_va.eq.1) sa=-1
      ireadbox=box
      call get_choice (52,  51,52,ichoice,choice)

cccc                   ID  PROMPT   ISW  VAR   ISW ROW COL NCHAR NDEC
      call nbox_ireg3 (51,pr_replace   ,p0, choice(1),p4,  1,15, 2, 0)
      call nbox_ireg3 (52,pr_add       ,p0, choice(2),p4,  2,15, 2, 0)
      call nbox_ireg  ( 0,                  nfun     ,m5,  1,58, 5, 0)
      call nbox_ireg  ( 0,                  nfun     ,m5,  2,58, 5, 0)
      call nbox_ireg2 (61,pr_NHX       ,p0, nhx      ,p1,  0,23, 2, 0)
      call nbox_ireg2 (62,pr_NHY       ,p0, nhy      ,sa, -1,-1, 2, 0)
      call nbox_ireg2(620,pr_order     ,p0, order    ,sa, -1,-1, 1, 0)
      call nbox_creg  ( 1,pr_filename                ,p2,  0, 1, 0, 0)
      call nbox_creg  ( 2,read_filename              ,p1,  0, 1, 0, 0)
      call nbox_ireg3 (68,pr_autosave  ,p0, autosave ,p3,  0,26, 2, 0)
      call nbox_mreg  (   work_filename,                  -1,-1)
      return


      entry read_trap (box,ident,index,   text,nread,endkey)

      if (endkey.eq.'ARRIVED') then
           if (ident.eq.68) then
             call fbox_maybe ('automatic file save option - see help')
           end if
      else if (ident.eq.1.and.endkey.eq.'RETURN') then
           call manage_read_filebox (vel)
           ident=2
      else if (ident.eq.2.and.nread.gt.0) then
           call f_read_inquire (read_filename,iexist,   msg,ierr)
      else if (ident.eq.68.and.nread.gt.0) then
           call f_autosave_workfile_cursor (0)
      else if (ident.eq.61.and.nread.gt.0) then
           call check_nhx (endkey)
      else if (ident.eq.62.and.nread.gt.0) then
           call check_nhy (endkey)
      else if (ident.eq.620.and.nread.gt.0) then
           call check_order (endkey)
      else if (endkey.eq.'RETURN') then
           call get_choice (ident,51,52,ichoice,choice)
      end if
      return


      entry read_getfile (dummy,old,buffer)
      call convert_hh2cc (buffer,0,   filename,0)
      if (filename.eq.' ') return
      read_filename=filename
      call f_read_inquire (read_filename,iexist,   msg,ierr)
      return


      entry read_apply (dummy,error)
      call f_read_velfile (.true.,read_filename,ichoice,   msg,error)
      return
      end




c-------------------------------------------------------------------------------


      subroutine check_nhx (endkey)
c     check nhx or nhy for invalid values.
c     print message and prepare for restore if error.
c     broadcast message if no error.

      implicit none
      character*(*) endkey 
      include 'vel_boxes.inc'

      if (nhx.lt.1) then
           call fbox_message('invalid X header (must be > 0) -'//
     $                                    ' reset to previous value')
           endkey='RESTORE'
           return
      end if
      call fbroadcast (vel,SENDER_WBOX,MESSAGE_XHEAD,0,0,0,0,0)
      return


      entry check_nhy (endkey)

      if (nhy.lt.1) then
           call fbox_message('invalid Y header (must be > 0 ) -'//
     $                                    ' reset to previous value')
           endkey='RESTORE'
           return
      end if
      call fbroadcast (vel,SENDER_WBOX,MESSAGE_YHEAD,0,0,0,0,0)
      return


      entry check_order (endkey)

      if (order.ne.2.and.order.ne.4) then
           call fbox_message('invalid NMO order (must be 2 or 4) -'//
     $                                    ' reset to previous value')
           endkey='RESTORE'
           return
      end if
      if (order.eq.2) then
           nhosign= 1.0
           nhoexp = 2.0
      else
           nhosign=-1.0
           nhoexp = 4.0
      end if
      call fbroadcast (vel,SENDER_WBOX,MESSAGE_ORDER,0,0,0,0,0)
      return
      end


c-------------------------------------------------------------------------------

      subroutine check_xbin (i,endkey,doverify)
c     check xbin or ybin for invalid values.
c     print message and prepare for restore if error.
c     broadcast message if no error.

      implicit none
      integer i,ierr
      character*(*) endkey 
      integer doverify
      include 'vel_boxes.inc'

      ierr=0
      if (doverify.ge.1) call verify_xbin (i,   ierr)
      if (ierr.ne.0) then
           call fbox_message('invalid XBIN - reset to previous value')
           endkey='RESTORE'
           return
      end if
      call fbroadcast (vel,SENDER_WBOX,MESSAGE_XBIN,i,0,0,0,0)
      return


      entry check_ybin (i,endkey,doverify)

      ierr=0
      if (doverify.ge.1) call verify_ybin (i,   ierr)
      if (ierr.ne.0) then
           call fbox_message('invalid YBIN - reset to previous value')
           endkey='RESTORE'
           return
      end if
      call fbroadcast (vel,SENDER_WBOX,MESSAGE_YBIN,i,0,0,0,0)
      return
      end



c-------------------------------------------------------------------------------


      subroutine save_register (box)

      implicit none
      integer box,ident,index,nread,dummy,error     ! arguments
      character*(*) text,endkey                     ! arguments
      include 'vel_boxes.inc'
      character*80 msg
      character*4 type2
      integer ierr,iexist

      integer      ichoice,choice(3)
      integer      itypeflag,typeflag(numtypes+1)
      save         ichoice,choice   
      save         itypeflag,typeflag

ccc---------beginning of stuff needed for IBM.
      character*33 pr_selected,pr_all,pr_active
      save         pr_selected,pr_all,pr_active
      character*57 pr_longline
      save         pr_longline
      character*25 pr_type
      save         pr_type
      character*30 pr_versus,pr_time
      save         pr_versus,pr_time
      character*9  pr_thick
      save         pr_thick
      character*18 pr_nmo,pr_rms,pr_av,pr_int,pr_depth
      save         pr_nmo,pr_rms,pr_av,pr_int,pr_depth
      character*11 pr_mixed
      save         pr_mixed
      character*31 pr_errors 
      save         pr_errors 
      character*11 pr_raytraced
      save         pr_raytraced
      character*17 pr_filename
      save         pr_filename
      character*8  pr_autosave
      save         pr_autosave
      integer p0,p1,p2,p3,p4,m1,m2,m3,m4,m5 
      save    p0,p1,p2,p3,p4,m1,m2,m3,m4,m5
      data    p0,p1,p2,p3,p4,m1,m2,m3,m4,m5/
     $         0, 1, 2, 3, 4,-1,-2,-3,-4,-5/
      data pr_selected /'Save selected velocity functions:'/
      data pr_all      /'Save all velocity functions:     '/
      data pr_active   /'Save active velocity function #: '/
      data pr_longline /
     $    '---------------------------------------------------------'/
      data pr_type   /'Type of velocity to save:'/
      data pr_versus /'  versus     versus   versus  '/
      data pr_time   /'2-way time    depth    layer  '/
      data pr_thick  /'thickness'/
      data pr_nmo    /'     NMO velocity:'/
      data pr_rms    /'     RMS velocity:'/
      data pr_av     /' Average velocity:'/
      data pr_int    /'Interval velocity:'/
      data pr_depth  /'            Depth:'/
      data pr_mixed  /'mixed types'/
      data pr_errors   /'velocity functions with errors:'/
      data pr_raytraced/' raytraced:'/
      data pr_filename /'Filename to save:'/
      data pr_autosave /'autosave'/
ccc---------end of stuff needed for IBM.

      isavebox=box
      call get_choice (52,  51,53,ichoice,choice)
      call get_choice ( 1,   1,numtypes+1,itypeflag,typeflag)

cccc                   ID  PROMPT   ISW  VAR   ISW ROW COL NCHAR NDEC
      call nbox_ireg3 (51,pr_selected, p0, choice(1),p4,    1,19,2,0)
      call nbox_ireg3 (52,pr_all,      p0, choice(2),p4,    2,19,2,0)
      call nbox_ireg3 (53,pr_active,   p0, choice(3),p4,    3,19,2,0)
      call nbox_ireg (0,nselect, m5,1,56,5,0)
      call nbox_ireg (0,nfun   , m5,2,56,5,0)
      call nbox_ireg (0,ifun   , m5,3,56,5,0)
      call nbox_mreg (pr_longline,0,12)
      call nbox_mreg (pr_type   , 0,16)
      call nbox_mreg (pr_versus , 6,35)
      call nbox_mreg (pr_time   , 0,35)
      call nbox_mreg (pr_thick  , 0,56)
      call nbox_mreg (pr_nmo    , 8,16)
      call nbox_mreg (pr_rms    , 0,16)
      call nbox_mreg (pr_av     , 0,16)
      call nbox_mreg (pr_int    , 0,16)
      call nbox_mreg (pr_depth  , 0,16)
      call nbox_ireg3 ( 1,veltype( 1), p0, typeflag( 1),p4,  8,36,2,0)
      call nbox_ireg3 ( 2,veltype( 2), p0, typeflag( 2),p4,  9,36,2,0)
      call nbox_ireg3 ( 3,veltype( 3), p0, typeflag( 3),p4,  9,46,2,0)
      call nbox_ireg3 ( 4,veltype( 4), p0, typeflag( 4),p4,  9,56,2,0)
      call nbox_ireg3 ( 5,veltype( 5), p0, typeflag( 5),p4, 10,36,2,0)
      call nbox_ireg3 ( 6,veltype( 6), p0, typeflag( 6),p4, 10,46,2,0)
      call nbox_ireg3 ( 7,veltype( 7), p0, typeflag( 7),p4, 10,56,2,0)
      call nbox_ireg3 ( 8,veltype( 8), p0, typeflag( 8),p4, 11,36,2,0)
      call nbox_ireg3 ( 9,veltype( 9), p0, typeflag( 9),p4, 11,46,2,0)
      call nbox_ireg3 (10,veltype(10), p0, typeflag(10),p4, 11,56,2,0)
      call nbox_ireg3 (11,veltype(11), p0, typeflag(11),p4, 12,36,2,0)
      call nbox_ireg3 (12,pr_mixed   , p0, typeflag(12),p4, 13,42,2,0)
ccc                        the 12 above should be numtypes+1
      call nbox_mreg (pr_longline,0,12)
      call nbox_ireg2 (66,pr_errors    ,p0, nerrmsg,  m5,   0,16, 6, 0)
      call nbox_ireg2 (67,pr_raytraced ,p0, nraymsg,  m5,  -1,-1 ,6, 0)
      call nbox_mreg  (   pr_filename  ,                    0, 1)
      call nbox_creg  (20,save_filename,p1,                 0, 1, 0, 0)
      call nbox_ireg3 (68,pr_autosave  ,p0,autosave,  p3,   0,26, 2, 0)
      call nbox_mreg  (   work_filename,                   -1,-1)
      return


      entry save_trap (box,ident,index,   text,nread,endkey)

      if (endkey.eq.'REDRAW') then
           continue
      else if (endkey.eq.'ARRIVED') then
           if (ident.ge.1.and.ident.le.numtypes) then
             call show_velfun_type (veltype(ident))
           else if (ident.eq.12) then
             call fbox_maybe ('save specified type for each function')
           else if (ident.eq.68) then
             call fbox_maybe ('automatic file save option - see help')
           end if
      else if (ident.eq.20.and.nread.gt.0.and.endkey.eq.'RETURN') then
           type2=' '
           if (itypeflag.le.numtypes) type2=veltype(itypeflag)
           call f_save_velfile
     $                (.true.,save_filename,ichoice,type2,   msg,ierr)
      else if (ident.eq.20.and.nread.gt.0) then
           call f_save_inquire (save_filename,iexist,   msg,ierr)
      else if (ident.eq.68.and.nread.gt.0) then
           call f_autosave_workfile_cursor (0)
      else if (endkey.eq.'RETURN') then
           call get_choice (ident,51,53,ichoice,choice)
           call get_choice (ident, 1,numtypes+1,itypeflag,typeflag)
      end if
      return


      entry save_apply (dummy,error)
      type2=' '
      if (itypeflag.le.numtypes) type2=veltype(itypeflag)
      call f_save_velfile
     $                (.true.,save_filename,ichoice,type2,   msg,error)
      return
      end





c-------------------------------------------------------------------------------


      subroutine pick_register (box)

      implicit none
      integer box,ident,index,nread,dummy,error     ! arguments
      character*(*) text,endkey                     ! arguments
      include 'vel_boxes.inc'
      character*80 msg
      character*1 raykeep,descrip
      integer p,ierr,itype,ntypes
      integer give_to_mike,mike_flag
      integer ident2,index2,istat    ! needed for restoring previous value.
CXXX  real fvar_keep                 ! needed for restoring previous value.
      integer doverify
      save    doverify

      real times_keep,vnmo_keep
      save times_keep,vnmo_keep
      character*1 chng
      integer sb,pw0,sw0,sw1,sw2,quickflag,invoke,ks1,ks3,ks7,sw(7)
      save    sb,pw0,sw0,sw1,sw2,quickflag,invoke,ks1,ks3,ks7,sw   ,chng

ccc---------beginning of stuff needed for IBM.
      character*8  pr_function
      save         pr_function
      character*2  pr_of
      save         pr_of
      character*3  pr_nhx,pr_nhy
      save         pr_nhx,pr_nhy
      character*9  pr_order
      save         pr_order
      character*18 pr_invoke
      save         pr_invoke
      character*4  pr_name,pr_xbin,pr_ybin,pr_type
      save         pr_name,pr_xbin,pr_ybin,pr_type
      character*5  pr_picks
      save         pr_picks
      character*11 pr_startvel,pr_bottom,pr_timetol,pr_depthtol
      save         pr_startvel,pr_bottom,pr_timetol,pr_depthtol
      character*7  pr_project,pr_line,pr_rdate,pr_pdate,pr_userid
      save         pr_project,pr_line,pr_rdate,pr_pdate,pr_userid
      character*7  pr_comment
      save         pr_comment
      character*10 pr_selected,pr_error,pr_raytraced
      save         pr_selected,pr_error,pr_raytraced
      character*1  pr_blank
      save         pr_blank
      character*5  pr_depth,pr_time
      save         pr_depth,pr_time
      character*7  pr_nmo,pr_rms,pr_av,pr_int,pr_maxoff
      save         pr_nmo,pr_rms,pr_av,pr_int,pr_maxoff
      character*9  pr_thick
      save         pr_thick
      integer nmax2,p0,p1,p2,p3,p4,m1,m2,m3,m4,m5,m44
      save    nmax2,p0,p1,p2,p3,p4,m1,m2,m3,m4,m5,m44
      data    nmax2,p0,p1,p2,p3,p4,m1,m2,m3,m4,m5,m44/
     $        nmax , 0, 1, 2, 3, 4,-1,-2,-3,-4,-5,-44/
      data pr_function /'function'/
      data pr_of       /'of'/
      data pr_nhx      /'NHX'/
      data pr_nhy      /'NHY'/
      data pr_order    /'NMO ORDER'/
      data pr_invoke   /'Invoke ray tracing'/
      data pr_name     /'NAME'/
      data pr_xbin     /'XBIN'/
      data pr_ybin     /'YBIN'/
      data pr_type     /'type'/
      data pr_picks    /'picks'/
      data pr_startvel /'   startvel'/
      data pr_bottom   /' bottomtime'/
      data pr_timetol  /'    timetol'/
      data pr_depthtol /'   depthtol'/
      data pr_project  /'project'/
      data pr_line     /'   line'/
      data pr_rdate    /'  rdate'/
      data pr_pdate    /'  pdate'/
      data pr_userid   /' userid'/
      data pr_comment  /'comment'/
      data pr_selected /' selected:'/
      data pr_error    /'    error:'/
      data pr_raytraced/'raytraced:'/
      data pr_blank    /' '/
      data pr_depth    /'depth'/
      data pr_time     /'time '/
      data pr_nmo      /'NMO VEL'/
      data pr_rms      /'RMS VEL'/
      data pr_av       /'AV VEL '/
      data pr_int      /'INT VEL'/
      data pr_maxoff   /'max off'/
      data pr_thick    /'thickness'/
ccc---------end of stuff needed for IBM.

      data    sb,pw0,sw0,sw1,sw2,quickflag/1,0,1,3,-5,0/
      data    ks1,ks3,ks7,sw/2,2,2,7*1/
ccc   data    doverify/1/        ! removed  5/23/97 due to problems.
      data    doverify/0/        ! inserted 5/23/97

      ipickbox=box
      if (am_in_va.eq.1) sb=-5

cccc                  ID  PROMPT      ISW  VAR       ISW  ROW COL NCHAR NDEC
      call nbox_ireg2 ( 1,pr_function ,p0, ifun   ,   p1,  1, 1,4,0)
      call nbox_ireg2 (90,pr_of       ,p0, nfun   ,   m5,  1,-1,4,0)
      call nbox_ireg2 (51,pr_nhx      ,p0, nhx    ,   m5,  1,26,2,0)
      call nbox_ireg2 (52,pr_nhy      ,p0, nhy    ,   m5,  1,33,2,0)
      call nbox_ireg2 (620,pr_order   ,p0, order  ,   m5,  1,41,1,0)
      call nbox_ireg3 (53,pr_invoke   ,pw0,invoke,    sw1, 1,57,2,0)
      call nbox_creg2 ( 2,pr_name     ,p0, vfid,      p1,  2, 1,0,0)
      call nbox_freg2 ( 8,pr_xbin     ,pw0,xbin,      sw0, 2,22,8,4)
      call nbox_freg2 ( 9,pr_ybin     ,pw0,ybin,      sw0, 2,36,8,4)
      call nbox_ireg3 ( 7,pr_picks    ,p0, n   ,      m5,  2,56,3,0)
      call nbox_creg2 (10,pr_type     ,p0, type,      p2,  2,67,0,0)
      call nbox_freg2 (71,pr_startvel ,p0, startvel,  sb,  1,80,5,0)
      call nbox_freg2 (72,pr_bottom   ,p0, bottomtime,sb,  0, 0,5,3)
      call nbox_freg2 (73,pr_timetol  ,p0, timetol,   sb,  0, 0,5,3)
      call nbox_freg2 (74,pr_depthtol ,p0, depthtol,  sb,  0, 0,5,0)
      call nbox_creg2 (13,pr_project  ,pw0,project,   sw0, 0,74,0,0)
      call nbox_creg2 (14,pr_line     ,pw0,line  ,    sw0, 0, 0,0,0)
      call nbox_creg2 (15,pr_rdate    ,pw0,rdate ,    sw0, 0, 0,0,0)
      call nbox_creg2 (16,pr_pdate    ,pw0,pdate ,    sw0, 0, 0,0,0)
      call nbox_creg2 (17,pr_userid   ,pw0,userid,    sw0, 0, 0,0,0)
      call nbox_creg2 (18,pr_comment  ,pw0,comment,   sw0, 0, 0,0,0)
      call nbox_creg2 (30,pr_selected ,pw0,select ,   sw2, 0,78,0,0)
      call nbox_creg2 (31,pr_error    ,pw0,errmsg ,   sw2, 0, 0,0,0)
      call nbox_creg2 (32,pr_raytraced,pw0,raymsg ,   sw2, 0, 0,0,0)
      call nbox_creg  (29,pr_blank    , p2,                3,12,0,0)

cccc                  N NMAX IROW NROW
      call fbox_rega (n,nmax2,3,   30)
cccc                   ID  PROMPT     ISW  VAR     ISW     COL NCHAR NDEC
      call nbox_xrega (20,chng       ,p2,  n     ,  m44   ,  0,  4,  0)
      call nbox_frega (21,pr_depth   ,ks1, depths, sw(1)  ,  0,  7,  0)
      call nbox_frega (22,pr_time    ,p2,  times , sw(2)  ,  0,  5,  3)
      call nbox_frega (27,pr_nmo     ,ks7, vnmo  , sw(7)  , 21,  7,  0)
      call nbox_frega (24,pr_rms     ,p2,  vrms  , sw(4)  ,  0,  7,  0)
      call nbox_frega (25,pr_av      ,p2,  vav   , sw(5)  ,  0,  7,  0)
      call nbox_frega (26,pr_int     ,p2,  vint  , sw(6)  ,  0,  7,  0)
      call nbox_frega (23,pr_thick   ,ks3, thick , sw(3)  , 54,  7,  0)
      call nbox_frega (28,pr_maxoff  ,p0, offpick,   m5   ,  0,  6,  0)

      call register_active_velfun
      call copy_velfun (ifun,nfunmax+2)
      call fbox_set_focus (ipickbox,21,1)
      call adjust_arrow_sensitivity (vel)
      return


      entry pick_trap (box,ident,index,   text,nread,endkey)

      mike_flag=0
      if (ident.ge.21.and.ident.le.27.and.type(ifun).eq.'VTNM') then
           call offset_index_words (p,depths,point(ifun))
           p=p+index
           if (endkey.eq.'REMOVE') then
                times_keep=times(p)
                vnmo_keep = vnmo(p)
           else if (endkey.eq.'REMOVED') then
                mike_flag=give_to_mike(index,times_keep,vnmo_keep)
           else if (endkey.eq.'INSERTED') then
                mike_flag=give_to_mike(index,times(p),vnmo(p))
           else if (nread.gt.0) then
c                              print *, '----------'
c             if (ident.eq.21) print *, 'depths(',index,') =',depths(p)
c             if (ident.eq.22) print *, 'depths(',index,') =',times (p)
c             if (ident.eq.23) print *, 'depths(',index,') =',thick (p)
c             if (ident.eq.24) print *, 'depths(',index,') =',vrms  (p)
c             if (ident.eq.25) print *, 'depths(',index,') =',vav   (p)
c             if (ident.eq.26) print *, 'depths(',index,') =',vint  (p)
c             if (ident.eq.27) print *, 'depths(',index,') =',vnmo  (p)
CXXX            if (ident.eq.21) fvar_keep = depths(p)
CXXX            if (ident.eq.22) fvar_keep = times (p)
CXXX            if (ident.eq.23) fvar_keep = thick (p)
CXXX            if (ident.eq.24) fvar_keep = vrms  (p)
CXXX            if (ident.eq.25) fvar_keep = vav   (p)
CXXX            if (ident.eq.26) fvar_keep = vint  (p)
CXXX            if (ident.eq.27) fvar_keep = vnmo  (p)
CXXX            if (ident.eq.21) call get_previous_fvar (depths(p))
CXXX            if (ident.eq.22) call get_previous_fvar (times (p))
CXXX            if (ident.eq.23) call get_previous_fvar (thick (p))
CXXX            if (ident.eq.24) call get_previous_fvar (vrms  (p))
CXXX            if (ident.eq.25) call get_previous_fvar (vav   (p))
CXXX            if (ident.eq.26) call get_previous_fvar (vint  (p))
CXXX            if (ident.eq.27) call get_previous_fvar (vnmo  (p))
                call restore_previous_value (box, ident2,index2,istat)
c             if (ident.eq.21) print *, 'depths(',index,') =',depths(p)
c             if (ident.eq.22) print *, 'depths(',index,') =',times (p)
c             if (ident.eq.23) print *, 'depths(',index,') =',thick (p)
c             if (ident.eq.24) print *, 'depths(',index,') =',vrms  (p)
c             if (ident.eq.25) print *, 'depths(',index,') =',vav   (p)
c             if (ident.eq.26) print *, 'depths(',index,') =',vint  (p)
c             if (ident.eq.27) print *, 'depths(',index,') =',vnmo  (p)
                mike_flag=give_to_mike(index,times(p),vnmo(p))
                call restore_previous_value (box, ident2,index2,istat)
CXXX            if (ident.eq.21) depths(p) = fvar_keep
CXXX            if (ident.eq.22) times (p) = fvar_keep
CXXX            if (ident.eq.23) thick (p) = fvar_keep
CXXX            if (ident.eq.24) vrms  (p) = fvar_keep
CXXX            if (ident.eq.25) vav   (p) = fvar_keep
CXXX            if (ident.eq.26) vint  (p) = fvar_keep
CXXX            if (ident.eq.27) vnmo  (p) = fvar_keep
c             if (ident.eq.21) print *, 'depths(',index,') =',depths(p)
c             if (ident.eq.22) print *, 'depths(',index,') =',times (p)
c             if (ident.eq.23) print *, 'depths(',index,') =',thick (p)
c             if (ident.eq.24) print *, 'depths(',index,') =',vrms  (p)
c             if (ident.eq.25) print *, 'depths(',index,') =',vav   (p)
c             if (ident.eq.26) print *, 'depths(',index,') =',vint  (p)
c             if (ident.eq.27) print *, 'depths(',index,') =',vnmo  (p)
c                              print *, '----------'
           end if
      end if

      if (endkey.eq.'REDRAW') then
           invoke=0
           if (raymsg(ifun).ne.' ') invoke=1
           call get_switches_from_type (type(ifun),   sw,ks1,ks3,ks7)
           call adjust_arrow_sensitivity (vel)  ! maybe not needed here
           if (ifun.gt.nfun) then
                pw0=-55
                sw0=-22
                sw1=-3
                sw2=-77
           else
                pw0=0
                sw0=1
                sw1=3
                sw2=-5
           end if
           call convert_ii2cc (changeflag,chng,1)
           return

      else if (endkey.eq.'ARRIVED') then
           if (ident.eq.-20)
     $      call fbox_maybe ('data change flag - see help')
           if (ident.ge.-23.and.ident.le.-21.and.quickflag.eq.0)
     $      call fbox_maybe
     $      ('press to select this depth/time/thickness for data entry')
           if (ident.ge.-27.and.ident.le.-24.and.quickflag.eq.0)
     $      call fbox_maybe
     $             ('press to select this velocity type for data entry')
           if (ident.eq.29.and.quickflag.eq.0) call fbox_maybe
     $                  ('press to select DEPTH and TIME data entry')
           if (ident.eq.10) call show_velfun_type (type(ifun))
           quickflag=0
           return

      else if (ident.eq.10.and.endkey.eq.'RETURN') then
           call increment_velfun_type (ifun)
           call pick_undo_sensitize (vel,1)

      else if (ident.eq.-20.and.endkey.eq.'RETURN') then
           call f_autosave_workfile_cursor (0)

      else if (ident.eq.29.and.endkey.eq.'RETURN') then
           type(ifun)='VTDP'
           call show_velfun_type (type(ifun))
           call pick_undo_sensitize (vel,1)
           quickflag=1

      else if (ident.ge.-23.and.ident.le.-21.and.endkey.eq.'RETURN')then
           if (type(ifun).ne.'VTDP') then
                sw(1)=-1
                sw(2)=-1
                sw(3)=-1
                sw(-ident-20)=1
                call get_type_from_switches (sw,   type(ifun))
           end if
           quickflag=1
           call show_velfun_type (type(ifun))
           call pick_undo_sensitize (vel,1)

      else if (ident.ge.-27.and.ident.le.-24.and.endkey.eq.'RETURN')then
           if (type(ifun).eq.'VTDP') sw(1)=-1
           sw(4)=-1
           sw(5)=-1
           sw(6)=-1
           sw(7)=-1
           sw(-ident-20)=1
           call get_type_from_switches (sw,   type(ifun))
           call show_velfun_type (type(ifun))
           quickflag=1
           call pick_undo_sensitize (vel,1)

      else if (ident.ge.21.and.ident.le.27.and.index.eq.1.and.
     $                   n(ifun).ge.2.and.endkey.eq.'REMOVE') then
           call fbox_message ('CANNOT REMOVE FIRST ROW'//
     $                               ' UNLESS IT IS THE ONLY ROW')
           endkey=' '

      else if (ident.eq.8.and.nread.gt.0) then
           call check_xbin (ifun,endkey,doverify)
           call pick_undo_sensitize (vel,1)

      else if (ident.eq.9.and.nread.gt.0) then
           call check_ybin (ifun,endkey,doverify)
           call pick_undo_sensitize (vel,1)

      else if ( (nread.gt.0.and.ident.ge.21.and.ident.le.27) .or.
     $          (nread.gt.0.and.ident.eq.53) ) then
           if (ident.eq.53.and.n(ifun).le.1) then
                raymsg(ifun)=' '
                endkey=' '
                return
           end if
           if (ident.ge.21.and.ident.le.27.and.index.gt.n(ifun).and.
     $                                       endkey.ne.'REMOVED') then
                call offset_index_words (p,depths,point(ifun))
                p=p+index
                if (ident.ne.21) depths(p)=fnil
                if (ident.ne.22) times (p)=fnil
                if (ident.ne.23) thick (p)=fnil
                if (ident.ne.24) vrms  (p)=fnil
                if (ident.ne.25) vav   (p)=fnil
                if (ident.ne.26) vint  (p)=fnil
                if (ident.ne.27) vnmo  (p)=fnil
                n(ifun)=index
           end if
           raykeep=raymsg(ifun)
           if (ident.eq.22.and.index.ge.n(ifun).and.
     $           endkey.ne.'INSERTED'.and.endkey.ne.'REMOVED'.and.
     $           endkey.ne.'RESTORED')
     $                   call f_maybe_truncate_velfun (ifun,type(ifun))
           call update_velfun (ifun,invoke,type(ifun),   msg,ierr)
           call pick_undo_sensitize (vel,1)
           call velfun_type_cc2ii (type(ifun),   itype,descrip,ntypes)
           if (mike_flag.eq.1) then
                if (endkey.eq.'INSERTED') then
                    call fbroadcast(vel,SENDER_WBOX,MESSAGE_MODIFY,
     $                               itype-1,ifun,ifun,index,-n(ifun))
                else if (endkey.eq.'REMOVED') then
                    call fbroadcast(vel,SENDER_WBOX,MESSAGE_MODIFY,
     $                               itype-1,ifun,ifun,-index,-n(ifun))
                else
                    call fbroadcast(vel,SENDER_WBOX,MESSAGE_MODIFY,
     $                               itype-1,ifun,ifun,index,index)
                end if
           else
                call fbroadcast(vel,SENDER_WBOX,MESSAGE_MODIFY,
     $                               itype-1,ifun,ifun,1,n(ifun))
           end if
           if (ierr.eq.0.and.raymsg(ifun).ne.raykeep) then
                if (raymsg(ifun).eq.' ') then
                     call fbox_message ('ray tracing cancelled')
                else
                     call fbox_message ('ray tracing invoked')
                end if
           end if
           if (ierr.ne.0.and.index.le.n(ifun).and.n(ifun).ge.2) then
                call fbox_message (msg)
           end if
      else if(nread.gt.0.and.ident.eq.1) then
           ifun=max(1,min(ifun,nfun+1,nfunmax))
           call f_autosave_workfile_cursor (0)
           call f_activate_velfun (ifun)
           call fbox_set_focus (ifunbox,14,ifun)
           call force_updates
           call fbroadcast (vel,SENDER_WBOX,MESSAGE_ACTIVE,0,0,0,0,0)
           return
      end if

      if (nread.gt.0.and.ifun.eq.nfun+1.and.ifun.le.nfunmax.and.
     $            (ident.lt.71.or.ident.gt.74).and.ident.ne.53) then
           call alloc_last_velfun (msg,ierr)
           call fbox_message (msg)
           if (ierr.eq.0) then
cccc            errmsg(ifun)='E'
                xbin(ifun)=0.
                ybin(ifun)=0.
                if (ifun.gt.1) ybin(ifun)=ybin(ifun-1)
                if(doverify.ge.1) call verify_ybin (ifun,   ierr)
                if(doverify.ge.1) call verify_xbin (ifun,   ierr)
                call fbroadcast (vel,SENDER_WBOX,MESSAGE_INSERT,nfun,
     $                                             0,0,0,0)
           else
                endkey='RESTORE'  ! probably will not work right.
           end if
           call adjust_arrow_sensitivity (vel)
      end if
      if (nread.gt.0.and.(ident.eq.2.or.ident.eq.8.or.ident.eq.9.or.
     $            (ident.ge.13.and.ident.le.18))) then
           changeflag=1
           call pick_undo_sensitize (vel,1)
      end if
      return


      entry pick_apply (dummy,error)
      error=0
      return

      entry pick_prev (dummy)
      if (ifun.gt.1) then
           call f_autosave_workfile_cursor (0)
           call f_activate_velfun (ifun-1)
           call fbox_set_focus (ifunbox,14,ifun)
           call fbox_event (ipickbox,'hello')
           call force_updates
           call fbroadcast (vel,SENDER_WBOX,MESSAGE_ACTIVE,0,0,0,0,0)
      end if
      return

      entry pick_next (dummy)
      if (ifun.lt.min(nfun+1,nfunmax)) then
           call f_autosave_workfile_cursor (0)
           call f_activate_velfun (ifun+1)
           call fbox_set_focus (ifunbox,14,ifun)
           call fbox_event (ipickbox,'hello')
           call force_updates
           call fbroadcast (vel,SENDER_WBOX,MESSAGE_ACTIVE,0,0,0,0,0)
      end if
      return

      entry pick_undo (dummy)
      call copy_velfun (nfunmax+2,ifun)
      call register_active_velfun
      call fbroadcast(vel,SENDER_WBOX,MESSAGE_MODIFY,
     $                           999,ifun,ifun,1,n(ifun))
      call fbox_event (ipickbox,'hello')       ! maybe fbox_update is enuf?
      call force_updates
      call fbox_messageline (ipickbox,'velocity function restored')
      return
      end







      subroutine get_switches_from_type (type,   sw,ks1,ks3,ks7)

      implicit none
      integer sw(7),ks1,ks3,ks7,i
      character*(*) type

      do i=1,7
           sw(i)=-1
      end do
      if (type(1:2).eq.'VZ'.or.type.eq.'VTDP') sw(1)=1
      if (type(1:2).eq.'VT'                  ) sw(2)=1
      if (type(1:2).eq.'VL'                  ) sw(3)=1
      if (type(3:4).eq.'RM'                  ) sw(4)=1
      if (type(3:4).eq.'AV'                  ) sw(5)=1
      if (type(3:4).eq.'IN'                  ) sw(6)=1
      if (type(3:4).eq.'NM'                  ) sw(7)=1
      ks1=2
      ks3=2
      ks7=2
      if (sw(7).eq.1                  ) ks1=-2  ! depth     prompt.
      if (sw(7).eq.1.or.type.eq.'VTDP') ks3=-2  ! thickness prompt.
      if (sw(2).ne.1                  ) ks7=-2  ! vnmo      prompt.
      return
      end






      subroutine get_type_from_switches (sw,   type)

      implicit none
      integer sw(7)
      character*(*) type

      type='XXXX'
      if (sw(1).eq.1) type(1:2)='VZ'
      if (sw(2).eq.1) type(1:2)='VT'
      if (sw(3).eq.1) type(1:2)='VL'
      if (sw(1).eq.1) type(3:4)='DP'
      if (sw(4).eq.1) type(3:4)='RM'
      if (sw(5).eq.1) type(3:4)='AV'
      if (sw(6).eq.1) type(3:4)='IN'
      if (sw(7).eq.1) type(3:4)='NM'
      return
      end





c-------------------------------------------------------------------------------


      subroutine fun_register (box)

      implicit none
      integer box,ident,index,nread,dummy,error     ! arguments
      character*(*) text,endkey                     ! arguments
      include 'vel_boxes.inc'
      character*80 msg
      integer ierr
      integer doverify
      save    doverify

      character*1 chng
      integer sa,xchoice(3),ychoice(3)
      save    sa,xchoice,ychoice,chng

ccc---------beginning of stuff needed for IBM.
      character*28 pr_selected
      save         pr_selected
      character*13 pr_errors
      save         pr_errors
      character*11 pr_raytraced
      save         pr_raytraced
      character*15 pr_active
      save         pr_active
      character*2  pr_of
      save         pr_of
      character*9  pr_nhx
      character*3  pr_nhy
      save         pr_nhx,pr_nhy
      character*12 pr_order
      save         pr_order
      character*26 pr_doverify  
      save         pr_doverify  
      character*7  pr_xsort,pr_ysort
      save         pr_xsort,pr_ysort
      character*4  pr_incr,pr_decr,pr_name,pr_xbin,pr_ybin,pr_type
      save         pr_incr,pr_decr,pr_name,pr_xbin,pr_ybin,pr_type
      character*6  pr_either,pr_select
      save         pr_either,pr_select
      character*1  pr_blank,pr_e,pr_r
      save         pr_blank,pr_e,pr_r
      character*4  pr_line
      save         pr_line
      character*5  pr_picks,pr_rdate,pr_pdate
      save         pr_picks,pr_rdate,pr_pdate
      character*2  pr_id
      save         pr_id
      character*7  pr_project,pr_comment
      save         pr_project,pr_comment
      integer nfunmax2,p0,p1,p2,p3,p4,p5,m1,m2,m3,m4,m5,m44,m99
      save    nfunmax2,p0,p1,p2,p3,p4,p5,m1,m2,m3,m4,m5,m44,m99
      data    nfunmax2,p0,p1,p2,p3,p4,p5,m1,m2,m3,m4,m5,m44,m99/
     $        nfunmax , 0, 1, 2, 3, 4, 5,-1,-2,-3,-4,-5,-44,-99/
      data pr_selected /'velocity functions selected:'/
      data pr_errors   /' with errors:'/
      data pr_raytraced/' raytraced:'/
      data pr_active   /'active function'/
      data pr_of       /'of'/
      data pr_nhx      /'      NHX'/
      data pr_nhy      /'NHY'/
      data pr_order    /'   NMO ORDER'/
      data pr_doverify /'enable bin auto-adjustment'/
      data pr_xsort    /'X sort:'/
      data pr_ysort    /'Y sort:'/
      data pr_incr     /'incr'/
      data pr_decr     /'decr'/
      data pr_name     /'NAME'/
      data pr_xbin     /'XBIN'/
      data pr_ybin     /'YBIN'/
      data pr_type     /'TYPE'/
      data pr_either   /'either'/
      data pr_select   /'select'/
      data pr_blank    /' '/
      data pr_e        /'E'/
      data pr_r        /'R'/
      data pr_line     /'line'/
      data pr_picks    /'picks'/
      data pr_rdate    /'rdate'/
      data pr_pdate    /'pdate'/
      data pr_id       /'id'/
      data pr_project  /'project'/
      data pr_comment  /'comment'/
ccc---------end of stuff needed for IBM.

      data    sa,xchoice,ychoice/1, 0,0,1, 0,0,1/
ccc   data    doverify/1/            ! removed  5/23/97 due to problems.
      data    doverify/0/            ! inserted 5/23/97.

c     if (am_in_va.eq.1) sa=-1
      ifunbox=box
      call get_sort_choice (xchoice,ychoice)

cccc                   ID  PROMPT   ISW  VAR   ISW ROW COL NCHAR NDEC
      call nbox_ireg2 (65,pr_selected , p0, nselect, m5,   1, 1, 6, 0)
      call nbox_ireg2 (66,pr_errors   , p0, nerrmsg, m5,  -1,-1, 6, 0)
      call nbox_ireg2 (67,pr_raytraced, p0, nraymsg, m5,  -1,-1, 6, 0)
      call nbox_ireg2 (63,pr_active   , p0, ifun   , p1,   2, 1, 4, 0)
      call nbox_ireg2 (64,pr_of       , p0, nfun   , m5,  -1,-1, 4, 0)
      call nbox_ireg2 (61,pr_nhx      , p0, nhx    , p1,  -1,-1, 2, 0)
      call nbox_ireg2 (62,pr_nhy      , p0, nhy    , sa,  -1,-1, 2, 0)
      call nbox_ireg2 (620,pr_order   , p0, order  , m5,  -1,-1, 1, 0)
ccc   call nbox_ireg3 (97,pr_doverify , p0, doverify,p3,   3,25, 2, 0) !5/23/97

      call nbox_mreg  (pr_xsort   , 1,79)
      call nbox_ireg3 (331,pr_incr    , p0, xchoice(1),p4,  1, 88,2,0)
      call nbox_ireg3 (332,pr_decr    , p0, xchoice(2),p4,  1, 97,2,0)
      call nbox_ireg3 (333,pr_either  , p0, xchoice(3),p4,  1,106,2,0)
      call nbox_mreg  (pr_ysort   , 2,79)
      call nbox_ireg3 (334,pr_incr    , p0, ychoice(1),p4,  2, 88,2,0)
      call nbox_ireg3 (335,pr_decr    , p0, ychoice(2),p4,  2, 97,2,0)
      call nbox_ireg3 (336,pr_either  , p0, ychoice(3),p4,  2,106,2,0)

cccc                   N    NMAX   IROW NROW
ccc   call fbox_rega (nfun,nfunmax2,4,   30)  ! removed  5/23/97
      call fbox_rega (nfun,nfunmax2,3,   30)  ! inserted 5/23/97
cccc                   ID  PROMPT     ISW  VAR     ISW     COL NCHAR NDEC
      call nbox_xrega (12,chng       ,p2  , n     , m44   ,  1,  4,   0)
      call nbox_irega (13,pr_blank   ,m99 , point ,  m99  ,  1,  4,   0)
      call nbox_irega (14,pr_blank   ,p0  , kode  ,  p4   ,  0,  2,   0)
      call nbox_crega (1 ,pr_name    ,p0  , vfid  ,  p1   ,  0,  0,   0)
      call nbox_crega (20,pr_select  ,p0  , select,  p2   ,  0,  0,   0)
      call nbox_frega (2 ,pr_xbin    ,p0  , xbin  ,  p1   ,  0,  8,   4)
      call nbox_frega (3 ,pr_ybin    ,p0  , ybin  ,  p1   ,  0,  8,   4)
      call nbox_crega (4 ,pr_type    ,p0  , type  ,  p2   ,  0,  0,   0)
      call nbox_crega (21,pr_e       ,p0  , errmsg,  p5   ,  0,  0,   0)
      call nbox_crega (22,pr_r       ,p0  , raymsg,  p5   ,  0,  0,   0)
      call nbox_irega (5 ,pr_picks   ,p0  , n     ,  p5   ,  0,  5,   0)
      call nbox_crega (6 ,pr_project ,p0  , project, p1   ,  0,  0,   0)
      call nbox_crega (7 ,pr_line    ,p0  , line  ,  p1   ,  0,  0,   0)
      call nbox_crega (8 ,pr_rdate   ,p0  , rdate ,  p1   ,  0,  0,   0)
      call nbox_crega (9 ,pr_pdate   ,p0  , pdate ,  p1   ,  0,  0,   0)
      call nbox_crega (10,pr_id      ,p0  , userid,  p1   ,  0,  0,   0)
      call nbox_crega (11,pr_comment ,p0  , comment, p1   ,  0,  0,   0)
      call fbox_set_focus (ifunbox,14,ifun)
      return


      entry fun_trap (box,ident,index,   text,nread,endkey)

      if (endkey.eq.'REDRAW') then
           call convert_ii2cc (changeflag,chng,1)
           call get_sort_choice (xchoice,ychoice)

      else if (endkey.eq.'ARRIVED') then
           if (ident.eq.-12) then
                call fbox_maybe ('data change flag - see help')
           else if (ident.eq.4.and.index.le.nfun) then
                call show_velfun_type (type(index))
           end if

      else if (ident.eq.2.and.nread.gt.0.and.index.le.nfun) then
           call check_xbin (index,endkey,doverify)

      else if (ident.eq.3.and.nread.gt.0.and.index.le.nfun) then
           call check_ybin (index,endkey,doverify)

      else if (ident.eq.-12.and.endkey.eq.'RETURN') then
           call f_autosave_workfile_cursor (0)

      else if (ident.eq.20.and.endkey.eq.'RETURN') then
           if (index.le.nfun) call adjust_select_codes (index)

      else if (ident.eq.4.and.endkey.eq.'RETURN') then
           if (index.le.nfun) call increment_velfun_type (index)
           if (index.eq.ifun) call pick_undo_sensitize (vel,1)

      else if (ident.eq.14.and.endkey.eq.'RETURN') then
           call f_autosave_workfile_cursor (0)
           call f_activate_velfun (index)
           call fbox_set_focus (ipickbox,21,1)
           if (index.gt.nfun) nread=0
           call fbroadcast (vel,SENDER_WBOX,MESSAGE_ACTIVE,0,0,0,0,0)

      else if (ident.ge.331.and.ident.le.336.and.endkey.eq.'RETURN')then
           call set_sort_choice (ident)

      else if (ident.eq.63.and.nread.gt.0) then
           ifun=max(1,min(ifun,nfun+1,nfunmax))
           call f_autosave_workfile_cursor (0)
           call f_activate_velfun (ifun)
           call fbox_set_focus (ipickbox,21,1)
           ident=1
           index=ifun
           nread=0
           call fbroadcast (vel,SENDER_WBOX,MESSAGE_ACTIVE,0,0,0,0,0)

      else if (ident.eq.61.and.nread.gt.0) then
           call check_nhx (endkey)

      else if (ident.eq.62.and.nread.gt.0) then
           call check_nhy (endkey)

      else if (ident.lt.60.and.nread.gt.0.and.index.gt.nfun) then
           call alloc_last_velfun (msg,ierr)
           call fbox_message (msg)
           if (ierr.eq.0) then
               errmsg(index)='N'
               if (ident.ne.2) xbin(index)=0.
               if (ident.ne.3) ybin(index)=0.
               if (ident.ne.3.and.index.gt.1) ybin(index)=ybin(index-1)
               if(doverify.ge.1) call verify_ybin (index,   ierr)
               if(doverify.ge.1) call verify_xbin (index,   ierr)
               call fbroadcast (vel,SENDER_WBOX,MESSAGE_INSERT,nfun,
     $                                             0,0,0,0)
           else
                endkey='RESTORE'  ! probably will not work right.
           end if

      else if (nread.gt.0) then
           continue

      else if (ident.lt.30.and.endkey.eq.'^C') then
           call clear_velfun (nfunmax+1)
           call fbox_message ('buffer cleared')
           endkey=' '

      else if (ident.lt.30.and.(endkey.eq.'INSERT'.or.endkey.eq.'^P'))
     $                                                            then
           call f_insert_velfun (index,   ierr)
           if (ierr.eq.0) call fbroadcast
     $               (vel, SENDER_WBOX, MESSAGE_INSERT,index,0,0,0,0)
           endkey=' '

      else if (ident.lt.30.and.endkey.eq.'REMOVE') then
           call f_remove_velfun (index,   ierr)
           if (ierr.eq.0) call fbroadcast
     $               (vel, SENDER_WBOX, MESSAGE_REMOVE,index,0,0,0,0)
           endkey=' '
      end if
      if (nread.gt.0.and.ident.ne.14.and.ident.ne.20) changeflag=1
      if (nread.gt.0.and.ident.ne.14.and.ident.ne.20.and.
     $    ident.ne.-12.and.ident.ne.61.and.ident.ne.62.and.
     $    index.eq.ifun) call pick_undo_sensitize (vel,1)
      return


      entry fun_apply (dummy,error)
      error=0
      return

      entry fun_deselect (dummy)
      call delete_select_codes
      call fbox_messageline (ifunbox,'select codes deleted')
      return
      end

  
          
      subroutine get_sort_choice (xchoice,ychoice)

      implicit none
      integer xchoice(3),ychoice(3)
      include 'vel_boxes.inc'

      xchoice(1)=0
      xchoice(2)=0
      xchoice(3)=0
      if (xdirwant.gt.0) xchoice(1)=1
      if (xdirwant.lt.0) xchoice(2)=1
      if (xdirwant.eq.0) xchoice(3)=1
      ychoice(1)=0
      ychoice(2)=0
      ychoice(3)=0
      if (ydirwant.gt.0) ychoice(1)=1
      if (ydirwant.lt.0) ychoice(2)=1
      if (ydirwant.eq.0) ychoice(3)=1
      return
      end



      subroutine set_sort_choice (ident)

      implicit none
      integer ident
      include 'vel_boxes.inc'

      if (ident.eq.331) xdirwant= 1
      if (ident.eq.332) xdirwant=-1
      if (ident.eq.333) xdirwant= 0
      if (ident.eq.334) ydirwant= 1
      if (ident.eq.335) ydirwant=-1
      if (ident.eq.336) ydirwant= 0
      return
      end



c-------------------------------------------------------------------------------


      subroutine res_register (box)

      implicit none
      integer box,ident,index,nread,dummy,error     ! arguments
      character*(*) text,endkey                     ! arguments
      include 'vel_boxes.inc'
      integer C2
      parameter (C2=47)                       ! second column number.

      integer      iwhich,    which(2)
      integer      ichoice,   choice(3)
      integer      iterpflag, terpflag(7)
      integer      istepflag, stepflag(3)
      integer      iendflag,  endflag(4)
      integer      itypeflag, typeflag(numtypes)
      integer      nwant,ncoef,nrun   
      real         xdelta,xdelta2,tdelta,tdelta2,vdelta
      integer      b_timedepth   ,b_velocity
      real         f_time,f_depth,f_velocity
      integer      ft,fx,fa,fc,fv,fr,so,sp,se,sn,sm,sy
      integer      pt,px,pa,pc,pv,pr,po,pp,pe,pn,pm,py
      integer      sb1,sb2,sb3,fb1,fb2,fb3,pb1,pb2,pb3

      save         iwhich,    which 
      save         ichoice,   choice
      save         iterpflag, terpflag
      save         istepflag, stepflag
      save         iendflag,  endflag
      save         itypeflag, typeflag
      save         nwant,ncoef,nrun
      save         xdelta,xdelta2,tdelta,tdelta2,vdelta
      save         b_timedepth   ,b_velocity
      save         f_time,f_depth,f_velocity
      save         ft,fx,fa,fc,fv,fr,so,sp,se,sn,sm,sy
      save         pt,px,pa,pc,pv,pr,po,pp,pe,pn,pm,py
      save         sb1,sb2,sb3,fb1,fb2,fb3,pb1,pb2,pb3
 
ccc---------beginning of stuff needed for IBM.
      character*37 pr_selected,pr_all,pr_active
      save         pr_selected,pr_all,pr_active
      character*43 pr_type,pr_method
      save         pr_type,pr_method
      character*23 pr_versus,pr_time
      save         pr_versus,pr_time
      character*18 pr_nmo,pr_rms,pr_av,pr_int,pr_depth
      save         pr_nmo,pr_rms,pr_av,pr_int,pr_depth
      character*38 pr_incr
      save         pr_incr
      character*29 pr_number
      save         pr_number
      character*24 pr_retain
      save         pr_retain
      character*22 pr_itime
      save         pr_itime
      character*7  pr_seconds
      save         pr_seconds
      character*23 pr_idepth
      save         pr_idepth
      character*11 pr_feet
      save         pr_feet
      character*23 pr_npicks
      save         pr_npicks
      character*42 pr_interp,pr_resetbot
      save         pr_interp,pr_resetbot
      character*41 pr_linear,pr_cubic,pr_spline,pr_poly,pr_run
      save         pr_linear,pr_cubic,pr_spline,pr_poly,pr_run
      character*41 pr_without,pr_reset
      save         pr_without,pr_reset
      character*39 pr_average
      save         pr_average
      character*40 pr_ncoef
      save         pr_ncoef
      character*35 pr_nrun
      save         pr_nrun
      character*21 pr_tbottom,pr_dbottom,pr_vbottom
      save         pr_tbottom,pr_dbottom,pr_vbottom
      character*2  pr_to
      save         pr_to
      character*4  pr_secs
      save         pr_secs
      integer p0,p1,p2,p3,p4,m1,m2,m3,m4,m5,m44,m99 
      save    p0,p1,p2,p3,p4,m1,m2,m3,m4,m5,m44,m99
      data    p0,p1,p2,p3,p4,m1,m2,m3,m4,m5,m44,m99/
     $         0, 1, 2, 3, 4,-1,-2,-3,-4,-5,-44,-99/
      data pr_selected/'Resample selected velocity functions:'/
      data pr_all     /'Resample all velocity functions:     '/
      data pr_active  /'Resample active velocity function #: '/
      data pr_type    /'-------TYPE OF FUNCTION TO RESAMPLE--------'/
      data pr_method  /'-------------RESAMPLING METHOD-------------'/
      data pr_versus  /'    versus       versus'/
      data pr_time    /'  2-way time      depth'/
      data pr_nmo     /'     NMO velocity:'/
      data pr_rms     /'     RMS velocity:'/
      data pr_av      /' Average velocity:'/
      data pr_int     /'Interval velocity:'/
      data pr_depth   /'            Depth:'/
      data pr_incr    /'Use specified time or depth increments'/
      data pr_number  /'Use specified number of picks'/
      data pr_retain  /'Retain the current picks'/
      data pr_itime   /'Time incr (top/bottom)'/
      data pr_seconds /'seconds'/
      data pr_idepth  /'Depth incr (top/bottom)'/
      data pr_feet    /'feet/meters'/
      data pr_npicks  /'Desired number of picks'/
      data pr_interp  /'------INTERPOLATION/SMOOTHING METHOD------'/
      data pr_resetbot/'-----------RESET BOTTOM PICK--------------'/
      data pr_linear  /'Resample with linear interpolation       '/
      data pr_cubic   /'Resample with 4-point cubic interpolation'/
      data pr_spline  /'Resample with spline interpolation       '/
      data pr_poly    /'Resample with polynomial fit             '/
      data pr_run     /'Use linear interp plus running average   '/
      data pr_without /'Resample without changing earth model    '/
      data pr_reset   /'Reset bottom pick                        '/
      data pr_average /'Average spline velocity error to accept'/
      data pr_ncoef   /'Number of coefficients in polynomial fit'/
      data pr_nrun    /'Number of points in running average'/
      data pr_tbottom /'Reset bottom time    '/
      data pr_dbottom /'Reset bottom depth   '/
      data pr_vbottom /'Reset bottom velocity'/
      data pr_to      /'to'/
      data pr_secs    /'secs'/
ccc---------end of stuff needed for IBM.

      data         nwant,ncoef,nrun                    /10,2,3        /
      data         xdelta,xdelta2                      /2000.,2000.   /
      data         tdelta,tdelta2,vdelta               /0.5,0.5,0.    /
      data         ft,fx,fa,fc,fv,fr,so,sp,se,sn,sm,sy /6*1,6*4       /
      data         pt,px,pa,pc,pv,pr,po,pp,pe,pn,pm,py /12*0          /
      data         sb1,sb2,sb3,fb1,fb2,fb3,pb1,pb2,pb3 /3*3,3*1,3*0   /
      data         b_timedepth   ,b_velocity           /1,0           /
      data         f_time,f_depth,f_velocity      /5.0,99999.0,20000.0/
 
      iresbox=box
      call get_choice (81,  81,82,iwhich   ,which   )
      call get_choice (52,  51,53,ichoice  ,choice  )
      call get_choice (41,  41,43,istepflag,stepflag)
      call get_choice (31,  31,37,iterpflag,terpflag)
      call get_choice (74,  71,74,iendflag ,endflag )
      call get_choice ( 1,   1,numtypes,itypeflag,typeflag)

cccc        ID      PROMPT ISW      VAR ISW      ROW COL NCHAR NDEC

      call nbox_ireg3 ( 51,pr_selected, p0, choice(1),p4,    1,20,2,0)
      call nbox_ireg3 ( 52,pr_all     , p0, choice(2),p4,    0,20,2,0)
      call nbox_ireg3 ( 53,pr_active  , p0, choice(3),p4,    0,20,2,0)
      call nbox_ireg  (  0,nselect    , m5,                  1,63,5,0)
      call nbox_ireg  (  0,nfun       , m5,                  0,63,5,0)
      call nbox_ireg  (  0,ifun       , m5,                  0,63,5,0)
cccccccccccccc first column:
      call nbox_mreg  (    pr_type                         ,5,1)
      call nbox_mreg  (    pr_versus                       ,0,20)
      call nbox_mreg  (    pr_time                         ,0,20)
      call nbox_mreg  (    pr_nmo                          , 8,1)
      call nbox_mreg  (    pr_rms                          , 0,1)
      call nbox_mreg  (    pr_av                           , 0,1)
      call nbox_mreg  (    pr_int                          , 0,1)
      call nbox_mreg  (    pr_depth                        , 0,1)
      call nbox_ireg3 (  1,veltype(1) ,py ,typeflag(1) ,sy,  8,23,2,0)
      call nbox_ireg3 (  2,veltype(2) ,py ,typeflag(2) ,sy,  9,23,2,0)
      call nbox_ireg3 (  3,veltype(3) ,py ,typeflag(3) ,sy,  9,36,2,0)
      call nbox_ireg3 (  5,veltype(5) ,py ,typeflag(5) ,sy, 10,23,2,0)
      call nbox_ireg3 (  6,veltype(6) ,py ,typeflag(6) ,sy, 10,36,2,0)
      call nbox_ireg3 (  8,veltype(8) ,p0 ,typeflag(8) ,p4, 11,23,2,0)
      call nbox_ireg3 (  9,veltype(9) ,p0 ,typeflag(9) ,p4, 11,36,2,0)
      call nbox_ireg3 ( 11,veltype(11),py ,typeflag(11),sy, 12,23,2,0)
      call nbox_mreg  (    pr_method                       ,0,1)
      call nbox_ireg3 ( 41,pr_incr    ,po ,stepflag(1),so,  0,1,2,0)
      call nbox_ireg3 ( 42,pr_number  ,po ,stepflag(2),so,  0,1,2,0)
      call nbox_ireg3 ( 43,pr_retain  ,pp ,stepflag(3),sp,  0,1,2,0)
      call nbox_freg2 ( 21,pr_itime   ,pt ,tdelta     ,ft,  0,1,5,3)
      call nbox_freg  ( 27,                tdelta2    ,ft,  -1,-1,5,3)
      call nbox_creg  (  0,pr_seconds ,pt ,                 -1,-1,0,0)
      call nbox_freg2 ( 22,pr_idepth  ,px ,xdelta     ,fx,    0,1,4,0)
      call nbox_freg  ( 28,                xdelta2    ,fx,  -1,-1,4,0)
      call nbox_creg  (  0,pr_feet    ,px ,                 -1,-1,0,0)
      call nbox_ireg2 ( 23,pr_npicks  ,pa ,nwant      ,fa,    0,1,3,0)
cccccccccccccc second column:
      call nbox_mreg  (    pr_interp                       ,5,C2)
      call nbox_ireg3 ( 31,pr_linear  ,pn ,terpflag(1),sn,  0,C2,2,0)
      call nbox_ireg3 ( 32,pr_cubic   ,pn ,terpflag(2),sn,  0,C2,2,0)
      call nbox_ireg3 ( 33,pr_spline  ,p0 ,terpflag(3),p4,  0,C2,2,0)
      call nbox_ireg3 ( 34,pr_poly    ,p0 ,terpflag(4),p4,  0,C2,2,0)
      call nbox_ireg3 ( 35,pr_run     ,p0 ,terpflag(5),p4,  0,C2,2,0)
      call nbox_ireg3 ( 36,pr_without ,pm ,terpflag(6),sm,  0,C2,2,0)
      call nbox_ireg3 ( 37,pr_reset   ,p0 ,terpflag(7),p4,  0,C2,2,0)
      call nbox_freg2 ( 61,pr_average ,pv ,vdelta     ,fv,  0,C2,4,0)
      call nbox_ireg2 ( 62,pr_ncoef   ,pc ,ncoef      ,fc,  0,0,2,0)
      call nbox_ireg2 ( 63,pr_nrun    ,pr ,nrun       ,fr,  0,0,2,0)
      call nbox_mreg  (    pr_resetbot,                      0,C2)
      call nbox_ireg3 (991,pr_tbottom ,pb1,b_timedepth,sb1,  0,C2,2,0)
      call nbox_freg2 ( 91,pr_to      ,pb1,f_time     ,fb1, -1,-1,6,3)
      call nbox_creg  (  0,pr_secs    ,pb1,                 -1,-1,0,0)
      call nbox_ireg3 (992,pr_dbottom ,pb2,b_timedepth,sb2,  0,C2,2,0)
      call nbox_freg2 ( 92,pr_to      ,pb2,f_depth    ,fb2, -1,-1,6,3)
      call nbox_ireg3 (993,pr_vbottom ,pb3,b_velocity ,sb3,  0,C2,2,0)
      call nbox_freg2 ( 93,pr_to      ,pb3,f_velocity ,fb3, -1,-1,6,3)
      return


      entry res_trap (box,ident,index,   text,nread,endkey)

      if (endkey.eq.'REDRAW') then
           call set_res_switches(veltype(itypeflag),istepflag,iterpflag,
     $                      ft,fx,fa,fc,fv,fr,so,sp,se,sn,sm,sy,
     $                      pt,px,pa,pc,pv,pr,po,pp,pe,pn,pm,py,
     $                      sb1,sb2,sb3,fb1,fb2,fb3,pb1,pb2,pb3)
      else if (endkey.eq.'ARRIVED') then
           if (ident.ge.1.and.ident.le.numtypes) then
                call show_velfun_type (veltype(ident))
           end if
      else if (nread.gt.0.and.(ident.eq.991.or.ident.eq.992)) then
           if (b_timedepth.le.0.and.b_velocity.le.0) b_velocity=1
      else if (nread.gt.0.and.ident.eq.993) then
           if (b_timedepth.le.0.and.b_velocity.le.0) b_timedepth=1
      else if ((nread.gt.0.or.endkey.eq.'RETURN').and.ident.eq.91) then
           f_time=max(f_time,2.0*timetol)
c          if (bottomtime.ne.fnil.and.bottomtime.gt.0.0)
c    $                                   f_time=min(f_time,bottomtime)
           b_timedepth=1
      else if ((nread.gt.0.or.endkey.eq.'RETURN').and.ident.eq.92) then
           f_depth=max(f_depth,2.0*depthtol)
           b_timedepth=1
      else if ((nread.gt.0.or.endkey.eq.'RETURN').and.ident.eq.93) then
           f_velocity=max(f_velocity,1.0)
           b_velocity=1
      else if (nread.gt.0.and.ident.eq.21) then
           tdelta=max(0.001,tdelta)
      else if (nread.gt.0.and.ident.eq.22) then
           xdelta=max(1.,xdelta)
      else if (nread.gt.0.and.ident.eq.27) then
           tdelta2=max(0.001,tdelta2)
      else if (nread.gt.0.and.ident.eq.28) then
           xdelta2=max(1.,xdelta2)
      else if (nread.gt.0.and.ident.eq.23) then
           nwant=max(2,min(nwant,nmax))
      else if (nread.gt.0.and.ident.eq.61) then
           vdelta=max(0.,vdelta)
      else if (nread.gt.0.and.ident.eq.62) then
           ncoef=max(0,min(ncoef,10))
      else if (nread.gt.0.and.ident.eq.63) then
           nrun=max(1,min(nrun,99))
      else if (endkey.eq.'RETURN') then
           call get_choice (ident,81,82,iwhich   ,which   )
           call get_choice (ident,51,53,ichoice  ,choice  )
           call get_choice (ident,41,43,istepflag,stepflag)
           call get_choice (ident,31,37,iterpflag,terpflag)
           call get_choice (ident,71,74,iendflag ,endflag )
           call get_choice (ident, 1,numtypes,itypeflag,typeflag)
      end if
      return


      entry res_apply (dummy,error)
      call f_resample_velfuns (ichoice,0,veltype(itypeflag),
     $        iwhich,iterpflag,istepflag,iendflag,
     $        b_timedepth   ,b_velocity,
     $        f_time,f_depth,f_velocity,
     $        nwant,ncoef,nrun,xdelta,xdelta2,tdelta,tdelta2,vdelta,
     $        error)
      return
      end






      subroutine set_res_switches (type2,istepflag,iterpflag,
     $                      ft,fx,fa,fc,fv,fr,so,sp,se,sn,sm,sy,
     $                      pt,px,pa,pc,pv,pr,po,pp,pe,pn,pm,py,
     $                      sb1,sb2,sb3,fb1,fb2,fb3,pb1,pb2,pb3)
c     set switches for vertical resampling box.
C     first letter = s = toggle or radio = 3, -3, 4, -4.
C     first letter = p = label or prompt = 0, -55.
C     first letter = f = text            = 1, -22.

      implicit none
      character*(*) type2
      integer istepflag,iterpflag
      integer ft,fx,fa,fc,fv,fr,so,sp,se,sn,sm,sy
      integer pt,px,pa,pc,pv,pr,po,pp,pe,pn,pm,py
      integer sb1,sb2,sb3,fb1,fb2,fb3,pb1,pb2,pb3

      ft=1                ! time increment.
      fx=1                ! depth increment.
      fa=1                ! number of picks.
      fc=1                ! number of coefficients.
      fv=1                ! average spline velocity error.
      fr=1                ! number of points in running average.
      so=4                ! Use specified time/depth/number.
      sp=4                ! Retain current picks.
      se=4                ! running average end option.
      sn=4                ! linear or cubic resampling.
      sm=4                ! Resample without changing earth model.
      sy=4                ! non-interval velocities.
      sb1=3               ! reset bottom time.
      sb2=3               ! reset bottom depth.
      sb3=3               ! reset bottom velocity.
      if (iterpflag.eq.7.or.istepflag.ne.1.or.type2(1:2).ne.'VT') ft=-22
      if (iterpflag.eq.7.or.istepflag.ne.1.or.type2(1:2).ne.'VZ') fx=-22
      if (iterpflag.eq.7.or.istepflag.ne.2                      ) fa=-22
      if (iterpflag.ne.4                      ) fc=-22
      if (iterpflag.ne.3                      ) fv=-22
      if (iterpflag.ne.5                      ) fr=-22
      if (iterpflag.eq.7                      ) so=-4
      if (iterpflag.ge.6.or.iterpflag.le.2    ) sp=-4
      if (iterpflag.ne.5                      ) se=-4
      if (istepflag.eq.3                      ) sn=-4
      if (istepflag.eq.3.or.type2(3:4).ne.'IN') sm=-4
      if (iterpflag.eq.6                      ) sy=-4
      if (iterpflag.ne.7.or.type2(1:2).ne.'VT') sb1=-3
      if (iterpflag.ne.7.or.
     $(type2(1:2).ne.'VZ'.and.type2.ne.'VTDP')) sb2=-3
      if (iterpflag.ne.7.or.type2.eq.'VTDP'   ) sb3=-3
      pt=0 
      px=0
      pa=0
      pc=0
      pv=0
      pr=0
      po=0
      pp=0
      pe=0
      pn=0
      pm=0
      py=0
      pb1=0
      pb2=0
      pb3=0
      fb1=1
      fb2=1
      fb3=1
      if (ft.lt.0) pt=-55
      if (fx.lt.0) px=-55
      if (fa.lt.0) pa=-55
      if (fc.lt.0) pc=-55
      if (fv.lt.0) pv=-55
      if (fr.lt.0) pr=-55
      if (so.lt.0) po=-55
      if (sp.lt.0) pp=-55
      if (se.lt.0) pe=-55
      if (sn.lt.0) pn=-55
      if (sm.lt.0) pm=-55
      if (sy.lt.0) py=-55
      if (sb1.lt.0) pb1=-55
      if (sb2.lt.0) pb2=-55
      if (sb3.lt.0) pb3=-55
      if (sb1.lt.0) fb1=-22
      if (sb2.lt.0) fb2=-22
      if (sb3.lt.0) fb3=-22
      return
      end




c-------------------------------------------------------------------------------


      subroutine get_choice (ident,ifirst,ilast,ichoice,choice)
c     sets ichoice and choice(n) according to the pushed radio button.
c     returns immediately if ident is not within range.
c     ident  = the ident that was pushed.
c     ifirst = first ident in this radio button group.
c     ilast  = last ident in this radio button group.
c     ichoice = integer specifying the choice made (returned).
c     choice(*) = integer array registered with idents [ifirst to ilast].

      implicit none
      integer ident,ifirst,ilast,ichoice,choice(*),i

      if (ident.lt.ifirst.or.ident.gt.ilast) return
      ichoice=ident-ifirst+1
      do i=ifirst,ilast
           choice(i-ifirst+1)=0
      end do
      choice(ichoice)=1
      return
      end



c-------------------------------------------------------------------------------



      subroutine lat_register (box)

      implicit none
      integer box,ident,index,nread,dummy,error     ! arguments
      character*(*) text,endkey                     ! arguments
      include 'vel_boxes.inc'
      integer C2
      parameter (C2=47)                       ! second column number.

      integer      nonselect
      integer      iwhich,    which(2)
      integer      ichoice,   choice(3)
      integer      iterpflag, terpflag(5)
      integer      istepflag, stepflag(3)
      integer      iendflag,  endflag(4)
      integer      itypeflag, typeflag(numtypes)
      integer      nwant,ncoef,nrun
      real         xdelta,vdelta,ybinchoice
      integer      sx,sa,sc,sv,sr,sp,se,sn
      integer      px,pa,pc,pv,pr,pp,pe,pn

      save         nonselect
      save         iwhich,    which
      save         ichoice,   choice
      save         iterpflag, terpflag
      save         istepflag, stepflag
      save         iendflag,  endflag
      save         itypeflag, typeflag
      save         nwant,ncoef,nrun
      save         xdelta,vdelta,ybinchoice
      save         sx,sa,sc,sv,sr,sp,se,sn
      save         px,pa,pc,pv,pr,pp,pe,pn
 
ccc---------beginning of stuff needed for IBM.
      character*28 pr_delete,pr_add
      save         pr_delete,pr_add
      character*11 pr_choice
      save         pr_choice
      character*36 pr_selected,pr_all,pr_non
      save         pr_selected,pr_all,pr_non
      character*43 pr_type,pr_method
      save         pr_type,pr_method
      character*23 pr_versus,pr_time
      save         pr_versus,pr_time
      character*18 pr_nmo,pr_rms,pr_av,pr_int,pr_depth
      save         pr_nmo,pr_rms,pr_av,pr_int,pr_depth
      character*40 pr_incr,pr_number,pr_retain
      save         pr_incr,pr_number,pr_retain
      character*21 pr_dincr
      save         pr_dincr
      character*34 pr_dnumber
      save         pr_dnumber
      character*42 pr_interp
      save         pr_interp
      character*31 pr_linear,pr_cubic,pr_spline,pr_poly,pr_run
      save         pr_linear,pr_cubic,pr_spline,pr_poly,pr_run
      character*39 pr_average
      save         pr_average
      character*40 pr_ncoef
      save         pr_ncoef
      character*38 pr_nrun
      save         pr_nrun
      character*40 pr_truncated,pr_shifted,pr_extended,pr_narrowed
      save         pr_truncated,pr_shifted,pr_extended,pr_narrowed
      integer p0,p1,p2,p3,p4,m1,m2,m3,m4,m5,m44,m99
      save    p0,p1,p2,p3,p4,m1,m2,m3,m4,m5,m44,m99
      data    p0,p1,p2,p3,p4,m1,m2,m3,m4,m5,m44,m99/
     $         0, 1, 2, 3, 4,-1,-2,-3,-4,-5,-44,-99/
      data pr_delete   /'Delete all current functions'/
      data pr_add      /'Add new functions at end    '/
      data pr_choice   /'YBIN choice'/
      data pr_selected /'Use selected functions as input:    '/
      data pr_all      /'Use all current functions as input: '/
      data pr_non      /'Use non-selected functions as input:'/
      data pr_type     /'--TYPE OF FUNCTION TO RESAMPLE LATERALLY---'/
      data pr_method   /'--------LATERAL RESAMPLING METHOD----------'/
      data pr_versus   /'    versus       versus'/
      data pr_time     /'  2-way time      depth'/
      data pr_nmo      /'     NMO velocity:'/
      data pr_rms      /'     RMS velocity:'/
      data pr_av       /' Average velocity:'/
      data pr_int      /'Interval velocity:'/
      data pr_depth    /'            Depth:'/
      data pr_incr     /'Use specified CMP increment (X bin)     '/
      data pr_number   /'Use specified number of output functions'/
      data pr_retain   /'Retain the current function locations   '/
      data pr_dincr    /'Desired CMP increment'/
      data pr_dnumber  /'Desired number of output functions'/
      data pr_interp   /'------INTERPOLATION/SMOOTHING METHOD------'/
      data pr_linear   /'Use linear interpolation       '/
      data pr_cubic    /'Use 4-point cubic interpolation'/
      data pr_spline   /'Use spline interpolation       '/
      data pr_poly     /'Use polynomial fit             '/
      data pr_run      /'Use running average            '/
      data pr_average  /'Average spline velocity error to accept'/
      data pr_ncoef    /'Number of coefficients in polynomial fit'/
      data pr_nrun     /'Number of functions in running average'/
      data pr_truncated/'Use truncated running average end option'/
      data pr_shifted  /'Use shifted running average end option  '/
      data pr_extended /'Use extended running average end option '/
      data pr_narrowed /'Use narrowed end option (graded ends)   '/
ccc---------end of stuff needed for IBM.

      data         nwant,ncoef,nrun         /10,2,3     /
      data         xdelta,vdelta            /100.,0.    /
      data         sx,sa,sc,sv,sr,sp,se,sn  /5*1,3*4    /
      data         px,pa,pc,pv,pr,pp,pe,pn  / 8*0       /
 
      ilatbox=box
      call get_choice (81,  81,82,iwhich   ,which   )
      call get_choice (52,  51,53,ichoice  ,choice  )
      call get_choice (41,  41,43,istepflag,stepflag)
      call get_choice (31,  31,35,iterpflag,terpflag)
      call get_choice (74,  71,74,iendflag ,endflag )
      call get_choice ( 1,   1,numtypes,itypeflag,typeflag)
      ybinchoice=fnil

cccc        ID      PROMPT ISW      VAR ISW      ROW COL NCHAR NDEC

      call nbox_ireg3 (81,pr_delete  , p0, which(1)  ,p4,  1, 1,2,0)
      call nbox_ireg3 (82,pr_add     , p0, which(2)  ,p4,  0, 1,2,0)
      call nbox_freg2 (83,pr_choice  , p0, ybinchoice,p1,  0, 1,8,2)
      call nbox_ireg3 (51,pr_selected, p0, choice(1) ,p4,  1,44,2,0)
      call nbox_ireg3 (52,pr_all     , p0, choice(2) ,p4,  0,44,2,0)
      call nbox_ireg3 (53,pr_non     , p0, choice(3) ,p4,  0,44,2,0)
      call nbox_ireg (0,nselect  , m5,1,86,5,0)
      call nbox_ireg (0,nfun     , m5,0,86,5,0)
      call nbox_ireg (0,nonselect, m5,0,86,5,0)
cccccccccccccc first column:
      call nbox_mreg (pr_type    ,5,1)
      call nbox_mreg (pr_versus  ,0,20)
      call nbox_mreg (pr_time    ,0,20)
      call nbox_mreg (pr_nmo     , 8,1)
      call nbox_mreg (pr_rms     , 0,1)
      call nbox_mreg (pr_av      , 0,1)
      call nbox_mreg (pr_int     , 0,1)
      call nbox_mreg (pr_depth   , 0,1)
      call nbox_ireg3 ( 1,veltype(1),p0, typeflag(1), p4,  8,23,2,0)
      call nbox_ireg3 ( 2,veltype(2),p0, typeflag(2), p4,  9,23,2,0)
      call nbox_ireg3 ( 3,veltype(3),p0, typeflag(3), p4,  9,36,2,0)
      call nbox_ireg3 ( 5,veltype(5),p0, typeflag(5), p4, 10,23,2,0)
      call nbox_ireg3 ( 6,veltype(6),p0, typeflag(6), p4, 10,36,2,0)
      call nbox_ireg3 ( 8,veltype(8),p0, typeflag(8), p4, 11,23,2,0)
      call nbox_ireg3 ( 9,veltype(9),p0, typeflag(9), p4, 11,36,2,0)
      call nbox_ireg3 (11,veltype(11),p0,typeflag(11),p4, 12,23,2,0)
      call nbox_mreg  (pr_method,0,1)
      call nbox_ireg3 (41,pr_incr    , p0, stepflag(1),p4,  0,1,2,0)
      call nbox_ireg3 (42,pr_number  , p0, stepflag(2),p4,  0,1,2,0)
      call nbox_ireg3 (43,pr_retain  , pp, stepflag(3),sp,  0,1,2,0)
      call nbox_freg2 (22,pr_dincr   , px, xdelta     ,sx,  0,1,8,2)
      call nbox_ireg2 (23,pr_dnumber , pa, nwant      ,sa,  0,1,4,0)
cccccccccccccc second column:
      call nbox_mreg  (pr_interp,5,C2)
      call nbox_ireg3 (31,pr_linear   , pn, terpflag(1),sn,  0,C2,2,0)
      call nbox_ireg3 (32,pr_cubic    , pn, terpflag(2),sn,  0,C2,2,0)
      call nbox_ireg3 (33,pr_spline   , p0, terpflag(3),p4,  0,C2,2,0)
      call nbox_ireg3 (34,pr_poly     , p0, terpflag(4),p4,  0,C2,2,0)
      call nbox_ireg3 (35,pr_run      , p0, terpflag(5),p4,  0,C2,2,0)
      call nbox_freg2 (61,pr_average  , pv, vdelta     ,sv,  0,C2,4,0)
      call nbox_ireg2 (62,pr_ncoef    , pc, ncoef      ,sc,  0,0,2,0)
      call nbox_ireg2 (63,pr_nrun     , pr, nrun       ,sr,  0,0,2,0)
      call nbox_ireg3 (71,pr_truncated, pe, endflag(1) ,se,  0,C2,2,0)
      call nbox_ireg3 (72,pr_shifted  , pe, endflag(2) ,se,  0,C2,2,0)
      call nbox_ireg3 (73,pr_extended , pe, endflag(3) ,se,  0,C2,2,0)
      call nbox_ireg3 (74,pr_narrowed , pe, endflag(4) ,se,  0,C2,2,0)
      return


      entry lat_trap (box,ident,index,   text,nread,endkey)

      if (endkey.eq.'REDRAW') then
           nonselect=nfun-nselect
           call set_lat_switches(istepflag,iterpflag,
     $                               sx,sa,sc,sv,sr,sp,se,sn,
     $                               px,pa,pc,pv,pr,pp,pe,pn)
      else if (endkey.eq.'ARRIVED') then
           if (ident.ge.1.and.ident.le.numtypes) then
                call show_velfun_type (veltype(ident))
           end if
      else if (nread.gt.0.and.ident.eq.22) then
           xdelta=max(0.01,xdelta)
      else if (nread.gt.0.and.ident.eq.23) then
           nwant=max(1,min(nwant,nfunmax))
      else if (nread.gt.0.and.ident.eq.61) then
           vdelta=max(0.,vdelta)
      else if (nread.gt.0.and.ident.eq.62) then
           ncoef=max(0,min(ncoef,10))
      else if (nread.gt.0.and.ident.eq.63) then
           nrun=max(1,min(nrun,99))
      else if (endkey.eq.'RETURN') then
           call get_choice (ident,81,82,iwhich   ,which   )
           call get_choice (ident,51,53,ichoice  ,choice  )
           call get_choice (ident,41,43,istepflag,stepflag)
           call get_choice (ident,31,35,iterpflag,terpflag)
           call get_choice (ident,71,74,iendflag ,endflag )
           call get_choice (ident, 1,numtypes,itypeflag,typeflag)
      end if
      return

      entry lat_apply (dummy,error)
      call f_latsample_velfuns (ichoice,0,veltype(itypeflag),
     $        iwhich,iterpflag,istepflag,iendflag,ybinchoice,
     $        nwant,ncoef,nrun,xdelta,vdelta,  error)
      return
      end






      subroutine set_lat_switches (istepflag,iterpflag,
     $                                sx,sa,sc,sv,sr,sp,se,sn,
     $                                px,pa,pc,pv,pr,pp,pe,pn)
c     set switches for lateral resampling box.

      implicit none
      integer istepflag,iterpflag
      integer sx,sa,sc,sv,sr,sp,se,sn
      integer px,pa,pc,pv,pr,pp,pe,pn

      sx=1                ! CMP increment.
      sa=1                ! number of picks.
      sc=1                ! number of coefficients.
      sv=1                ! average spline velocity error.
      sr=1                ! number of functions in running average.
      sp=4                ! Retain current functions.
      se=4                ! running average end option.
      sn=4                ! linear or cubic resampling.
      if (istepflag.ne.1       ) sx=-22
      if (istepflag.ne.2       ) sa=-22
      if (iterpflag.ne.4       ) sc=-22
      if (iterpflag.ne.3       ) sv=-22
      if (iterpflag.ne.5       ) sr=-22
      if (iterpflag.le.2       ) sp=-4
      if (iterpflag.ne.5       ) se=-4
      if (istepflag.eq.3       ) sn=-4
      px=0
      pa=0
      pc=0
      pv=0
      pr=0
      pp=0
      pe=0
      pn=0
      if (sx.lt.0) px=-55
      if (sa.lt.0) pa=-55
      if (sc.lt.0) pc=-55
      if (sv.lt.0) pv=-55
      if (sr.lt.0) pr=-55
      if (sp.lt.0) pp=-55
      if (se.lt.0) pe=-55
      if (sn.lt.0) pn=-55
      return
      end




c-------------------------------------------------------------------------------



      subroutine lat2_register (box)

      implicit none
      integer box,ident,index,nread,dummy,error     ! arguments
      character*(*) text,endkey                     ! arguments
      include 'vel_boxes.inc'
      integer C2,C3,control,idirflag2
      parameter (C2=47)                       ! second column number.
      parameter (C3=C2+21)

      integer      idirflag,  dirflag(4)
      integer      iterpflag, terpflag(5)
      integer      ichngflag, chngflag(2)
      integer      iendflag,  endflag(4)
      integer      itypeflag, typeflag(numtypes)
      integer      ncoef,nrun
      real         vdelta
      integer      sc,sv,sr,sp,se,sn
      integer      pc,pv,pr,pp,pe,pn
      real    r_xfirst,r_yfirst, r_xinc,r_yinc, r_xlast,r_ylast 
      integer r_nx    ,r_ny    , srx,sry
      integer      sbelow,stbelow,sdbelow
      real         tbelow,dbelow
      integer      below

      save         idirflag,  dirflag
      save         iterpflag, terpflag
      save         ichngflag, chngflag
      save         iendflag,  endflag
      save         itypeflag, typeflag
      save         ncoef,nrun
      save         vdelta
      save         sc,sv,sr,sp,se,sn
      save         pc,pv,pr,pp,pe,pn
      save    r_xfirst,r_yfirst, r_xinc,r_yinc, r_xlast,r_ylast
      save    r_nx    ,r_ny    , srx,sry
      save         sbelow,stbelow,sdbelow
      save         tbelow,dbelow
      save         below
 
ccc---------beginning of stuff needed for IBM.
      character*43 pr_type,pr_method
      save         pr_type,pr_method
      character*23 pr_versus,pr_time
      save         pr_versus,pr_time
      character*18 pr_nmo,pr_rms,pr_av,pr_int,pr_depth
      save         pr_nmo,pr_rms,pr_av,pr_int,pr_depth
      character*37 pr_change,pr_retain
      save         pr_change,pr_retain
      character*14 pr_xybin
      save         pr_xybin
      character*21 pr_first,pr_incr,pr_last,pr_number
      save         pr_first,pr_incr,pr_last,pr_number
      character*42 pr_direction,pr_interp
      save         pr_direction,pr_interp
      character*16 pr_xonly,pr_yonly
      save         pr_xonly,pr_yonly
      character*18 pr_xtheny,pr_ythenx
      save         pr_xtheny,pr_ythenx
      character*31 pr_linear,pr_cubic,pr_spline,pr_poly,pr_run
      save         pr_linear,pr_cubic,pr_spline,pr_poly,pr_run
      character*39 pr_average
      character*40 pr_ncoef
      character*38 pr_nfunc
      save         pr_average,pr_ncoef,pr_nfunc
      character*40 pr_truncated,pr_shifted,pr_extended,pr_narrowed
      save         pr_truncated,pr_shifted,pr_extended,pr_narrowed
      character*17 pr_below
      character*4  pr_tbelow
      character*5  pr_dbelow
      save         pr_below,pr_tbelow,pr_dbelow
      integer p0,p1,p2,p3,p4,m1,m2,m3,m4,m5,m44,m99 
      save    p0,p1,p2,p3,p4,m1,m2,m3,m4,m5,m44,m99
      data    p0,p1,p2,p3,p4,m1,m2,m3,m4,m5,m44,m99/
     $         0, 1, 2, 3, 4,-1,-2,-3,-4,-5,-44,-99/
      data pr_type     /'--TYPE OF FUNCTION TO RESAMPLE LATERALLY---'/
      data pr_method   /'-------------RESAMPLING METHOD-------------'/
      data pr_versus   /'    versus       versus'/
      data pr_time     /'  2-way time      depth'/
      data pr_nmo      /'     NMO velocity:'/
      data pr_rms      /'     RMS velocity:'/
      data pr_av       /' Average velocity:'/
      data pr_int      /'Interval velocity:'/
      data pr_depth    /'            Depth:'/
      data pr_change   /'Change the X or Y function locations '/
      data pr_retain   /'Retain the current function locations'/
      data pr_xybin    /'X bin    Y bin'/
      data pr_first    /'first location:      '/
      data pr_incr     /'increment:           '/
      data pr_last     /'last location:       '/
      data pr_number   /'number of locations: '/
      data pr_direction/'-----------RESAMPLING DIRECTION-----------'/
      data pr_interp   /'------INTERPOLATION/SMOOTHING METHOD------'/
      data pr_xonly    /'X direction only'/
      data pr_yonly    /'Y direction only'/
      data pr_xtheny   /'X then Y direction'/
      data pr_ythenx   /'Y then X direction'/
      data pr_linear   /'Use linear interpolation       '/
      data pr_cubic    /'Use 4-point cubic interpolation'/
      data pr_spline   /'Use spline interpolation       '/
      data pr_poly     /'Use polynomial fit             '/
      data pr_run      /'Use running average            '/
      data pr_average  /'Average spline velocity error to accept'/
      data pr_ncoef    /'Number of coefficients in polynomial fit'/
      data pr_nfunc    /'Number of functions in running average'/
      data pr_truncated/'Use truncated running average end option'/
      data pr_shifted  /'Use shifted running average end option  '/
      data pr_extended /'Use extended running average end option '/
      data pr_narrowed /'Use narrowed end option (graded ends)   '/
      data pr_below    /'smooth only below'/
      data pr_tbelow   /'time'/
      data pr_dbelow   /'depth'/
ccc---------end of stuff needed for IBM.

      data         ncoef,nrun         /2,3     /
      data         vdelta            /0.    /
      data         sc,sv,sr,sp,se,sn  /3*1,3*4    /
      data         pc,pv,pr,pp,pe,pn  / 6*0       /
      data    r_xfirst,r_yfirst, r_xinc,r_yinc, r_xlast,r_ylast
     $                                   /2*0.0, 2*100.0, 2*1000.0/
      data    r_nx    ,r_ny    , srx,sry               /2*11, 2*-1/
      data         tbelow,dbelow    /0.0,0.0/
      data         below            /0/
      data         sbelow,stbelow,sdbelow  /3,1,1/

      ilat2box=box
      call get_choice (81,  81,84,idirflag ,dirflag )
      call get_choice (41,  41,42,ichngflag,chngflag)
      call get_choice (31,  31,35,iterpflag,terpflag)
      call get_choice (74,  71,74,iendflag ,endflag )
      call get_choice ( 1,   1,numtypes,itypeflag,typeflag)
      call recalculate_lat2 (4,r_xfirst,r_xinc,r_xlast,r_nx,'RESTORED')
      call recalculate_lat2 (4,r_yfirst,r_yinc,r_ylast,r_ny,'RESTORED')

cccc        ID      PROMPT ISW      VAR ISW      ROW COL NCHAR NDEC

cccccccccccccc first column:
      call nbox_mreg (pr_type  , 1, 1)
      call nbox_mreg (pr_versus, 0,20)
      call nbox_mreg (pr_time  , 0,20)
      call nbox_mreg (pr_nmo   , 4, 1)
      call nbox_mreg (pr_rms   , 0, 1)
      call nbox_mreg (pr_av    , 0, 1)
      call nbox_mreg (pr_int   , 0, 1)
      call nbox_mreg (pr_depth , 0, 1)
      call nbox_ireg3 (1,veltype(1),p0, typeflag(1), p4,  4,23,2,0)
      call nbox_ireg3 (2,veltype(2),p0, typeflag(2), p4,  5,23,2,0)
      call nbox_ireg3 (3,veltype(3),p0, typeflag(3), p4,  5,36,2,0)
      call nbox_ireg3 (5,veltype(5),p0, typeflag(5), p4,  6,23,2,0)
      call nbox_ireg3 (6,veltype(6),p0, typeflag(6), p4,  6,36,2,0)
      call nbox_ireg3 (8,veltype(8),p0, typeflag(8), p4,  7,23,2,0)
      call nbox_ireg3 (9,veltype(9),p0, typeflag(9), p4,  7,36,2,0)
      call nbox_ireg3(11,veltype(11),p0,typeflag(11),p4,  8,23,2,0)
      call nbox_mreg(pr_method,10,1)
      call nbox_ireg3 (41,pr_change, p0, chngflag(1),p4,  0,1,2,0)
      call nbox_ireg3 (42,pr_retain, pp, chngflag(2),sp,  0,1,2,0)
      call nbox_mreg (pr_xybin,0,23)
      call nbox_mreg (pr_first,0,1)
         call nbox_freg (221,r_xfirst,srx,-1,-1,8,2)
         call nbox_freg (331,r_yfirst,sry,-1,-1,8,2)
      call nbox_mreg (pr_incr,0,1)
         call nbox_freg (222,r_xinc  ,srx,-1,-1,8,2)
         call nbox_freg (332,r_yinc  ,sry,-1,-1,8,2)
      call nbox_mreg (pr_last,0,1)
         call nbox_freg (223,r_xlast ,srx,-1,-1,8,2)
         call nbox_freg (333,r_ylast ,sry,-1,-1,8,2)
      call nbox_mreg (pr_number,0,1)
         call nbox_ireg (224,r_nx    ,srx,-1,-1,8,0)
         call nbox_ireg (334,r_ny    ,sry,-1,-1,8,0)
      call nbox_ireg3 (774,pr_below,p0, below,sbelow, 0,1,2,0)
         call nbox_freg2 (775,pr_tbelow,p0, tbelow,stbelow, -1,-1,6,3)
         call nbox_freg2 (776,pr_dbelow,p0, dbelow,sdbelow, -1,-1,6,0)
cccccccccccccc second column:
      call nbox_mreg  (   pr_direction,                     1,C2)
      call nbox_ireg3 (81,pr_xonly    , p0, dirflag(1) ,p4, 2,C2,2,0)
      call nbox_ireg3 (82,pr_yonly    , p0, dirflag(2) ,p4, 3,C2,2,0)
      call nbox_ireg3 (83,pr_xtheny   , p0, dirflag(3) ,p4, 2,C3,2,0)
      call nbox_ireg3 (84,pr_ythenx   , p0, dirflag(4) ,p4, 3,C3,2,0)
      call nbox_mreg  (   pr_interp   ,                     5,C2)
      call nbox_ireg3 (31,pr_linear   , pn, terpflag(1),sn, 0,C2,2,0)
      call nbox_ireg3 (32,pr_cubic    , pn, terpflag(2),sn, 0,C2,2,0)
      call nbox_ireg3 (33,pr_spline   , p0, terpflag(3),p4, 0,C2,2,0)
      call nbox_ireg3 (34,pr_poly     , p0, terpflag(4),p4, 0,C2,2,0)
      call nbox_ireg3 (35,pr_run      , p0, terpflag(5),p4, 0,C2,2,0)
      call nbox_freg2 (61,pr_average  , pv, vdelta     ,sv, 0,C2,4,0)
      call nbox_ireg2 (62,pr_ncoef    , pc, ncoef      ,sc, 0, 0,2,0)
      call nbox_ireg2 (63,pr_nfunc    , pr, nrun       ,sr, 0, 0,2,0)
      call nbox_ireg3 (71,pr_truncated, pe, endflag(1) ,se, 0,C2,2,0)
      call nbox_ireg3 (72,pr_shifted  , pe, endflag(2) ,se, 0,C2,2,0)
      call nbox_ireg3 (73,pr_extended , pe, endflag(3) ,se, 0,C2,2,0)
      call nbox_ireg3 (74,pr_narrowed , pe, endflag(4) ,se, 0,C2,2,0)
      return


      entry lat2_trap (box,ident,index,   text,nread,endkey)

      if (endkey.eq.'REDRAW') then
           call set_lat2_switches(veltype(itypeflag),
     $                                     ichngflag,iterpflag,idirflag,
     $                                     sc,sv,sr,sp,se,sn,
     $                                     pc,pv,pr,pp,pe,pn,
     $                                     srx,sry,
     $                             below,  sbelow,stbelow,sdbelow)
      else if (endkey.eq.'ARRIVED') then
           if (ident.ge.1.and.ident.le.numtypes) then
                call show_velfun_type (veltype(ident))
           end if
      else if (nread.gt.0.and.ident.ge.221.and.ident.le.224) then
        call recalculate_lat2 (ident-220,r_xfirst,r_xinc,r_xlast,r_nx,
     $                               endkey)
      else if (nread.gt.0.and.ident.ge.331.and.ident.le.334) then
        call recalculate_lat2 (ident-330,r_yfirst,r_yinc,r_ylast,r_ny,
     $                               endkey)
      else if (nread.gt.0.and.ident.eq.61) then
           vdelta=max(0.,vdelta)
      else if (nread.gt.0.and.ident.eq.62) then
           ncoef=max(0,min(ncoef,10))
      else if (nread.gt.0.and.ident.eq.63) then
           nrun=max(1,min(nrun,99))
      else if (endkey.eq.'RETURN') then
           call get_choice (ident,81,84,idirflag ,dirflag )
           call get_choice (ident,41,42,ichngflag,chngflag)
           call get_choice (ident,31,35,iterpflag,terpflag)
           call get_choice (ident,71,74,iendflag ,endflag )
           call get_choice (ident, 1,numtypes,itypeflag,typeflag)
      end if
      return

      entry lat2_apply (dummy,error)
      if (idirflag.eq.1.or.idirflag.eq.2) then
           control=0
           call f_lat2sample_velfuns (0,veltype(itypeflag),
     $        0,idirflag,iterpflag,ichngflag,iendflag,
     $        ncoef,nrun,vdelta,below,tbelow,dbelow,
     $        r_xfirst,r_xinc,r_nx,
     $        r_yfirst,r_yinc,r_ny,  error)
      else
           control=-1
           if (idirflag.eq.3) idirflag2=1
           if (idirflag.eq.4) idirflag2=2
           call f_lat2sample_velfuns (0,veltype(itypeflag),
     $        control,idirflag2,iterpflag,ichngflag,iendflag,
     $        ncoef,nrun,vdelta,below,tbelow,dbelow,
     $        r_xfirst,r_xinc,r_nx,
     $        r_yfirst,r_yinc,r_ny,  error)
           if (error.eq.0) then
                control=1
                if (idirflag.eq.3) idirflag2=2
                if (idirflag.eq.4) idirflag2=1
                call f_lat2sample_velfuns (0,veltype(itypeflag),
     $             control,idirflag2,iterpflag,ichngflag,iendflag,
     $             ncoef,nrun,vdelta,below,tbelow,dbelow,
     $             r_xfirst,r_xinc,r_nx,
     $             r_yfirst,r_yinc,r_ny,  error)
           end if
      end if
      return
      end




      subroutine recalculate_lat2 (iwhich,first,inc,last,n,endkey)
c     iwhich = 1 when first is the recently-changed variable.
c     iwhich = 2 when inc   is the recently-changed variable.
c     iwhich = 3 when last  is the recently-changed variable.
c     iwhich = 4 when n     is the recently-changed variable.

      implicit none
      integer iwhich,n
      real first,inc,last
      character*(*) endkey

      if (endkey.eq.'RESTORED') then
           if (inc.eq.0.0) inc=1.0
           if (n  .lt.1  ) n  =1
      else
           if (inc.eq.0.0) then
                call fbox_message('increment cannot be zero -'//
     $                                    ' reset to previous value')
                endkey='RESTORE'
                return
           else if (n.lt.1) then  
                call fbox_message('number of functions must exceed'//
     $                            ' zero - reset to previous value')
                endkey='RESTORE'
                return
           end if
      end if
      if (iwhich.eq.3.and.last.gt.first.and.inc.lt.0.0) inc=-inc
      if (iwhich.eq.3.and.last.lt.first.and.inc.gt.0.0) inc=-inc
      if (inc.gt.0.0.and.last.lt.first) last=first
      if (inc.lt.0.0.and.last.gt.first) last=first
      if (iwhich.eq.3) n=1+nint((last-first)/inc)
      last=first+(n-1)*inc
      return
      end




      subroutine set_lat2_switches (type2,ichngflag,iterpflag,idirflag,
     $                                      sc,sv,sr,sp,se,sn,
     $                                      pc,pv,pr,pp,pe,pn,
     $                                      srx,sry,
     $                              below,  sbelow,stbelow,sdbelow)
c     set switches for second lateral resampling box.

      implicit none
      character*(*) type2
      integer ichngflag,iterpflag,idirflag
      integer sc,sv,sr,sp,se,sn
      integer pc,pv,pr,pp,pe,pn
      integer srx,sry
      integer below                        ! value of toggle button.
      integer sbelow,stbelow,sdbelow

      sc=1                ! number of coefficients.
      sv=1                ! average spline velocity error.
      sr=1                ! number of functions in running average.
      sp=4                ! Retain current functions.
      se=4                ! running average end option.
      sn=4                ! linear or cubic resampling.
      srx=1               ! xbin resampling.
      sry=1               ! ybin resampling.
      sbelow=3            ! toggle for smoothing only below time/depth.
      stbelow=1           ! smoothing only below this time.
      sdbelow=1           ! smoothing only below this depth.
      if (iterpflag.ne.4       ) sc=-22
      if (iterpflag.ne.3       ) sv=-22
      if (iterpflag.ne.5       ) sr=-22
      if (iterpflag.le.2       ) sp=-4
      if (iterpflag.ne.5       ) se=-4
      if (ichngflag.eq.2       ) sn=-4
      if (ichngflag.eq.2.or.idirflag.eq.2) srx=-22
      if (ichngflag.eq.2.or.idirflag.eq.1) sry=-22
      if (ichngflag.eq.1                                    )  sbelow=-3
      if (ichngflag.eq.1.or.below.eq.0.or.type2(1:2).ne.'VT') stbelow=-1
      if (ichngflag.eq.1.or.below.eq.0.or.type2(1:2).ne.'VZ') sdbelow=-1
      pc=0
      pv=0
      pr=0
      pp=0
      pe=0
      pn=0
      if (sc.lt.0) pc=-55
      if (sv.lt.0) pv=-55
      if (sr.lt.0) pr=-55
      if (sp.lt.0) pp=-55
      if (se.lt.0) pe=-55
      if (sn.lt.0) pn=-55
      return
      end




c-------------------------------------------------------------------------------


      subroutine ray_register (box)

      implicit none
      integer box,ident,index,nread,dummy,error     ! arguments
      character*(*) text,endkey                     ! arguments
      include 'vel_boxes.inc'

      integer ichoice,choice(3),iwhich,which(4)
      save    ichoice,choice   ,iwhich,which   
 
ccc---------beginning of stuff needed for IBM.
      character*36 pr_selected,pr_all,pr_active
      save         pr_selected,pr_all,pr_active
      character*50 pr_longline,pr_longnote
      save         pr_longline,pr_longnote
      character*47 pr_raytr,pr_inverse,pr_awhile
      save         pr_raytr,pr_inverse,pr_awhile
      character*44 pr_resetnmo,pr_resetrms
      save         pr_resetnmo,pr_resetrms
      integer p0,p1,p2,p3,p4,m1,m2,m3,m4,m5,m44,m99 
      save    p0,p1,p2,p3,p4,m1,m2,m3,m4,m5,m44,m99
      data    p0,p1,p2,p3,p4,m1,m2,m3,m4,m5,m44,m99/
     $         0, 1, 2, 3, 4,-1,-2,-3,-4,-5,-44,-99/
      data pr_selected /'Process selected velocity functions:'/
      data pr_all      /'Process all velocity functions:     '/
      data pr_active   /'Process active velocity function #: '/
      data pr_longline /
     $    '--------------------------------------------------'/
      data pr_longnote /
     $    '--------------------- note -----------------------'/
      data pr_raytr  /'Raytrace to get NMO from RMS velocities        '/
      data pr_inverse/'Inverse raytrace to get RMS from NMO velocities'/
      data pr_awhile /'Raytracing will take awhile for many functions.'/
      data pr_resetnmo/'Reset NMO velocities equal to RMS velocities'/
      data pr_resetrms/'Reset RMS velocities equal to NMO velocities'/
ccc---------end of stuff needed for IBM.

      iraybox=box
      call get_choice (52,  51,53,ichoice,choice)
      call get_choice ( 1,   1, 4,iwhich ,which )

cccc                   ID  PROMPT   ISW  VAR   ISW ROW COL NCHAR NDEC
      call nbox_ireg3 (51,pr_selected,  p0, choice(1),p4,    1,1,2,0)
      call nbox_ireg3 (52,pr_all     ,  p0, choice(2),p4,    2,1,2,0)
      call nbox_ireg3 (53,pr_active  ,  p0, choice(3),p4,    3,1,2,0)
      call nbox_ireg (0,nselect, m5,1,43,5,0)
      call nbox_ireg (0,nfun   , m5,2,43,5,0)
      call nbox_ireg (0,ifun   , m5,3,43,5,0)
      call nbox_mreg (pr_longline,0,1)
      call nbox_ireg3 (1,pr_raytr   ,p0,which(1),p4,       0,1,2,0)
      call nbox_ireg3 (2,pr_inverse ,p0,which(2),p4,       0,1,2,0)
      call nbox_ireg3 (3,pr_resetnmo,p0,which(3),p4,       0,1,2,0)
      call nbox_ireg3 (4,pr_resetrms,p0,which(4),p4,       0,1,2,0)
      call nbox_mreg (pr_longnote,0,1)
      call nbox_mreg (pr_awhile,0,1)
      return


      entry ray_trap (box,ident,index,   text,nread,endkey)

      if (endkey.eq.'REDRAW') then
           continue
      else if (endkey.eq.'RETURN') then
           call get_choice (ident,51,53,ichoice,choice)
           call get_choice (ident, 1, 4,iwhich ,which )
      end if
      return


      entry ray_apply (dummy,error)
      if (iwhich.eq.1) then
           call f_raytrace_velfuns (ichoice,1,'VTRM',   error)
      else if (iwhich.eq.2) then
           call f_raytrace_velfuns (ichoice,1,'VTNM',   error)
      else if (iwhich.eq.3) then
           call f_raytrace_velfuns (ichoice,0,'VTRM',   error)
      else if (iwhich.eq.4) then
           call f_raytrace_velfuns (ichoice,0,'VTNM',   error)
      end if
      return
      end



c-------------------------------------------------------------------------------


      subroutine set_register (box)

      implicit none
      integer box,ident,index,nread,dummy,error     ! arguments
      character*(*) text,endkey                     ! arguments
      include 'vel_boxes.inc'

      integer ichoice,choice(4),itypeflag,typeflag(numtypes)
      save    ichoice,choice   ,itypeflag,typeflag    

ccc---------beginning of stuff needed for IBM.
      character*41 pr_selected,pr_all,pr_active,pr_default
      save         pr_selected,pr_all,pr_active,pr_default
      character*51 pr_longline
      save         pr_longline
      character*13 pr_type
      save         pr_type
      character*30 pr_versus,pr_time
      save         pr_versus,pr_time
      character*9  pr_thick
      save         pr_thick
      character*18 pr_nmo,pr_rms,pr_av,pr_int,pr_depth
      save         pr_nmo,pr_rms,pr_av,pr_int,pr_depth
      character*50 pr_msg1,pr_msg2,pr_msg3,pr_msg4
      character*50 pr_msg5,pr_msg6,pr_msg7,pr_msg8
      save         pr_msg1,pr_msg2,pr_msg3,pr_msg4
      save         pr_msg5,pr_msg6,pr_msg7,pr_msg8
      integer p0,p1,p2,p3,p4,m1,m2,m3,m4,m5,m44,m99 
      save    p0,p1,p2,p3,p4,m1,m2,m3,m4,m5,m44,m99
      data    p0,p1,p2,p3,p4,m1,m2,m3,m4,m5,m44,m99/
     $         0, 1, 2, 3, 4,-1,-2,-3,-4,-5,-44,-99/
      data pr_selected /'Reset selected velocity function types:  '/
      data pr_all      /'Reset all velocity function types:       '/
      data pr_active   /'Reset type of active velocity function #:'/
      data pr_default  /'Reset default velocity function type:    '/
      data pr_longline /
     $    '---------------------------------------------------'/
      data pr_type   /'Desired type:'/
      data pr_versus /'  versus     versus   versus  '/
      data pr_time   /'2-way time    depth    layer  '/
      data pr_thick  /'thickness'/
      data pr_nmo    /'     NMO velocity:'/
      data pr_rms    /'     RMS velocity:'/
      data pr_av     /' Average velocity:'/
      data pr_int    /'Interval velocity:'/
      data pr_depth  /'            Depth:'/
      data pr_msg1/'Note: This action will not re-calculate any       '/
      data pr_msg2/'velocities, since all velocity types are available'/
      data pr_msg3/'in memory at all times.  The velocity function    '/
      data pr_msg4/'types determine what are saved on a velocity file,'/
      data pr_msg5/'when the "mixed types" option is chosen while     '/
      data pr_msg6/'saving the file.  A velocity function type gets   '/
      data pr_msg7/'changed when you change the columns used while    '/
      data pr_msg8/'editing the active velocity function.             '/
ccc---------end of stuff needed for IBM.

      isetbox=box
      call get_choice (52,  51,54,ichoice  ,choice)
      call get_choice ( 1,   1,numtypes,itypeflag,typeflag)

cccc                   ID  PROMPT   ISW  VAR   ISW ROW COL NCHAR NDEC
      call nbox_ireg3 (51,pr_selected, p0, choice(1),p4,    1,1,2,0)
      call nbox_ireg3 (52,pr_all     , p0, choice(2),p4,    0,1,2,0)
      call nbox_ireg3 (53,pr_active  , p0, choice(3),p4,    0,1,2,0)
      call nbox_ireg3 (54,pr_default , p0, choice(4),p4,    0,1,2,0)
      call nbox_ireg (0,nselect        , m5,1,46,5,0)
      call nbox_ireg (0,nfun           , m5,0,46,5,0)
      call nbox_ireg (0,ifun           , m5,0,46,5,0)
      call nbox_creg (0,type(nfunmax+3), m5,0,46,0,0)
      call nbox_mreg (pr_longline,0,1)
      call nbox_mreg (pr_type   ,0,1)
      call nbox_mreg (pr_versus ,7,20)
      call nbox_mreg (pr_time   ,0,20)
      call nbox_mreg (pr_thick  ,0,41)
      call nbox_mreg (pr_nmo    ,9,1)
      call nbox_mreg (pr_rms    ,0,1)
      call nbox_mreg (pr_av     ,0,1)
      call nbox_mreg (pr_int    ,0,1)
      call nbox_mreg (pr_depth  ,0,1)
      call nbox_ireg3 ( 1,veltype( 1),p0, typeflag( 1),p4,  9,21,2,0)
      call nbox_ireg3 ( 2,veltype( 2),p0, typeflag( 2),p4, 10,21,2,0)
      call nbox_ireg3 ( 3,veltype( 3),p0, typeflag( 3),p4, 10,31,2,0)
      call nbox_ireg3 ( 4,veltype( 4),p0, typeflag( 4),p4, 10,41,2,0)
      call nbox_ireg3 ( 5,veltype( 5),p0, typeflag( 5),p4, 11,21,2,0)
      call nbox_ireg3 ( 6,veltype( 6),p0, typeflag( 6),p4, 11,31,2,0)
      call nbox_ireg3 ( 7,veltype( 7),p0, typeflag( 7),p4, 11,41,2,0)
      call nbox_ireg3 ( 8,veltype( 8),p0, typeflag( 8),p4, 12,21,2,0)
      call nbox_ireg3 ( 9,veltype( 9),p0, typeflag( 9),p4, 12,31,2,0)
      call nbox_ireg3 (10,veltype(10),p0, typeflag(10),p4, 12,41,2,0)
      call nbox_ireg3 (11,veltype(11),p0, typeflag(11),p4, 13,21,2,0)
      call nbox_mreg (pr_msg1,14,1)
      call nbox_mreg (pr_msg2, 0,0)
      call nbox_mreg (pr_msg3, 0,0)
      call nbox_mreg (pr_msg4, 0,0)
      call nbox_mreg (pr_msg5, 0,0)
      call nbox_mreg (pr_msg6, 0,0)
      call nbox_mreg (pr_msg7, 0,0)
      call nbox_mreg (pr_msg8, 0,0)
      return


      entry set_trap (box,ident,index,   text,nread,endkey)

      if (endkey.eq.'REDRAW') then
           continue
      else if (endkey.eq.'ARRIVED') then
           if (ident.ge.1.and.ident.le.numtypes) then
                call show_velfun_type (veltype(ident))
           end if
      else if (endkey.eq.'RETURN') then
           call get_choice (ident,51,54,ichoice  ,choice)
           call get_choice (ident, 1,numtypes,itypeflag,typeflag)
      end if
      return


      entry set_apply (dummy,error)
      call f_velfun_types (ichoice,veltype(itypeflag))
      error=0
      return
      end




c-------------------------------------------------------------------------------


      subroutine vfid_register (box)

      implicit none
      integer box,ident,index,nread,dummy,error     ! arguments
      character*(*) text,endkey                     ! arguments
      integer i
      include 'vel_boxes.inc'

      character*3 prefix
      integer ichoice,choice(4),iwhich,which(3),iblank
      save    ichoice,choice,iwhich,which,prefix,iblank

ccc---------beginning of stuff needed for IBM.
      character*41 pr_selected,pr_all,pr_active,pr_blank
      save         pr_selected,pr_all,pr_active,pr_blank
      character*50 pr_longline
      save         pr_longline
      character*6  pr_prefix
      save         pr_prefix
      character*34 pr_counter,pr_xloc,pr_yxloc
      save         pr_counter,pr_xloc,pr_yxloc
      character*43 pr_msg1,pr_msg2,pr_msg3,pr_msg4
      save         pr_msg1,pr_msg2,pr_msg3,pr_msg4
      integer p0,p1,p2,p3,p4,m1,m2,m3,m4,m5,m44,m99 
      save    p0,p1,p2,p3,p4,m1,m2,m3,m4,m5,m44,m99
      data    p0,p1,p2,p3,p4,m1,m2,m3,m4,m5,m44,m99/
     $         0, 1, 2, 3, 4,-1,-2,-3,-4,-5,-44,-99/
      data pr_selected /'Reset selected velocity function names:  '/
      data pr_all      /'Reset all velocity function names:       '/
      data pr_active   /'Reset name of active velocity function #:'/
      data pr_blank    /'Reset blank velocity function names:     '/
      data pr_longline /
     $    '--------------------------------------------------'/
      data pr_prefix /'prefix'/
      data pr_counter/'Set name to prefix plus a counter '/
      data pr_xloc   /'Set name to prefix plus X location'/
      data pr_yxloc  /'Set name to Y plus X location     '/
      data pr_msg1   /'Blank velocity function names will be reset'/
      data pr_msg2   /'as chosen above whenever a new velocity    '/
      data pr_msg3   /'function is inserted or a CPS velocity     '/
      data pr_msg4   /'file is saved.                             '/
ccc---------end of stuff needed for IBM.

      data    prefix/'VEL'/
      data    iwhich/2/         ! needed because vfid_apply_blanks
                                ! may may be called before vfid_register

      ivfidbox=box
      call get_choice (52,  51,54,ichoice,choice)
      call get_choice ( 2,   1, 3,iwhich ,which )

cccc                   ID  PROMPT   ISW  VAR   ISW ROW COL NCHAR NDEC
      call nbox_ireg3 (51,pr_selected, p0, choice(1),p4,    1,1,2,0)
      call nbox_ireg3 (52,pr_all     , p0, choice(2),p4,    2,1,2,0)
      call nbox_ireg3 (53,pr_active  , p0, choice(3),p4,    3,1,2,0)
      call nbox_ireg3 (54,pr_blank   , p0, choice(4),p4,    4,1,2,0)
      call nbox_ireg (0,nselect, m5,1,46,5,0)
      call nbox_ireg (0,nfun   , m5,2,46,5,0)
      call nbox_ireg (0,ifun   , m5,3,46,5,0)
      call nbox_ireg (0,iblank , m5,4,46,5,0)
      call nbox_mreg (pr_longline,0,1)
      call nbox_creg2 (20,pr_prefix , p0, prefix  ,p1,  0,18,0,0)
      call nbox_ireg3 ( 1,pr_counter, p0, which(1),p4,  0, 1,2,0)
      call nbox_ireg3 ( 2,pr_xloc   , p0, which(2),p4,  0, 1,2,0)
      call nbox_ireg3 ( 3,pr_yxloc  , p0, which(3),p4,  0, 1,2,0)
      call nbox_mreg (pr_longline,0,1)
      call nbox_mreg (pr_msg1,0,1)
      call nbox_mreg (pr_msg2,0,1)
      call nbox_mreg (pr_msg3,0,1)
      call nbox_mreg (pr_msg4,0,1)
      return


      entry vfid_trap (box,ident,index,   text,nread,endkey)

      if (endkey.eq.'REDRAW') then
           iblank=0
           do i=1,nfun
             if (vfid(i).eq.' '.or.vfid(i).eq.'blank') iblank=iblank+1
           end do
      else if (nread.gt.0.and.ident.eq.20.and.iwhich.eq.3) then
           call get_choice (2    , 1, 3,iwhich ,which )
      else if (endkey.eq.'RETURN') then
           call get_choice (ident,51,54,ichoice,choice)
           call get_choice (ident, 1, 3,iwhich ,which )
      end if
      return


      entry vfid_apply (dummy,error)
      call f_velfun_vfids (ichoice,iwhich,prefix)
      error=0
      return


      entry vfid_apply_blanks
      call f_velfun_vfids (4,iwhich,prefix)
      return


      entry vfid_apply_blank (index)
      call f_velfun_vfid (iwhich,prefix,index)
      return
      end



c-------------------------------------------------------------------------------


      subroutine head_register (box)

      implicit none
      integer box,ident,index,nread,dummy,error     ! arguments
      character*(*) text,endkey                     ! arguments
      include 'vel_boxes.inc'

      integer     ichoice,choice(3),which(7)
      save        ichoice,choice   ,which   

ccc---------beginning of stuff needed for IBM.
      character*43 pr_selected,pr_all,pr_active
      save         pr_selected,pr_all,pr_active
      character*52 pr_longline
      save         pr_longline
      character*26 pr_project,pr_line,pr_rdate,pr_pdate,pr_userid
      save         pr_project,pr_line,pr_rdate,pr_pdate,pr_userid
      character*26 pr_comment
      save         pr_comment
      character*37 pr_vname
      save         pr_vname
      character*42 pr_msg1,pr_msg2
      save         pr_msg1,pr_msg2
      integer p0,p1,p2,p3,p4,m1,m2,m3,m4,m5,m44,m99 
      save    p0,p1,p2,p3,p4,m1,m2,m3,m4,m5,m44,m99
      data    p0,p1,p2,p3,p4,m1,m2,m3,m4,m5,m44,m99/
     $         0, 1, 2, 3, 4,-1,-2,-3,-4,-5,-44,-99/
      data pr_selected /'Reset selected velocity function headers:  '/
      data pr_all      /'Reset all velocity function headers:       '/
      data pr_active   /'Reset header of active velocity function #:'/
      data pr_longline /
     $    '----------------------------------------------------'/
      data pr_project /'Set project to this field:'/
      data pr_line    /'Set line    to this field:'/
      data pr_rdate   /'Set rdate   to this field:'/
      data pr_pdate   /'Set pdate   to this field:'/
      data pr_userid  /'Set userid  to this field:'/
      data pr_comment /'Set comment to this field:'/
      data pr_vname   /'Set comment to velocity function name'/
      data pr_msg1    /'The above fields are also defaults for new'/
      data pr_msg2    /'and cleared velocity functions.           '/
ccc---------end of stuff needed for IBM.

      data        which/7*0/

      iheadbox=box
      call get_choice (52,  51,53,ichoice,choice)

cccc                   ID  PROMPT   ISW  VAR   ISW ROW COL NCHAR NDEC
      call nbox_ireg3 (51,pr_selected,  p0, choice(1),p4,    1,1,2,0)
      call nbox_ireg3 (52,pr_all     ,  p0, choice(2),p4,    2,1,2,0)
      call nbox_ireg3 (53,pr_active  ,  p0, choice(3),p4,    3,1,2,0)
      call nbox_ireg (0,nselect, m5,1,48,5,0)
      call nbox_ireg (0,nfun   , m5,2,48,5,0)
      call nbox_ireg (0,ifun   , m5,3,48,5,0)
      call nbox_mreg (pr_longline,0,1)
      call nbox_creg (31,project(nfunmax+3),p1,  5,32,0,0)
      call nbox_creg (32,line   (nfunmax+3),p1,  0, 0,0,0)
      call nbox_creg (33,rdate  (nfunmax+3),p1,  0, 0,0,0)
      call nbox_creg (34,pdate  (nfunmax+3),p1,  0, 0,0,0)
      call nbox_creg (35,userid (nfunmax+3),p1,  0, 0,0,0)
      call nbox_creg (36,comment(nfunmax+3),p1,  0, 0,0,0)
      call nbox_ireg3 ( 1,pr_project, p0, which(1),p3,  5,1,2,0)
      call nbox_ireg3 ( 2,pr_line   , p0, which(2),p3,  0,1,2,0)
      call nbox_ireg3 ( 3,pr_rdate  , p0, which(3),p3,  0,1,2,0)
      call nbox_ireg3 ( 4,pr_pdate  , p0, which(4),p3,  0,1,2,0)
      call nbox_ireg3 ( 5,pr_userid , p0, which(5),p3,  0,1,2,0)
      call nbox_ireg3 ( 6,pr_comment, p0, which(6),p3,  0,1,2,0)
      call nbox_ireg3 ( 7,pr_vname  , p0, which(7),p3,  0,1,2,0)
      call nbox_mreg (pr_msg1,0,1)
      call nbox_mreg (pr_msg2,0,1)
      return


      entry head_trap (box,ident,index,   text,nread,endkey)

      if (endkey.eq.'REDRAW') then
           continue
      else if (nread.gt.0) then
           if (ident.eq.31) which(1)=1
           if (ident.eq.32) which(2)=1
           if (ident.eq.33) which(3)=1
           if (ident.eq.34) which(4)=1
           if (ident.eq.35) which(5)=1
           if (ident.eq.36) which(6)=1
           if (ident.eq.36) which(7)=0
      else if (endkey.eq.'RETURN') then
           call get_choice (ident,51,53,ichoice,choice)
           if (ident.eq.6.and.which(6).ne.0) which(7)=0
           if (ident.eq.7.and.which(7).ne.0) which(6)=0
      end if
      return


      entry head_apply (dummy,error)
      call f_velfun_headers (ichoice,which)
      error=0
      return
      end



c-------------------------------------------------------------------------------


      subroutine del_register (box)

      implicit none
      integer box,ident,index,nread,dummy,error     ! arguments
      character*(*) text,endkey                     ! arguments
      include 'vel_boxes.inc'

      integer ichoice,choice(3)

ccc---------beginning of stuff needed for IBM.
        character*35 pr_selected,pr_all,pr_active
        save         pr_selected,pr_all,pr_active
        integer p0,p1,p2,p3,p4,m1,m2,m3,m4,m5,m44,m99 
        save    p0,p1,p2,p3,p4,m1,m2,m3,m4,m5,m44,m99
        data    p0,p1,p2,p3,p4,m1,m2,m3,m4,m5,m44,m99/
     $           0, 1, 2, 3, 4,-1,-2,-3,-4,-5,-44,-99/
        data pr_selected /'Delete selected velocity functions:'/
        data pr_all      /'Delete all velocity functions:     '/
        data pr_active   /'Delete active velocity function #: '/
ccc---------end of stuff needed for IBM.

      save    ichoice,choice   

      idelbox=box
      call get_choice (52,  51,53,ichoice,choice)

cccc                   ID  PROMPT   ISW  VAR   ISW ROW COL NCHAR NDEC
      call nbox_ireg3 (51,pr_selected,  p0, choice(1),p4,    1,1,2,0)
      call nbox_ireg3 (52,pr_all     ,  p0, choice(2),p4,    2,1,2,0)
      call nbox_ireg3 (53,pr_active  ,  p0, choice(3),p4,    3,1,2,0)
      call nbox_ireg (0,nselect, m5,1,41,5,0)
      call nbox_ireg (0,nfun   , m5,2,41,5,0)
      call nbox_ireg (0,ifun   , m5,3,41,5,0)
      return


      entry del_trap (box,ident,index,   text,nread,endkey)

      if (endkey.eq.'REDRAW') then
           continue
      else if (endkey.eq.'RETURN') then
           call get_choice (ident,51,53,ichoice,choice)
      end if
      return


      entry del_apply (dummy,error)
      call f_delete_velfuns (ichoice,   error)
      return
      end




c-------------------------------------------------------------------------------


      subroutine mult_register (box)

      implicit none
      integer box,ident,index,nread,dummy,error     ! arguments
      character*(*) text,endkey                     ! arguments
      include 'vel_boxes.inc'

      integer ichoice,choice(3)
      integer itypeflag,typeflag(numtypes)
      integer p0t,p1t,p0x,p1x

      real constant,ta,tb,xa,xb
      save constant,ta,tb,xa,xb
      data constant,ta,tb,xa,xb/1.0,0.0,999999.0,0.0,999999.0/

ccc---------beginning of stuff needed for IBM.
        character*39 pr_selected,pr_all,pr_active
        save         pr_selected,pr_all,pr_active
        integer p0,p1,p2,p3,p4,m1,m2,m3,m4,m5,m44,m99 
        save    p0,p1,p2,p3,p4,m1,m2,m3,m4,m5,m44,m99
        data    p0,p1,p2,p3,p4,m1,m2,m3,m4,m5,m44,m99/
     $           0, 1, 2, 3, 4,-1,-2,-3,-4,-5,-44,-99/
        data pr_selected /'Operate on selected velocity functions:'/
        data pr_all      /'Operate on all velocity functions:     '/
        data pr_active   /'Operate on active velocity function #: '/
ccc---------end of stuff needed for IBM.

      character*49 pr_longline
      save         pr_longline
      character*13 pr_type
      save         pr_type
      character*21 pr_versus,pr_time
      save         pr_versus,pr_time
      character*18 pr_nmo,pr_rms,pr_av,pr_int,pr_depth
      save         pr_nmo,pr_rms,pr_av,pr_int,pr_depth
      character*35 pr_constant
      save         pr_constant
      character*29 pr_times
      save         pr_times
      character*29 pr_depths
      save         pr_depths

      data pr_longline /
     $    '-------------------------------------------------'/
      data pr_type     /'Desired type:'/
      data pr_versus   /'  versus     versus  '/
      data pr_time     /'2-way time    depth  '/
      data pr_nmo      /'     NMO velocity:'/
      data pr_rms      /'     RMS velocity:'/
      data pr_av       /' Average velocity:'/
      data pr_int      /'Interval velocity:'/
      data pr_depth    /'            Depth:'/
      data pr_constant /'constant to multiply velocities by:'/
      data pr_times    /'min/max times to operate on: '/
      data pr_depths   /'min/max depths to operate on:'/

      save    ichoice,choice   
      save    itypeflag,typeflag
      save    p0t,p1t,p0x,p1x
      data    p0t,p1t,p0x,p1x/0,1,0,1/

      imultbox=box
      call get_choice (52,  51,53,ichoice,choice)
      call get_choice ( 1,   1,numtypes,itypeflag,typeflag)


cccc                   ID  PROMPT      ISW   VAR     ISW  ROW COL NCHAR NDEC
      call nbox_ireg3 (51,pr_selected,  p0, choice(1),p4,    1, 1,2,0)
      call nbox_ireg3 (52,pr_all     ,  p0, choice(2),p4,    2, 1,2,0)
      call nbox_ireg3 (53,pr_active  ,  p0, choice(3),p4,    3, 1,2,0)
      call nbox_ireg  ( 0,                  nselect  ,m5,    1,44,5,0)
      call nbox_ireg  ( 0,                  nfun     ,m5,    2,44,5,0)
      call nbox_ireg  ( 0,                  ifun     ,m5,    3,44,5,0)

      call nbox_mreg  (   pr_longline                      , 4, 1)
      call nbox_mreg  (   pr_type                          , 5, 1)
      call nbox_mreg  (   pr_versus                        , 0,20)
      call nbox_mreg  (   pr_time                          , 0,20)
      call nbox_mreg  (   pr_nmo                           , 8, 1)
      call nbox_mreg  (   pr_rms                           , 0, 1)
      call nbox_mreg  (   pr_av                            , 0, 1)
      call nbox_mreg  (   pr_int                           , 0, 1)
      call nbox_mreg  (   pr_depth                         , 0, 1)
      call nbox_ireg3 ( 1,veltype(1) ,p0 ,typeflag(1) ,p4,   8,22,2,0)
      call nbox_ireg3 ( 2,veltype(2) ,p0 ,typeflag(2) ,p4,   9,22,2,0)
      call nbox_ireg3 ( 3,veltype(3) ,p0 ,typeflag(3) ,p4,   9,33,2,0)
      call nbox_ireg3 ( 5,veltype(5) ,p0 ,typeflag(5) ,p4,  10,22,2,0)
      call nbox_ireg3 ( 6,veltype(6) ,p0 ,typeflag(6) ,p4,  10,33,2,0)
      call nbox_ireg3 ( 8,veltype(8) ,p0 ,typeflag(8) ,p4,  11,22,2,0)
      call nbox_ireg3 ( 9,veltype(9) ,p0 ,typeflag(9) ,p4,  11,33,2,0)
      call nbox_ireg3 (11,veltype(11),p0 ,typeflag(11),p4,  12,22,2,0)
      call nbox_mreg  (   pr_longline                      ,13, 1)
      call nbox_freg2 (21,pr_constant,p0 ,constant    ,p1,   0, 1,8,6)
      call nbox_freg2 (22,pr_times   ,p0t,ta          ,p1t,  0, 1,8,6)
      call nbox_freg  (23,                tb          ,p1t, -1,-1,8,6)
      call nbox_freg2 (24,pr_depths  ,p0x,xa          ,p1x,  0, 1,8,6)
      call nbox_freg  (25,                xb          ,p1x, -1,-1,8,6)
      return


      entry mult_trap (box,ident,index,   text,nread,endkey)

      if (endkey.eq.'REDRAW') then
           p0t=-55
           p1t=-22
           p0x=-55
           p1x=-22
           if (veltype(itypeflag)(1:2).eq.'VT') then
                p0t=0
                p1t=1
           else if (veltype(itypeflag)(1:2).eq.'VZ') then
                p0x=0
                p1x=1
           end if
      else if (endkey.eq.'ARRIVED') then
           if (ident.ge.1.and.ident.le.numtypes) then
                call show_velfun_type (veltype(ident))
           end if
      else if (endkey.eq.'RETURN') then
           call get_choice (ident,51,53,ichoice,choice)
           call get_choice (ident, 1,numtypes,itypeflag,typeflag)
      end if
      return


      entry mult_apply (dummy,error)
      call f_multiply_velfuns (ichoice,veltype(itypeflag),
     $                         constant,ta,tb,xa,xb,   error)
      return
      end




c-------------------------------------------------------------------------------



      subroutine misc_register (box)

      implicit none
      integer box,ident,index,nread,dummy,error     ! arguments
      character*(*) text,endkey                     ! arguments
      include 'vel_boxes.inc'

      real    vwater,btime,bvel
      integer ichoice,choice(3)
      integer iwhich,which(5)
      save    ichoice,choice,iwhich,which,vwater,btime,bvel

ccc---------beginning of stuff needed for IBM.
      character*39 pr_selected,pr_all,pr_active
      save         pr_selected,pr_all,pr_active
      character*50 pr_longline
      save         pr_longline
      character*27 pr_meters,pr_feet
      save         pr_meters,pr_feet
      character*44 pr_remove
      save         pr_remove
      character*17 pr_water
      save         pr_water
      character*23 pr_normally
      save         pr_normally
      character*43 pr_msg1,pr_msg2
      save         pr_msg1,pr_msg2
      integer p0,p1,p2,p3,p4,m1,m2,m3,m4,m5,m44,m99 
      save    p0,p1,p2,p3,p4,m1,m2,m3,m4,m5,m44,m99
      data    p0,p1,p2,p3,p4,m1,m2,m3,m4,m5,m44,m99/
     $         0, 1, 2, 3, 4,-1,-2,-3,-4,-5,-44,-99/
      data pr_selected /'Operate on selected velocity functions:'/
      data pr_all      /'Operate on all velocity functions:     '/
      data pr_active   /'Operate on active velocity function #: '/
      data pr_longline /
     $    '--------------------------------------------------'/
      data pr_meters  /'Convert from feet to meters'/
      data pr_feet    /'Convert from meters to feet'/
      data pr_remove  /'Remove water velocity for cascaded migration'/
      data pr_water   /'water velocity   '/
      data pr_normally/'(normally 5000 or 1500)'/
      data pr_msg1    /'Warning: After changing between feet and   '/
      data pr_msg2    /'meters, make sure your offsets are correct!'/
ccc---------end of stuff needed for IBM.

      data    vwater,btime,bvel/5000.,5.0,10000./

      imiscbox=box
      call get_choice (52,  51,53,ichoice,choice)
      call get_choice ( 1,   1, 5,iwhich ,which )

cccc                   ID  PROMPT   ISW  VAR   ISW ROW COL NCHAR NDEC
      call nbox_ireg3 (51,pr_selected,  p0, choice(1),p4,    1,1,2,0)
      call nbox_ireg3 (52,pr_all     ,  p0, choice(2),p4,    2,1,2,0)
      call nbox_ireg3 (53,pr_active  ,  p0, choice(3),p4,    3,1,2,0)
      call nbox_ireg (0,nselect, m5,1,45,5,0)
      call nbox_ireg (0,nfun   , m5,2,45,5,0)
      call nbox_ireg (0,ifun   , m5,3,45,5,0)
      call nbox_mreg (pr_longline,0,1)
      call nbox_ireg3 ( 1,pr_meters  , p0, which(1),p4, 0, 1,2,0)
      call nbox_ireg3 ( 2,pr_feet    , p0, which(2),p4, 0, 1,2,0)
      call nbox_ireg3 ( 3,pr_remove  , p0, which(3),p4, 0, 1,2,0)
      call nbox_mreg  (   pr_longline,                  0, 1)
      call nbox_freg2 (20,pr_water   , p0, vwater  ,p1, 0, 3,5,0)
      call nbox_creg  ( 0,pr_normally, p0,             -1,-1,0,0)
      call nbox_mreg  (   pr_longline,                  0, 1)
      call nbox_mreg  (   pr_msg1    ,                  0, 1)
      call nbox_mreg  (   pr_msg2    ,                  0, 1)
      return


      entry misc_trap (box,ident,index,   text,nread,endkey)

      if (endkey.eq.'REDRAW') then
           continue
      else if (nread.gt.0.and.ident.eq.20) then
           call get_choice (3    , 1, 5,iwhich ,which )
      else if (nread.gt.0.and.ident.eq.30) then
           call get_choice (4    , 1, 5,iwhich ,which )
      else if (nread.gt.0.and.ident.eq.40) then
           call get_choice (5    , 1, 5,iwhich ,which )
      else if (endkey.eq.'RETURN') then
           call get_choice (ident,51,53,ichoice,choice)
           call get_choice (ident, 1, 5,iwhich ,which )
      end if
      return


      entry misc_apply (dummy,error)
      call f_misc_actions (ichoice,iwhich,vwater,btime,bvel,   error)
      return
      end




c-------------------------------------------------------------------------------


      subroutine offmute_register (box)

      implicit none
      integer box,ident,index,nread,dummy,error     ! arguments
      character*(*) text,endkey                     ! arguments
      include 'vel_boxes.inc'
      character*80 msg
      integer i,ierr
      logical fbox_managed,update
      real factor

      integer immediate,choice(3)
      save    immediate,choice

ccc---------beginning of stuff needed for IBM.
      character*16 pr_action,pr_help
      save         pr_action,pr_help
      character*22 pr_meters,pr_feet
      save         pr_meters,pr_feet
      character*14 pr_minimum,pr_maximum
      save         pr_minimum,pr_maximum
      character*20 pr_not
      save         pr_not
      character*30 pr_maxoff
      save         pr_maxoff
      character*43 pr_after
      save         pr_after
      character*1  pr_blank
      save         pr_blank
      character*6  pr_offset
      save         pr_offset
      character*9  pr_time
      save         pr_time
      integer nmutemax2,p0,p1,p2,p3,p4,m1,m2,m3,m4,m5,m44,m99 
      save    nmutemax2,p0,p1,p2,p3,p4,m1,m2,m3,m4,m5,m44,m99
      data    nmutemax2,p0,p1,p2,p3,p4,m1,m2,m3,m4,m5,m44,m99/
     $        nmutemax , 0, 1, 2, 3, 4,-1,-2,-3,-4,-5,-44,-99/
      data pr_action /'Immediate action'/
      data pr_help   /'      (see help)'/
      data pr_meters /'Convert feet to meters'/
      data pr_feet   /'Convert meters to feet'/
      data pr_minimum/'Minimum offset'/
      data pr_maximum/'Maximum offset'/
      data pr_not    /'Do not mute the data'/
      data pr_maxoff /'Mute at maximum offset = depth'/
      data pr_after  /'Use the following mute after NMO correction'/
      data pr_blank  /' '/
      data pr_offset /'OFFSET'/
      data pr_time   /'MUTE TIME'/
ccc---------end of stuff needed for IBM.

      data    immediate/0/

      ioffmutebox=box
      call get_choice (5,  4,6,muteflag,choice)

cccc                  ID  PROMPT            ISW VAR ISW ROW COL NCHAR NDEC
      call nbox_ireg3 (1,pr_action , p0, immediate,p3  ,1, 1,2,0)
      call nbox_creg ( 0,pr_help   , p0,                2, 1,0,0)
      call nbox_creg (50,pr_meters , p2,                1,26,0,0)
      call nbox_creg (60,pr_feet   , p2,                2,26,0,0)
      call nbox_freg2 (2,pr_minimum, p0, offmin   ,p1,  4, 1,6,0)
      call nbox_freg2 (3,pr_maximum, p0, offmax   ,p1, -1,27,6,0)
      call nbox_ireg3 (4,pr_not    , p0, choice(1),p4,  0, 1,2,0)
      call nbox_ireg3 (5,pr_maxoff , p0, choice(2),p4,  0, 1,2,0)
      call nbox_ireg3 (6,pr_after  , p0, choice(3),p4,  0, 1,2,0)

cccc                    N     NMAX   IROW NROW
      call fbox_rega (nmute,nmutemax2,0,   5)
cccc                   ID  PROMPT      ISW  VAR     ISW COL NCHAR NDEC
      call nbox_xrega (0 ,pr_blank  , p0  , nmute , m44, 11,  3,  0)
      call nbox_frega (12,pr_offset , p0  , omute ,  p1,  0,  8,  0)
      call nbox_frega (13,pr_time   , p0  , tmute ,  p1,  0,  8,  3)
      return


      entry offmute_trap (box,ident,index,   text,nread,endkey)

      update=raymsg(ifun).ne.' '.and.fbox_managed(ipickbox)
      if (endkey.eq.'REDRAW') then
           return
      else if (endkey.eq.'ARRIVED') then
           return
      else if (endkey.eq.'DO') then
           go to 999
      else if (endkey.eq.'RETURN'.and.ident.ge.50) then
           factor=3.28084
           if (ident.eq.50) factor=1./factor
           offmin=offmin*factor
           offmax=offmax*factor
           do i=1,nmute
                if (omute(i).ne.fnil) omute(i)=omute(i)*factor
           end do
           nread=1
      else if ((endkey.eq.'RETURN'.or.endkey.eq.'RESTORED').and.
     $                              ident.ge.4.and.ident.le.6) then
           call get_choice (ident,4,6,muteflag,choice)
      else if (nread.gt.0.and.(ident.eq.12.or.ident.eq.13)) then
           call get_choice (6    ,4,6,muteflag,choice)
      else if (ident.eq.2.and.nread.gt.0) then
           offmin=min(offmax,max(offmin,0.))
      else if (ident.eq.3.and.nread.gt.0) then
           offmax=max(offmax,offmin)
      end if
      if (endkey.ne.'REMOVED'.and.nread.gt.0.and.index.gt.nmute) then
           if (ident.eq.12.or.ident.eq.13) nmute=index
           if (ident.eq.12) tmute(nmute)=fnil
           if (ident.eq.13) omute(nmute)=fnil
      end if
      if (muteflag.eq.3) then
           if (nmute.eq.0) then
                call fbox_message ('You must supply offsets and mutes')
                go to 999
           else
                do i=1,nmute
                     if (omute(i).eq.fnil.or.tmute(i).eq.fnil) then
                         call fbox_message ('Missing offsets or mutes')
                         go to 999
                     end if
                end do
           end if
           if (nmute.ge.2) then
                do i=2,nmute
                     if (omute(i).le.omute(i-1).or.
     $                   tmute(i).le.tmute(i-1)) then
                       call fbox_message
     $                     ('OFFSETS or MUTE TIMES are not increasing')
                       go to 999
                     end if
                end do
           end if
      end if
      if (update.and.nread.gt.0.and.immediate.ne.0) then
           call update_velfun (ifun,1,type(ifun),   msg,ierr)
           call pick_undo_sensitize (vel,1)
           call fbroadcast(vel,SENDER_WBOX,MESSAGE_MODIFY,
     $                                999,ifun,ifun,1,n(ifun))
           if (ierr.eq.0) then
                msg='velocity function raytraced'
           else
                msg='velocity function RAYTRACE ERROR'
           end if
           call fbox_message ('active '//msg)
           call fbox_messageline (ipickbox,msg)
           return
      end if
999   if (update) call fbox_messageline (ipickbox,'no change')
      return


      entry offmute_apply (dummy,error)
      error=0
      return
      end






      subroutine vel_immediate (box,word,ifun,nfun,step)
c     immediately prints a message to the windowbox message area.
c     does nothing if not in a windowbox trap.

      implicit none
      character*(*) word
      integer box,ifun,nfun,step
      character*80 msg

      if (step.le.0.or.nfun.le.step) return
      if (ifun.gt.1.and.ifun.lt.nfun.and.mod(ifun,step).ne.0) return
      msg=word
      write (msg(len(word)+2:80),1000) ifun,nfun
1000  format ('function',i5,' of',i5)
      call fbox_messageline (box,msg)
      return
      end






      subroutine set_velfun_type (i,want)
c     sets velocity function to desired type (indicated by want).
c     sets type to VTNM if want is invalid.
c     this type is used for saving velocity functions, and for
c        setting the independent variables in the pick_box.

      implicit none
      character*(*) want
      integer i,j,ntypes
      character*60 descrip
      include 'vel_boxes.inc'

      call velfun_type_cc2ii (want,    j,descrip,ntypes)
      if (j.gt.0) then
           type(i)=want
      else
           type(i)='VTNM'
      end if
      changeflag=1
      return
      end






      subroutine increment_velfun_type (i)

      implicit none
      integer i,j,ntypes
      character*60 descrip
      include 'vel_boxes.inc'

      call velfun_type_cc2ii (type(i),   j,descrip,ntypes)
      j=mod(j,ntypes)+1
      call velfun_type_ii2cc (j,   type(i),descrip,ntypes)
      changeflag=1
      return
      end




      subroutine show_velfun_type (want)

      implicit none
      character*(*) want
      integer j,ntypes
      character*60 descrip

      call velfun_type_cc2ii (want,   j,descrip,ntypes)
      call fbox_maybe (descrip)
      return
      end





      subroutine register_active_velfun
c     Call this routine whenever a new velocity function has been chosen to
c         be the current active one.  This routine should be called AFTER
c         resetting ifun.
c     If the pickbox exists, the function is registered with it.

      implicit none
      integer p
      include 'vel_boxes.inc'

      if (ipickbox.gt.0) then
           call fbox_nnewreg (ipickbox,21,n      (ifun))
           call fbox_inewreg (ipickbox, 7,n      (ifun))
           call fbox_fnewreg (ipickbox, 8,xbin   (ifun))
           call fbox_fnewreg (ipickbox, 9,ybin   (ifun))
           call fbox_cnewreg (ipickbox, 2,vfid   (ifun))
           call fbox_cnewreg (ipickbox,10,type   (ifun))
           call fbox_cnewreg (ipickbox,13,project(ifun))
           call fbox_cnewreg (ipickbox,14,line   (ifun))
           call fbox_cnewreg (ipickbox,15,rdate  (ifun))
           call fbox_cnewreg (ipickbox,16,pdate  (ifun))
           call fbox_cnewreg (ipickbox,17,userid (ifun))
           call fbox_cnewreg (ipickbox,18,comment(ifun))
           call fbox_cnewreg (ipickbox,30,select (ifun))
           call fbox_cnewreg (ipickbox,31,errmsg (ifun))
           call fbox_cnewreg (ipickbox,32,raymsg (ifun))
           call offset_index_words (p,depths,point(ifun))
           p=p+1
           call fbox_fnewreg (ipickbox,21,depths (p))
           call fbox_fnewreg (ipickbox,22,times  (p))
           call fbox_fnewreg (ipickbox,27,vnmo   (p))
           call fbox_fnewreg (ipickbox,24,vrms   (p))
           call fbox_fnewreg (ipickbox,25,vav    (p))
           call fbox_fnewreg (ipickbox,26,vint   (p))
           call fbox_fnewreg (ipickbox,23,thick  (p))
           call fbox_fnewreg (ipickbox,28,offpick(p))
           call pick_undo_sensitize (vel,0)
      end if
      return
      end


