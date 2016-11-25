
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


ccccccccccccc   vel_fbridge.f   ccccccccccccccccc

cccc      message_wbox (sender,message,one,two,three,four,five)

cccc      f_save_tempfile  (ibox,   ierr)
cccc      f_read_tempfile  (dummy,  ierr)    <-- entry
cccc      f_close_tempfile                   <-- entry

cccc      f_all_undo_sensitize       (ibox)

cccc      f_autoname_workfile
cccc      f_autosave_workfile_cursor (ibox)
cccc      f_autosave_workfile        (ibox)
cccc      f_autodelete_workfile

cccc      ierr = read_velocities (buffer,        muffer)  <-- integer function
cccc      ierr = save_velocities (buffer,tbuf,   muffer)  <-- integer function
cccc      ierr = validate_velocities (buffer,
cccc                             nhx,nhy,nfun,   muffer)  <-- integer function
cccc      remove_velocities

cccc      f_read_inquire (      filename,iexist,          msg,ierr)
cccc      f_save_inquire (      filename,iexist,          msg,ierr)
cccc      f_read_velfile (broad,filename,ichoice,         msg,ierr)
cccc      f_save_velfile (stuff,filename,ichoice,type2,   msg,ierr)

cccc      f_activate_velfun     (i                                )
cccc      f_insert_blank_velfun (i,                           ierr)
cccc      f_insert_velfun       (i,                           ierr)
cccc      f_remove_velfun       (i,                           ierr)
cccc      f_modify_velfun       (i,itype,                     ierr)
cccc      f_raytrace_velfuns    (ichoice,invoke,type2,        ierr)
cccc      f_resample_velfuns    (ichoice,invoke,type2,###,    ierr)
cccc      f_latsample_velfuns   (ichoice,invoke,type2,%%%,    ierr)
cccc      f_lat2sample_velfuns  (        invoke,type2,@@@,    ierr)
cccc      f_velfun_types        (ichoice,type2                    )
cccc      f_velfun_vfids        (ichoice,iwhich,prefix,           )
cccc      f_velfun_vfid         (        iwhich,prefix,i,     ierr)
cccc      f_velfun_headers      (ichoice, which,                  )
cccc      f_misc_actions        (ichoice,iwhich,vwater,btime,bvel, ierr)
cccc      f_multiply_velfuns    (ichoice,type2,constant,ta,tb,xa,xb,  ierr)
cccc      f_delete_velfuns      (ichoice,                     ierr)
cccc      f_maybe_truncate_velfun (i,type2)

cccc      f_reset_bottom_pick (type2,nmax,
cccc                           b_timedepth   ,b_velocity,
cccc                           f_time,f_depth,f_velocity,
cccc                           timetol,depthtol,n,x,v,   nn,xx,vv)

cccc      ### = iwhich,iterpflag,istepflag,iendflag,
cccc            b_timedepth   ,b_velocity,
cccc            f_time,f_depth,f_velocity,
cccc            nwant,ncoef,nrun,xdelta,xdelta2,tdelta,tdelta2,vdelta

cccc      %%% = iwhich,iterpflag,istepflag,iendflag,ybinchoice,
cccc            nwant,ncoef,nrun,xdelta,vdelta

cccc      @@@ = control,idirflag,iterpflag,ichngflag,iendflag,
cccc            ncoef,nrun,vdelta,below,tbelow,dbelow,
cccc            r_xfirst,r_xinc,r_nx,
cccc            r_yfirst,r_yinc,r_ny



      subroutine message_wbox (sender,message,one,two,three,four,five)
c     main switch routine for message to windowboxes.

      integer sender,message,one,two,three,four,five
      include 'vel_boxes.inc'

      if (sender.eq.SENDER_WBOX) return
      if (message.eq.MESSAGE_NEWFILE) then
           call force_updates
      else if (message.eq.MESSAGE_NUMBER) then
           call force_updates
      else if (message.eq.MESSAGE_SORT  ) then
           call force_updates
      else if (message.eq.MESSAGE_MODIFY) then
c          we assume that f_modify_velfun has already been called
c            for each modified function.
           call force_updates
      else if (message.eq.MESSAGE_INSERT) then
c          we assume that f_insert_velfun has already been called.
           if (vfid(one).eq.'blank') call vfid_apply_blank (one)
           call force_updates
      else if (message.eq.MESSAGE_REMOVE) then
c          we assume that f_remove_velfun has already been called.
           call force_updates
      else if (message.eq.MESSAGE_ACTIVE) then
c          we assume that f_activate_velfun has already been called.
cc         call fbox_set_focus (ifunbox,14,ifun)
cc         call fbox_set_focus (ipickbox,21,1)
cc         call fbox_event (ipickbox,'hello')
           call force_updates
      else if (message.eq.MESSAGE_XHEAD ) then
           call force_updates
      else if (message.eq.MESSAGE_YHEAD ) then
           call force_updates
      else if (message.eq.MESSAGE_ORDER ) then
           call force_updates
      end if
      return
      end

 


      subroutine f_autoname_workfile
c     gets non-existing name for automatic-save workfile.
c     this workfile will always be in local directory.

      implicit none
      integer ierr,iexist,i,j,k
      character*80 filename,msg
      character*36 letters
      include 'vel_boxes.inc'
      data letters/'abcdefghijklmnopqrstuvwxyz1234567890'/

      filename=read_filename
      j=0
      do i=1,len(filename)
           if (filename(i:i).eq.']'.or.filename(i:i).eq.'/') j=i
      end do
      if (j.gt.0) filename=read_filename(j+1:)
      if (filename.eq.' ') filename='velocity'
      call addext_replace (filename,'work')
      j=2
      do i=1,len(filename)
           if (filename(i:i).eq.'.') j=i
      end do
      work_filename=filename
      do k=1,36
           call inquire_velfile (work_filename,iexist,   msg,ierr)
           if (iexist.eq.0) return
           work_filename=filename(1:j-1)//letters(k:k)//'.work'
      end do
      return
      end

      


      subroutine f_autosave_workfile_cursor (ibox)
c     saves all data on automatic-save workfile if required.
c     also displays cursor if necessary.

      implicit none
      include 'vel_boxes.inc'
      integer ibox
   
      if (autosave.eq.0.or.changeflag.eq.0) return
      call start_wait_cursor (vel)
      call f_autosave_workfile (ibox)
      call stop_wait_cursor (vel)
      return
      end

 
   

      subroutine f_autosave_workfile (ibox)
c     saves all data on automatic-save workfile if required.

      implicit none
      integer ibox,ierr
      character*80 msg
      include 'vel_boxes.inc'

      if (autosave.eq.0.or.changeflag.eq.0) return
      if (work_filename.eq.' ') call f_autoname_workfile
      msg='saving workfile '//work_filename
      call fbox_messageline (ireadbox,msg)
      call fbox_messageline (isavebox,msg)
      call fbox_messageline (ifunbox,msg)
      call fbox_messageline (ipickbox,msg)
      call fbox_messageline (ibox,msg)
      call save_workfile (work_filename,2,' ',   msg,ierr)
      if (ierr.eq.0) then
           msg='done'
           changeflag=0
      end if
      call fbox_messageline (ireadbox,msg)
      call fbox_messageline (isavebox,msg)
      call fbox_messageline (ifunbox,msg)
      call fbox_messageline (ipickbox,msg)
      call fbox_messageline (ibox,msg)
      return
      end
   
   
   
   

      subroutine f_autodelete_workfile
c     deletes automatic-save workfile.

      implicit none
      integer lun
      include 'vel_boxes.inc'

      call getlun (lun,*999)
      open (lun,file=work_filename,err=999,status='old',
     $                                            form='unformatted')
      close (lun,status='delete')
           print *, 'autodelete ',work_filename
      work_filename=' '
999   return
      end

     

      subroutine f_all_undo_sensitize (ibox)
c     manage the sensitivity of the Undo button in all boxes.
c     the sensitivity is set to True for the box specified by ibox.
c     the sensitivity is set to False in all other boxes.
c     the sensitivity is set to False in all boxes if ibox=0.

      implicit none
      integer ibox
      include 'vel_boxes.inc'

      call res_undo_sensitize (vel,ibox,iresbox)
      call lat_undo_sensitize (vel,ibox,ilatbox)
      call lat2_undo_sensitize (vel,ibox,ilat2box)
      call del_undo_sensitize (vel,ibox,idelbox)
      call mult_undo_sensitize (vel,ibox,imultbox)
      return
      end
   
   
   
      subroutine f_save_tempfile (ibox,   ierr)
c     save or read or close a temporary velocity function file.
c     the logical unit number and box number are saved within this routine.
c     if ibox is not zero, directs messages to that box, and manages
c       the sensitization of the Undo button in that box.

      implicit none
      integer ibox,ierr,lun,iboxkeep,dummy
      character*80 msg
      include 'vel_boxes.inc'
      save lun,iboxkeep
      data lun,iboxkeep/0,0/

      if (lun.ne.0) close (lun)
      call fbox_messageline (ibox,'saving data on temporary file...')
      call save_tempfile (lun,  msg,ierr)
      call fbox_messageline (ibox,msg)                  ! added 9/26/96
      if (ierr.ne.0) call fbox_ring_bell                ! added 9/26/96
      if (lun.ne.0) call f_all_undo_sensitize(ibox)
      if (lun.eq.0) call f_all_undo_sensitize(0)
      iboxkeep=ibox
      return

      entry f_read_tempfile (dummy,ierr)
      if (lun.eq.0) then
           msg='no data available for restoring'
           call fbox_messageline (iboxkeep,msg)
           ierr=1
           return
      end if
      call start_wait_cursor (vel)
      call change_file_size (0,   msg,ierr)
      if (ierr.eq.0) msg='restoring data from temporary file...'
      call fbox_messageline (iboxkeep,msg)
      call fbox_messageline (ifunbox ,msg)
      call read_tempfile (lun,  msg,ierr)
      if (ierr.eq.0) msg='original data restored'
      changeflag=1
      call register_active_velfun
      call f_autosave_workfile (iboxkeep)
      call copy_velfun (ifun,nfunmax+2)
      call fbox_set_focus (ipickbox,21,1)
      call fbox_set_focus (ifunbox,14,ifun)
      call fbox_event (ipickbox,'hello')       ! maybe force_updates is enuf?
      call force_updates
      call adjust_arrow_sensitivity (vel)
      call fbox_messageline (iboxkeep,msg)
      call fbox_messageline (ifunbox ,msg)
      call f_all_undo_sensitize (0)
      call stop_wait_cursor (vel)
      call fbroadcast (vel,SENDER_WBOX,MESSAGE_NUMBER,0,0,0,0,0)
      return

      entry f_close_tempfile
      if (lun.ne.0) then
           close (lun)
           lun=0
           call f_all_undo_sensitize (0)
      end if
      return
      end

      
   
   
   
   

      subroutine f_read_inquire (filename,iexist,   msg,ierr)
c     inquire about a velocity function file to read.

      implicit none
      include 'vel_boxes.inc'
      character*(*) filename,msg
      integer iexist,ierr

      call inquire_velfile (filename,iexist,   msg,ierr)
      if (iexist.eq.1) then
           msg='velocity file found - press OK or Apply to read it'
      end if
      call fbox_messageline (ireadbox,msg)
      return
      end




      subroutine f_save_inquire (filename,iexist,   msg,ierr)
c     inquire about a velocity function file to save.

      implicit none
      include 'vel_boxes.inc'
      character*(*) filename,msg
      integer iexist,ierr
      logical fbox_in_trap

      call inquire_velfile (filename,iexist,   msg,ierr)
      if (fbox_in_trap()) then
           if (ierr.eq.0.and.iexist.eq.1) then
               msg='file already exists - press OK or Apply'//
     $                   ' to overwrite it'
           else if (ierr.eq.0) then
               msg='file does not yet exist - press OK or Apply'//
     $                   ' to save it'
           end if
      end if
      call fbox_messageline (isavebox,msg)
      return
      end




      subroutine get_velstruct_pointer (vel2)
c     get the pointer to VelStruct from the common block and return to c.

      implicit none
      integer vel2
      include 'vel_boxes.inc'

      vel2=vel
      return
      end




      subroutine remove_velocities
c     remove all velocities.
c     called from c function read_vels, which is called by Trey Roby.
c     should not broadcast messages, or display wait cursor, from here.

      implicit none
      include 'vel_boxes.inc'
      integer ierr
      character*80 msg

      call f_autosave_workfile (0)
      call change_file_size (0,   msg,ierr)
      call register_active_velfun
      call copy_velfun (ifun,nfunmax+2)
      call fix_select_codes
      call fbox_set_focus (ifunbox,14,ifun)
      call fbox_set_focus (ipickbox,21,1)
      call adjust_arrow_sensitivity (vel)
      return
      end





      integer function read_velocities (buffer,   muffer)
c     read a velocity function file or workfile.
c     called from c function read_vels, which is called by Trey Roby.
c     replace functions in memory - cannot append functions to memory.
c     should not broadcast messages, or display wait cursor, from here.

      implicit none
      integer buffer(*),muffer(*),ierr
      character*80 filename,msg

      call convert_hh2cc (buffer,0,   filename,0)
      call f_read_velfile (.false.,filename,1,   msg,ierr)
      call convert_cc2hh (msg,0,   muffer,0)
      read_velocities=ierr
      return
      end



      integer function save_velocities (buffer,tbuf,   muffer)
c     save a velocity function file.
c     called from c function save_vels, which is called by Mike Sherrill.
c     saves all functions which are in memory.
c     should not broadcast messages from here.

      implicit none
      integer buffer(*),tbuf(*),muffer(*),ierr
      character*80 filename,msg
      character*8 type2

      call convert_hh2cc (buffer,0,   filename,0)
      call convert_hh2cc (tbuf  ,0,   type2   ,0)
      call f_save_velfile (.false.,filename,2,type2,   msg,ierr)
      call convert_cc2hh (msg,0,   muffer,0)
      save_velocities=ierr
      return
      end



      integer function validate_velocities (buffer,
     $                                      nhx,nhy,nfun,   muffer)
c     validate a velocity function file or workfile.
c     called from c function validate_vels, which is called by Mike Sherrill.
c     does not affect functions in memory.
c     should not broadcast messages, or display wait cursor, from here.

      implicit none
      integer buffer(*),nhx,nhy,nfun,muffer(*),ierr
      character*80 filename,info

      call convert_hh2cc (buffer,0,   filename,0)
      call f_validate_velfile (filename,   nhx,nhy,nfun,info,ierr)
      call convert_cc2hh (info,0,   muffer,0)
      validate_velocities=ierr
      return
      end





      subroutine f_read_velfile (broad,filename,ichoice,   msg,ierr)
c     read a velocity function file or workfile.
c     reads from workfile if extension is .work.
c     reads from cps velfile if extension is anything else or missing.
c     does not broadcast message, or display wait cursor, if broad is false.
c     sets icheck differently depending on whether we are in va.

      implicit none
      include 'vel_boxes.inc'
      logical broad
      character*(*) filename,msg
      character*80 temporary
      integer ichoice,ierr,kount,lun,i,ierr2,icheck

      icheck=am_in_va
      if (broad) call start_wait_cursor (vel)
      if (ichoice.eq.1) call f_autosave_workfile (0)
      temporary=filename
      call addext_replace (temporary,'work')  ! now definitely has work ext.
      if (temporary.eq.filename) then
           msg='reading data from workfile '//filename
           call fbox_messageline (ireadbox,msg)
           call read_workfile (filename,ichoice,icheck,  msg,ierr)
      else
           call open_old_velfile (lun,filename,ichoice,kount,icheck,
     $                                                        msg,ierr)
           if (ierr.ne.0) go to 555
           do i=1,kount
                call vel_immediate (ireadbox,'reading',i,kount,50)
                call read_velfun (lun,   msg,ierr)
                if (ierr.ne.0) go to 444
                if (i.eq.1) ifun=nfun
           end do
           call close_old_velfile (lun,filename,kount,msg,ierr)
      end if
444   if (ichoice.eq.1) then
           xdirwant=0
           ydirwant=0
      end if
      call fsort_functions (vel,ierr2)
      read_filename=filename
      if (ierr.eq.0.and.(ichoice.eq.1.or.kount.eq.nfun)) then
           changeflag=0
           work_filename=' '
c          call f_autoname_workfile
      else
           changeflag=1
           call f_autosave_workfile (0)
      end if
      call register_active_velfun
      call copy_velfun (ifun,nfunmax+2)
      call fix_select_codes
      call fbox_set_focus (ifunbox,14,ifun)
      call fbox_set_focus (ipickbox,21,1)
      call adjust_arrow_sensitivity (vel)
      call fbox_messageline (ireadbox,msg)
      if (broad) then
           call stop_wait_cursor (vel)
           call fbroadcast (vel,SENDER_WBOX,MESSAGE_NEWFILE,0,0,0,0,0)
      end if
      return
555   call fbox_messageline (ireadbox,msg)
      if (broad) call stop_wait_cursor (vel)
      return
      end




      subroutine f_save_velfile
     $                   (stuff,filename,ichoice,type2,   msg,ierr)
c     save a velocity function file or workfile.
c     saves on workfile if extension is .work.
c     saves on cps velfile if extension is anything else or missing.
c     skips some stuff if stuff is false.

      implicit none
      include 'vel_boxes.inc'
      character*(*) filename,type2,msg
      logical stuff
      integer ichoice,ierr,kount,lun,i,j1,j2,kounte,kountf
      character*80 temporary
      logical choose_velfun

      call start_wait_cursor (vel)
      temporary=filename
      call addext_replace (temporary,'work')  ! now definitely has work ext.
      if (temporary.eq.filename) then
           msg='saving data on workfile '//filename
           call fbox_messageline (isavebox,msg)
           call save_workfile (filename,ichoice,type2,   msg,ierr)
      else
           call open_new_velfile (lun,filename,ichoice,kount,kounte,
     $                                                        msg,ierr)
           if (ierr.ne.0) go to 666
           call vfid_apply_blanks
           kountf=0
           do i=1,nfun
                if (choose_velfun(ichoice,i)) then
                     if (n(i).gt.0) then
                       call vel_immediate (isavebox,'saving',i,nfun,50)
                       call save_velfun (lun,i,type2,   msg,ierr)
c                      if (ierr.ne.0) go to 666
                       if (ierr.ne.0) kountf=kountf+1
                     end if
                end if
           end do
           call close_new_velfile (lun,filename,kount,kounte,kountf,
     $                                              type2,   msg,ierr)
      end if
666   call fbox_messageline (isavebox,msg)
      if (stuff) then
           save_filename=filename
           if (ierr.eq.0.and.ichoice.eq.2) then
                changeflag=0
                j1=0
                do i=1,len(save_filename)
                     if (save_filename(i:i).eq.'/'.or.
     $                   save_filename(i:i).eq.']') j1=i
                end do
                j2=0
                do i=1,len(work_filename)-1
                     if (work_filename(i:i).eq.'/'.or.
     $                   work_filename(i:i).eq.']') j2=i
                end do
                if (save_filename(j1+1:).ne.work_filename(j2+1:))
     $                                     call f_autodelete_workfile
           end if
      end if
      call stop_wait_cursor (vel)
      return
      end





      subroutine f_validate_velfile (filename,   nhx,nhy,nfun,info,ierr)
c     validate a velocity function file or workfile.
c     validates a workfile if extension is .work.
c     validates a cps velfile if extension is anything else or missing.

      implicit none
      character*(*) filename,info
      character*80 temporary
      integer nhx,nhy,nfun,ierr

      temporary=filename
      call addext_replace (temporary,'work')  ! now definitely has work ext.
      if (temporary.eq.filename) then
           call validate_workfile (filename,  nhx,nhy,nfun,info,ierr)
      else
           call validate_velfile (filename,  nhx,nhy,nfun,info,ierr)
      end if
      return
      end





      subroutine f_activate_velfun (i)
c     make velocity function i the active one.
c     if i is not in range, this routine does nothing.
c     if i=ifun, this routine still does its thing, because ifun
c                  may have already been reset.
c     also saves workfile if required.

      implicit none
      integer i
      include 'vel_boxes.inc'

      if (i.lt.1.or.i.gt.min(nfun+1,nfunmax)) return
      ifun=i
      call register_active_velfun
      call f_autosave_workfile (0)
      call copy_velfun (ifun,nfunmax+2)
      call adjust_arrow_sensitivity (vel)
      call fbox_set_focus (ifunbox,14,ifun)
      call fbox_set_focus (ipickbox,21,1)
      call fbox_event (ipickbox,'hello')
      if (sifun.gt.0) call fbox_set_focus (iinfobox,11,sifun)
      if (cifun.gt.0) call fbox_set_focus (iinfobox,21,cifun)
ccc the above two lines will work only if this is not called from
ccc a windowbox trap.
      call fbox_set_focus (iinfobox,33, ifun)
      return
      end





      subroutine f_insert_blank_velfun (i,   ierr)
c     insert a blank velocity function into the file at location i.
c     currently called only by Mike Sherrill.
c     new velocity function name = 'blank' - it will subsequently be
c       changed (when getting MESSAGE_INSERT from Mike) to match the
c       coordinates which Mike sets after calling this subroutine.

      implicit none
      integer i,ierr
      character*80 msg
      include 'vel_boxes.inc'

      call insert_velfun (i,   msg,ierr)
      if (ierr.eq.0) then
           n(i)=0
           if (ifun.ge.i) call register_active_velfun
           if (ifun.eq.i) call copy_velfun (ifun,nfunmax+2)
           if (ifun.eq.i) call fbox_set_focus (ipickbox,21,1)
           call adjust_arrow_sensitivity (vel)
      end if
      call fbox_messageline (ifunbox,msg)
      return
      end




      subroutine f_insert_velfun (i,   ierr)
c     insert a velocity function (from buffer) into the file at location i.

      implicit none
      integer i,ierr
      character*80 msg
      include 'vel_boxes.inc'

      call insert_velfun (i,   msg,ierr)
      if (ierr.eq.0) then
           if (vfid(i).eq.'blank') call vfid_apply_blank (i)
           if (ifun.ge.i) call register_active_velfun
           if (ifun.eq.i) call copy_velfun (ifun,nfunmax+2)
           if (ifun.eq.i) call fbox_set_focus (ipickbox,21,1)
           call adjust_arrow_sensitivity (vel)
      end if
      call fbox_messageline (ifunbox,msg)
      return
      end





      subroutine f_remove_velfun (i,   ierr)
c     remove a velocity function from the file (into buffer) at location i.

      implicit none
      integer i,ierr
      character*80 msg
      include 'vel_boxes.inc'

      call remove_velfun (i,   msg,ierr)
      if (ierr.eq.0) then
           if (ifun.ge.i) call register_active_velfun
           if (ifun.eq.i) call copy_velfun (ifun,nfunmax+2)
           if (ifun.eq.i) call fbox_set_focus (ipickbox,21,1)
           call adjust_arrow_sensitivity (vel)
      end if
      call fbox_messageline (ifunbox,msg)
      return
      end



      subroutine f_maybe_truncate_velfun (i,type2)
c     maybe truncate velocity function based upon bottomtime.

      implicit none
      integer i,p
      character*4 type2
      include 'vel_boxes.inc'
  
      if (bottomtime.eq.fnil.or.bottomtime.le.0.0) return
      if (type2(1:2).ne.'VT'.or.n(i).eq.0) return
      call offset_index_words (p,depths,point(i))
      if (times(p+n(i)).eq.fnil              ) return
      if (times(p+n(i)).lt.bottomtime-timetol) return
      if (n(i).ge.2) then
           if (times(p+n(i)-1).eq.fnil      ) return
           if (times(p+n(i)-1).ge.bottomtime) return
      end if
      times(p+n(i))=bottomtime
      return
      end



      subroutine f_modify_velfun (i,itype,   ierr)
c     update a modified velocity function at location i.
c     designed to be called by Mike Sherrill.
c     in this routine, itype starts at 0 rather than 1 (i.e. 0 thru ntypes-1).
c     the picks must have already been modified for the type itype.

      implicit none
      integer i,itype,ierr,invoke,ntypes
      character*4 type2
      character*80 descrip,msg
      include 'vel_boxes.inc'

      invoke=0
      if (raymsg(i).ne.' ') invoke=1
      call velfun_type_ii2cc (itype+1,  type2,descrip,ntypes)
      if (type2.eq.' ') type2='junk'
      call f_maybe_truncate_velfun (i,type2)
      call update_velfun (i,invoke,type2,   msg,ierr)
      if (i.eq.ifun) then
           call fbox_messageline (ipickbox,msg)
           call pick_undo_sensitize (vel,1)
      end if
      return
      end





      subroutine f_raytrace_velfuns (ichoice,invoke,type2,   ierr)
c     raytrace selected velocity functions.

      implicit none
      include 'vel_boxes.inc'
      character*(*) type2
      character*80 msg
      integer ichoice,invoke,ierr,kount,kounte,i,kstart,kstop
      logical choose_velfun

      if (nfun.eq.0) then
           msg='no velocity functions'
           ierr=1
           call fbox_messageline (iraybox,msg)
           return
      end if

      call start_wait_cursor (vel)
      kount=0
      kounte=0
      do i=1,nfun
           if (choose_velfun(ichoice,i)) then
                if (invoke.eq.1)
     $               call vel_immediate (iraybox,'raytracing',i,nfun,5)
                call update_velfun (i,invoke,type2,   msg,ierr)
                kount=kount+1
                if (ierr.ne.0) kounte=kounte+1
                if (kount.eq.1) kstart=i
                kstop=i
                if (i.eq.ifun) call pick_undo_sensitize (vel,1)
           end if
      end do

      if (invoke.eq.1) then
           msg='raytraced'
           if (type2.eq.'VTNM') msg='inverse raytraced'
      else
           msg='reset NMO to RMS'
           if (type2.eq.'VTNM') msg='reset RMS to NMO'
      end if
      write (msg(18:),1000) kount,kounte
1000  format (I5,' functions with',I5,' errors')
      call fbox_messageline (iraybox,msg)
      if (kount.gt.0) changeflag=1
      call f_autosave_workfile (iraybox)
      call stop_wait_cursor (vel)
      ierr=0
      if (kount.eq.0.or.kounte.gt.0) ierr=1
      if (kount.gt.0) call fbroadcast
     $    (vel,SENDER_WBOX,MESSAGE_MODIFY,999,kstart,kstop,1,n(kstart))
      return
      end






      subroutine f_resample_velfuns (ichoice,invoke,type2,
     $         iwhich,iterpflag,istepflag,iendflag,
     $         b_timedepth   ,b_velocity,
     $         f_time,f_depth,f_velocity,
     $         nwant,ncoef,nrun,xdelta,xdelta2,tdelta,tdelta2,
     $         vdelta,   ierr)
c     resample selected velocity functions.

      implicit none
      include 'vel_boxes.inc'
      character*(*) type2
      character*80 msg
      integer ichoice,invoke,ierr,kount,kounte,i,step,nn,ni
      integer iwhich,iterpflag,istepflag,iendflag
      integer b_timedepth   ,b_velocity
      real    f_time,f_depth,f_velocity
      integer nwant,ncoef,nrun,nfunkeep,new,kstart,kstop
      real    xdelta,xdelta2,tdelta,tdelta2,vdelta,xmax
      logical choose_velfun
      real dx,dx2,x(nmax),v(nmax),xx(nmax),vv(nmax),q(nmax,12)

      if (nfun.eq.0) then
           msg='no velocity functions'
           ierr=1
           call fbox_messageline (iresbox,msg)
           return
      else if (type2(1:2).eq.'VL') then
           msg='cannot resample velocities versus layer thickness'
           ierr=1
           call fbox_messageline (iresbox,msg)
           return
      else if (iterpflag.eq.6.and.type2(3:4).ne.'IN') then
           msg='cannot use this option without interval velocities'
           ierr=1
           call fbox_messageline (iresbox,msg)
           return
      else if (iterpflag.eq.7.and.type2.eq.'VTDP'.and.
     $                                   b_timedepth.le.0) then
           msg='you must choose to reset the bottom time and depth'
           ierr=1
           call fbox_messageline (iresbox,msg)
           return
      else if (iterpflag.eq.7.and.b_velocity.le.0.and.
     $                                   b_timedepth.le.0) then
           msg='you must choose to reset the bottom time or depth'
     $                                         //' or velocity'
           ierr=1
           call fbox_messageline (iresbox,msg)
           return
      end if

      kount=0
      xmax=0.
      do i=1,nfun
           if (choose_velfun(ichoice,i)) then
                kount=kount+1
                if (kount.eq.1) kstart=i
                kstop=i
                call get_velfun_picks (i,type2,n(i),x,v,   msg,ierr)
                xmax=max(xmax,x(n(i)))
           end if
      end do
      if (kount.eq.0) then
           msg='no velocity functions selected'
           ierr=1
           call fbox_messageline (iresbox,msg)
           return
      end if

      step=50
      if (invoke.ge.1) step=5

      dx=xdelta
      dx2=xdelta2
      if (type2(1:2).eq.'VT') dx=tdelta
      if (type2(1:2).eq.'VT') dx2=tdelta2

      call start_wait_cursor (vel)
      call f_save_tempfile (iresbox,   ierr)
      if (ierr.ne.0) then
           call stop_wait_cursor (vel)
           return
      end if
      kount=0
      kounte=0
      nfunkeep=nfun
      do i=1,nfunkeep
           if (choose_velfun(ichoice,i)) then
                call vel_immediate (iresbox,'resampling',i,nfun,step)
                call get_velfun_picks (i,type2,n(i),x,v,   msg,ierr)
                if (ierr.ne.0) go to 50
                if (iterpflag.eq.7) then
                     call f_reset_bottom_pick (type2,nmax,
     $                   b_timedepth   ,b_velocity,
     $                   f_time,f_depth,f_velocity,
     $                   timetol,depthtol,n(i),x,v,   nn,xx,vv)
                else
                     ni=n(i)
                     if (x(ni).lt.xmax) then
                          if (ni.lt.nmax) ni=ni+1
                          x(ni)=xmax
                          v(ni)=v(n(i))
                     end if
                     call densify_nonlin (iterpflag,istepflag,iendflag,
     $                   dx,dx2,nwant,vdelta,ncoef,nrun,
     $                   ni,x,v,nn,xx,vv,nmax,q,   msg,ierr)
                         if (ierr.ne.0) go to 50
                end if
                if (iwhich.eq.1) then
                     new=i
                else
                     call alloc_last_velfun (msg,ierr)
                     if (ierr.ne.0) go to 999
                     new=nfun
                     call copy_velfun (i,new)
                     select(new)=' '
                     if (kount.eq.0) select(new)=' stop'
                end if
                call put_velfun_picks (new,type2,nn,xx,vv,   msg,ierr)
                call update_velfun (new,invoke,type2,   msg,ierr)
                if (i.eq.ifun) call pick_undo_sensitize (vel,1)
50              kount=kount+1
                if (ierr.ne.0) kounte=kounte+1
           end if
      end do

      if (invoke.ge.1) then
           msg='resampled/raytraced'
           if (type2.eq.'VTNM') msg='resampled/inverse raytraced'
      else
           msg='resampled'
      end if
      write (msg(29:),1000) kount,kounte
1000  format (I5,' functions with',I5,' errors')
999   call fbox_messageline (iresbox,msg)
      if (kounte.gt.0) call fbox_waste_time (90000)
      if (kounte.gt.0) call fbox_flush_buffer
      changeflag=1
      call f_autosave_workfile (iresbox)
      call stop_wait_cursor (vel)
      if (iwhich.eq.1) then
           call fbroadcast
     $    (vel,SENDER_WBOX,MESSAGE_MODIFY,999,kstart,kstop,1,n(kstart))
      else
           call fbroadcast (vel,SENDER_WBOX,MESSAGE_NUMBER,0,0,0,0,0)
      end if
      return
      end




      subroutine f_reset_bottom_pick (type2,nmax,
     $         b_timedepth   ,b_velocity,
     $         f_time,f_depth,f_velocity,
     $         timetol,depthtol,n,x,v,   nn,xx,vv)
c     reset bottom time and/or depth and/or velocity.

      implicit none
      character*4 type2
      integer     b_v,b_timedepth   ,b_velocity,n,nn,nmax,i
      real    f_x,f_v,f_time,f_depth,f_velocity,x(*),v(*),xx(*),vv(*)
      real    xtol,timetol,depthtol,terp1

      nn=n
      if (n.le.0) return
      do i=1,n
           xx(i)=x(i)
           vv(i)=v(i)
      end do
      if (type2(1:2).eq.'VT') then
           f_x=f_time
           xtol=timetol
      else if (type2(1:2).eq.'VZ') then
           f_x=f_depth
           xtol=depthtol
      else
           return
      end if
      if (type2(3:4).eq.'DP') then
           b_v=b_timedepth
           f_v=f_depth
      else
           b_v=b_velocity
           f_v=f_velocity
      end if
      if (b_timedepth.ge.1) then
           if (xx(nn).lt.f_x-xtol) then
                if (nn.lt.nmax) then
                     nn=nn+1
                     if (b_v.le.0) vv(nn)=vv(nn-1)
                end if
           else if (xx(nn).gt.f_x+xtol) then
                do i=n,1,-1
                     if (xx(i).gt.f_x-xtol) nn=i  ! + changed to -
                end do
                if (b_v.le.0) vv(nn)=terp1(f_x,x,n,v)
           end if
           xx(nn)=f_x
      end if
      if (b_v.ge.1) vv(nn)=f_v
      return
      end

c               else if (iwhich.eq.4) then           ! reset bottom time
c                    if (times(p+n(i)).eq.fnil.or.
c    $                   times(p+n(i)).ge.btime-timetol.or.n(i).eq.nmax)
c    $                                                             then
c                         times(p+n(i))=btime
c                    else
c                         n(i)=n(i)+1
c                         times(p+n(i))=btime
c                         vnmo (p+n(i))=vnmo(p+n(i)-1)
c                    end if
c                    call update_velfun (i,0,'VTNM',   msg,ierr)
c                    if (ierr.ne.0) kounte=kounte+1
c               else if (iwhich.eq.5) then           ! reset bottom vel
c                    vnmo(p+n(i))=bvel
c                    call update_velfun (i,0,'VTNM',   msg,ierr)
c                    if (ierr.ne.0) kounte=kounte+1


      real function get_ybin_center (y)

      implicit none
      include 'vel_boxes.inc'
      real y
      integer ybin_number

      ybin_number=nint((y-ycenter)/ywidth)
      get_ybin_center=ycenter+ybin_number*ywidth
      return
      end



      real function get_xbin_center (x)

      implicit none
      include 'vel_boxes.inc'
      real x
      integer xbin_number

      xbin_number=nint((x-xcenter)/xwidth)
      get_xbin_center=xcenter+xbin_number*xwidth
      return
      end



      subroutine f_latsample_velfuns (ichoice,invoke,type2,
     $         iwhich,iterpflag,istepflag,iendflag,ybinchoice,
     $         nwant,ncoef,nrun,xdelta,vdelta,   ierr)
c     laterally resample selected velocity functions.

      implicit none
      include 'vel_boxes.inc'
      character*(*) type2
      character*80 msg
      integer ichoice,invoke,ierr,kount,kounte,i,step,nn
      integer iwhich,iterpflag,istepflag,iendflag
      integer nwant,ncoef,nrun
      real    xdelta,vdelta,ybinchoice,get_ybin_center
      logical choose_velfun
      real x(nfunmax),v(nfunmax),xx(nfunmax),vv(nfunmax),q(nfunmax,12)
      real xfirst(nmax),xtemp,vtemp,ybin1,ybin2,tolerance
      integer ifirst,ilast,j,nfunkeep,nkeep,n2

      if (nfun.eq.0) then
           msg='no velocity functions'
           go to 999
      else if (type2(1:2).eq.'VL') then
           msg='cannot resample velocities versus layer thickness'
           go to 999
      end if

      ybin2=get_ybin_center(ybinchoice)
      tolerance=0.001
      if (type2(1:2).eq.'VZ') tolerance = 10.
      kount=0
      do i=1,nfun
           ybin1=get_ybin_center(ybin(i))
           if (((ichoice.le.2.and.choose_velfun(ichoice,i)).or.
     $         (ichoice.eq.3.and..not.choose_velfun(1,i))).and.
     $        (ybin1.eq.ybin2.or.ybinchoice.eq.fnil)) then
               kount=kount+1
               if (kount.eq.1) then
                    ifirst=i
                    if (ybinchoice.eq.fnil) ybin2=ybin1
               else if (ybin1.ne.ybin2) then
                    msg='velocity functions not all at same YBIN'
                    go to 999
               else if (xbin(i).le.xbin(ilast)) then
                    msg='velocity functions not at increasing XBIN'
                    go to 999
               else if (n(i).ne.n(ilast)) then
                    msg='all velocity functions must have same'//
     $                                    ' number of picks'
                    go to 999
               else if (errmsg(i).ne.' ') then
                    msg='one or more velocity functions have'//
     $                                    ' pre-existing errors'
                    go to 999
               end if
               do j=1,n(i)
                    call get_velfun_pick (i,type2,j,xtemp,vtemp,
     $                                                        msg,ierr)
                    if (kount.eq.1) xfirst(j)=xtemp
                    if (abs(xtemp-xfirst(j)).ge.tolerance) then
                         if (type2(1:2).eq.'VZ') then
                           msg='velocity functions do not have'//
     $                                    ' same depth values'
                         else
                           msg='velocity functions do not have'//
     $                                    ' same time values'
                         end if
                         go to 999
                    end if
               end do
               x(kount)=xbin(i)
               ilast=i
           end if
      end do

      if (kount.lt.2) then
           msg='you need at least two available velocity functions'
           go to 999
      end if

      call start_wait_cursor (vel)
      nkeep=0                          ! throw away all the old functions.
      if (iwhich.eq.2) nkeep=nfun      ! keep all the old functions.
      nfunkeep=nfun
      kounte=0
      do j=1,n(ifirst)
           kount=0
           do i=1,nfunkeep
                ybin1=get_ybin_center(ybin(i))
                if (((ichoice.le.2.and.choose_velfun(ichoice,i)).or.
     $              (ichoice.eq.3.and..not.choose_velfun(1,i))).and.
     $              (ybin1.eq.ybin2)) then
                     kount=kount+1
                     call get_velfun_pick (i,type2,j,xtemp,v(kount),
     $                                                       msg,ierr)
                end if
           end do
           call densify (iterpflag,istepflag,iendflag,
     $              xdelta,nwant,vdelta,ncoef,nrun,
     $              kount,x,v,nn,xx,vv,nfunmax-nkeep,q,  msg,ierr)
           if (j.eq.1) then
                if (ierr.ne.0) go to 888
                call f_save_tempfile (ilatbox,   ierr)
                if (ierr.ne.0) then
                     call stop_wait_cursor (vel)
                     return
                end if
                call change_file_size (max(nfunkeep,nkeep+nn),msg,ierr)
                if (ierr.ne.0) call f_close_tempfile
                if (ierr.ne.0) go to 888
                msg='doing the resampling...'
                call fbox_messageline (ilatbox,msg)
                call fbox_messageline (ifunbox,msg)
           end if
           if (ierr.ne.0) kounte=kounte+1
           do i=1,nn
                call put_velfun_pick (nkeep+i,type2,j,
     $                                   xfirst(j),vv(i),   msg,ierr)
           end do
      end do

      n2=n(ifirst)
      step=50
      if (invoke.ge.1) step=5
      do i=1,nn
           call vel_immediate (ilatbox,'resampling',i,nn,step)
           call clear_velfun (nkeep+i)
           xbin  (nkeep+i)=xx(i)
           ybin  (nkeep+i)=ybin2
           n     (nkeep+i)=n2
           vfid  (nkeep+i)='blank'
           select(nkeep+i)=' '
           type  (nkeep+i)=type2
           call vfid_apply_blank (nkeep+i)
           call update_velfun (nkeep+i,invoke,type2,  msg,ierr)
           if (nkeep+i.eq.ifun) call pick_undo_sensitize (vel,1)
      end do
      if (iwhich.eq.2) select(nkeep+1)=' stop'
      call change_file_size (nkeep+nn,msg,ierr)
           
      if (invoke.ge.1) then
           msg='resampled/raytraced'
           if (type2.eq.'VTNM') msg='resampled/inverse raytraced'
      else
           msg='resampled'
      end if
      write (msg(29:),1000) nn,kounte
1000  format (I5,' functions laterally with',I4,' picks with errors')
      call fbox_messageline (ilatbox,msg)
      call fbox_messageline (ifunbox,msg)
      changeflag=1
      call f_autosave_workfile (ilatbox)
      call stop_wait_cursor (vel)
      call fbroadcast (vel,SENDER_WBOX,MESSAGE_NUMBER,0,0,0,0,0)
      return
888   changeflag=1
      call f_autosave_workfile (ilatbox)
      call stop_wait_cursor (vel)
      call fbroadcast (vel,SENDER_WBOX,MESSAGE_NUMBER,0,0,0,0,0)
999   ierr=1
      call fbox_messageline (ilatbox,msg)
      call fbox_messageline (ifunbox,msg)
      return
      end





      subroutine f_lat2sample_velfuns (invoke,type2,
     $         control,idirflag,iterpflag,ichngflag,iendflag,
     $         ncoef,nrun,vdelta,below,tbelow,dbelow,
     $         r_xfirst,r_xinc,r_nx,
     $         r_yfirst,r_yinc,r_ny,  ierr)
c     laterally resample all velocity functions.
c     control=-1 means this is the first of two calls.
c     control= 0 means this is the only call.
c     control= 1 means this is the second of two calls.
c     idirflag=1 means resample in X direction.
c     idirflag=2 means resample in Y direction.

      implicit none
      include 'vel_boxes.inc'
      character*(*) type2
      character*80 msg
      integer invoke,ierr,kount,kounte,i,step,nn,ierr2
      integer control,idirflag,iterpflag,ichngflag,iendflag
      integer nwant,ncoef,nrun,r_nx,r_ny,numlines,below
      real    tbelow,dbelow,tdbelow
      real    vdelta,xdelta,get_xbin_center,get_ybin_center
      real    r_xfirst,r_xinc
      real    r_yfirst,r_yinc
c     real x(nfunmax),v(nfunmax),xx(nfunmax),vv(nfunmax),q(nfunmax,12)
      real                       xx(nfunmax),vv(nfunmax),q(nfunmax,12)
      real timedepth(nmax),xtemp,vtemp,tolerance
      real velocities(nfunmax,nmax),sum
      integer j,nfunkeep,istepflag,i2,k,npicks
      real    primary_bin     (nfunmax),secondary_bin(nfunmax)
      integer original_funcnum(nfunmax),first_of_line(nfunmax)
      integer istart,istop,k2

c----------check for obvious errors.
c----------also get timedepth(npicks).

      if (nfun.eq.0) then
           msg='no velocity functions'
           go to 777
      else if (type2(1:2).eq.'VL') then
           msg='cannot resample velocities versus layer thickness'
           go to 777
      else if (r_xinc.eq.0.0.or.r_yinc.eq.0.0.or.
     $         r_nx  .le.0  .or.r_ny  .le.0  ) then
           msg='error in requested resampling parameters'
           go to 777
      end if
      if (ichngflag.eq.1.and.idirflag.eq.1) then
           if (r_xinc.lt.xwidth) then
               msg='requested resampling yields'//
     $               ' more than one function at same X bin'
               go to 777
           end if
      else if (ichngflag.eq.1.and.idirflag.eq.2) then
           if (r_yinc.lt.ywidth) then
               msg='requested resampling yields'//
     $               ' more than one function at same Y bin'
               go to 777
           end if
      end if

      tolerance=0.001
      if (type2(1:2).eq.'VZ') tolerance=10.
      npicks=n(1)

      do i=1,nfun
          if (n(i).ne.npicks) then
               msg='all velocity functions must have same'//
     $                                    ' number of picks'
               go to 777
c         else if (errmsg(i).ne.' ') then
c              msg='one or more velocity functions have'//
c    $                                    ' pre-existing errors'
c              go to 777
          end if
          do j=1,npicks
               call get_velfun_pick (i,type2,j,xtemp,vtemp,  msg,ierr)
               if (ierr.ne.0) then
                    msg='one or more velocity functions have'//
     $                                    ' pre-existing errors'
                    go to 777
               end if
               if (i.eq.1) timedepth(j)=xtemp
               if (abs(xtemp-timedepth(j)).ge.tolerance) then
                    if (type2(1:2).eq.'VZ') then
                         msg='velocity functions do not have'//
     $                                       ' same depth values'
                    else
                         msg='velocity functions do not have'//
     $                                       ' same time values'
                    end if
                    go to 777
               end if
          end do
      end do

      tdbelow=timedepth(1)-999.
      if (ichngflag.ne.1.and.below.ne.0) then
           if (type2(1:2).eq.'VZ') then
                tdbelow=dbelow
           else if (type2(1:2).eq.'VT')then
                tdbelow=tbelow
           end if
      end if
      if (tdbelow.gt.timedepth(npicks)) then
           msg='smoothing would start below bottom time/depth'
           go to 777
      end if

c            print *, 'npicks = ',npicks
c            print *, 'timedepth(npicks) = ',(timedepth(j),j=1,npicks)

c----------get bin locations and number of lines.

cc primary_bin     (nfun)     for primary   sort xbin or ybin array.
cc secondary_bin   (nfun)     for secondary sort xbin or ybin array.
cc original_funcnum(nfun)     for original velocity function number.
cc first_of_line   (numlines) for first function of each line.

      do i=1,nfun
           if (idirflag.eq.1) then
                primary_bin  (i)=get_ybin_center(ybin(i))
                secondary_bin(i)=get_xbin_center(xbin(i))
           else
                primary_bin  (i)=get_xbin_center(xbin(i))
                secondary_bin(i)=get_ybin_center(ybin(i))
           end if
           original_funcnum(i)=i
      end do
      call triplesort (primary_bin,secondary_bin,original_funcnum,nfun)

      numlines=1
      first_of_line(1)=1
      do i=2,nfun
           if (primary_bin(i).ne.primary_bin(i-1)) then
                numlines=numlines+1
                first_of_line(numlines)=i
           else if (secondary_bin(i).eq.secondary_bin(i-1)) then
                msg='you have two or more velocity functions'//
     $                      ' at same (X,Y) bin'
                go to 777
           end if
      end do

c     print*,'numlines = ',numlines
c     print*,'nfun = ',nfun
c     print*,'primary_bin(nfun)= ',(primary_bin(i),i=1,nfun)
c     print*,'secondary_bin(nfun)= ',(secondary_bin(i),i=1,nfun)
c     print*,'original_funcnum(nfun)= ',(original_funcnum(i),i=1,nfun)
c     print*,'first_of_line(numlines)= ',(first_of_line(i),i=1,numlines)

c----------convert primary_bin from bin center to average bin.
c----------convert secondary_bin from bin center to actual bin.

      do k=1,numlines
          istart=first_of_line(k)
          if (k.lt.numlines) then
              istop=first_of_line(k+1)-1
          else
              istop=nfun
          end if
          kount=istop-istart+1
          sum=0.0
          do i=istart,istop
              i2=original_funcnum(i)
              if (idirflag.eq.1) then
                  sum             =sum+ybin(i2)
                  secondary_bin(i)=    xbin(i2)
              else
                  sum             =sum+xbin(i2)
                  secondary_bin(i)=    ybin(i2)
              end if
          end do
          sum=sum/kount
          do i=istart,istop
              primary_bin(i)=sum
          end do
      end do

c     print*,'primary_bin(nfun)= ',(primary_bin(i),i=1,nfun)
c     print*,'secondary_bin(nfun)= ',(secondary_bin(i),i=1,nfun)

c----------get and check number of requested functions per line.

      if (ichngflag.eq.1) then
           if (idirflag.eq.1) then
                nn=r_nx
           else
                nn=r_ny
           end if
           if (numlines*nn.gt.nfunmax) then
               write (msg,4000) numlines*nn,nfunmax
4000           format ('requested resampling yields',I6,
     $           ' functions - maximum allowed is',I6)
               go to 777
           end if
c                  print*,'nn = ',nn
      end if

c----------get all of the velocities.

      do i=1,nfun
           i2=original_funcnum(i)
           do j=1,npicks
               call get_velfun_pick (i2,type2,j,xtemp,vtemp,  msg,ierr)
               velocities(i,j)=vtemp
           end do
c          print*,'velocities(i,npicks)=',(velocities(i,j),j=1,npicks)
      end do

c----------get parameters for resampling.

      if (ichngflag.eq.1) then   !  create uniform X or Y increments.
          istepflag=4
          do i=1,nn
               if (idirflag.eq.1) then
                    xx(i)=r_xfirst+(i-1)*r_xinc
               else
                    xx(i)=r_yfirst+(i-1)*r_yinc
               end if
          end do
c           print*,'xx(nn) = ',(xx(i),i=1,nn)
      else                       !  retain current X or Y locations.
          istepflag=3
      end if
      xdelta=1.0                 !  dummy
      nwant=1                    !  dummy

c----------prepare to do the resampling.

      if (control.le.0) then
           call start_wait_cursor (vel)
           call f_save_tempfile (ilat2box,   ierr)
           if (ierr.ne.0) then
                call stop_wait_cursor (vel)
                return
           end if
      end if
      if (idirflag.eq.1) msg='doing the resampling in X direction...'
      if (idirflag.eq.2) msg='doing the resampling in Y direction...'
      call fbox_messageline (ilat2box,msg)
      call fbox_messageline (ifunbox,msg)

c----------create new file if changing bin locations.
c----------also save old nfun.

      nfunkeep=nfun
      if (ichngflag.eq.1) then
           call change_file_size (numlines*nn,msg,ierr)
           if (ierr.ne.0) go to 999
           do k=1,numlines
                k2=first_of_line(k)
                do i=1,nn
                     i2=(k-1)*nn+i
                     call clear_velfun (i2)
                     if (idirflag.eq.1) then
                          xbin(i2)=xx(i)
                          ybin(i2)=primary_bin(k2)
                     else
                          xbin(i2)=primary_bin(k2)
                          ybin(i2)=xx(i)
                     end if
                     n     (i2)=npicks
                     vfid  (i2)='blank'
                     select(i2)=' '
                     type  (i2)=type2
                     call vfid_apply_blank (i2)
                end do
           end do
           ifun=1
      end if
c               print*,'new nfun = ',nfun

c----------begin loop, resampling each line.

      do k=1,numlines
        istart=first_of_line(k)
        if (k.lt.numlines) then
           istop=first_of_line(k+1)-1
        else
           istop=nfunkeep
        end if
        kount=istop-istart+1
        do j=1,npicks

c         print*,' '
c         print*,'before pick ',j,'   nn = ',nn,'   kount = ',kount
c         print*,'secondary_bin(kount) = ',(secondary_bin(istart+i-1),
c    $                                      i=1,kount)
c         print*,'velocities(kount,j) = ',(velocities(istart+i-1,j),
c    $                                      i=1,kount)

             if (tdbelow.le.timedepth(j)) then
             call densify (iterpflag,istepflag,iendflag,
     $              xdelta,nwant,vdelta,ncoef,nrun,
     $              kount,secondary_bin(istart),velocities(istart,j),
     $              nn,xx,vv,nfunmax,q,  msg,ierr)
             else
ccccccc this has iterpflag=1 to make it do-nothing:
             call densify (1,istepflag,iendflag,
     $              xdelta,nwant,vdelta,ncoef,nrun,
     $              kount,secondary_bin(istart),velocities(istart,j),
     $              nn,xx,vv,nfunmax,q,  msg,ierr)
             end if
             if (ierr.ne.0) go to 999

c         print*,'after  pick ',j,'   nn = ',nn,'   kount = ',kount
c         print*,'xx(nn) = ',(xx(i),i=1,nn)
c         print*,'vv(nn) = ',(vv(i),i=1,nn)

c----------place resampled velocity into correct location.

             do i=1,nn
                if (ichngflag.eq.1) then
                     i2=(k-1)*nn+i
                else
                     i2=original_funcnum(istart+i-1)
                end if
                call put_velfun_pick (i2,type2,j,
     $                 timedepth(j),vv(i),   msg,ierr)
             end do

c----------end loop, resampling each line.

        end do
      end do

c----------update all velocity functions.

      call fsort_functions (vel,ierr2)
      kounte=0
      step=50
      if (invoke.ge.1) step=5
      do i=1,nfun
           call vel_immediate (ilat2box,'updating',i,nfun,step)
           call update_velfun (i,invoke,type2,  msg,ierr)
           if (ierr.ne.0) kounte=kounte+1
      end do

c----------successful finish.

      if (invoke.ge.1) then
           msg='resampled/raytraced'
           if (type2.eq.'VTNM') msg='resampled/inverse raytraced'
      else
           msg='resampled'
      end if
      write (msg(29:),1000) nfun,kounte
1000  format (I5,' functions laterally with',I4,' errors')
      if (kounte.gt.0) go to 999
      ierr=0
      go to 888

c----------an error occurred after changes were made.

999   ierr=1
      go to 888

c----------an error occurred before changes were made.

777   ierr=1
      if (control.gt.0) go to 888
      call fbox_messageline (ilat2box,msg)
      call fbox_messageline (ifunbox,msg)
      return

c----------finish up after changes were made.

888   changeflag=1
      if (control.ge.0.or.ierr.eq.1) call f_autosave_workfile (ilat2box)
      call fbox_messageline (ilat2box,msg)
      call fbox_messageline (ifunbox,msg)
      if (control.ge.0.or.ierr.eq.1) call stop_wait_cursor (vel)
      if (control.ge.0.or.ierr.eq.1)
     $     call fbroadcast (vel,SENDER_WBOX,MESSAGE_NUMBER,0,0,0,0,0)
      return
      end



cccccccccc not needed:

      subroutine switch_and_sort (idirflag,   msg,ierr)
c     switches xbin and ybin if idirflag is 2.
c     then sorts whether or not switching was performed.

      implicit none
      include 'vel_boxes.inc'
      character*80 msg
      integer idirflag,ierr,i
      real temporary

      if (idirflag.eq.2) then
           do i=1,nfun
                temporary=xbin(i)
                xbin(i)=ybin(i)
                ybin(i)=temporary
           end do
      end if
      msg=' '
      call fsort_functions (vel,   ierr)
      if (ierr.ne.0) msg='error while sorting velocity functions'
      return
      end




      subroutine f_velfun_types (ichoice,type2)
c     reset selected velocity function types.

      implicit none
      include 'vel_boxes.inc'
      character*(*) type2
      character*80 msg
      integer ichoice,kount,i
      logical choose_velfun

      if (ichoice.eq.4) then
           type(nfunmax+3)=type2
           msg='default type reset to type '//type2
           call fbox_messageline (isetbox,msg)
           return
      else if (nfun.eq.0) then
           msg='no velocity functions'
           call fbox_messageline (isetbox,msg)
           return
      end if
      kount=0
      do i=1,nfun
           if (choose_velfun(ichoice,i)) then
                   call set_velfun_type (i,type2)
                   kount=kount+1
                   if (i.eq.ifun) call pick_undo_sensitize (vel,1)
           end if
      end do
      msg=' '
      write (msg,1000) kount,type2
1000  format (I5,' functions reset to type ',A4)
      call fbox_messageline (isetbox,msg)
      if (kount.gt.0) changeflag=1
      call f_autosave_workfile_cursor (isetbox)
      return
      end







      subroutine f_velfun_vfids (ichoice,iwhich,prefix)
c     reset selected velocity function names.

      implicit none
      include 'vel_boxes.inc'
      character*(*) prefix
      character*80 msg
      integer ichoice,iwhich,kount,i,kounte
      logical choose_velfun

      if (nfun.eq.0) then
           msg='no velocity functions'
           call fbox_messageline (ivfidbox,msg)
           return
      end if
      kount=0
      kounte=0
      do i=1,nfun
           if ((ichoice.le.3.and.choose_velfun(ichoice,i)).or.
     $         (ichoice.eq.4.and.(vfid(i).eq.' '.or.
     $                            vfid(i).eq.'blank'))) then
                call f_velfun_vfid (iwhich,prefix,i)
                if (vfid(i).eq.'whoops') then
                     kounte=kounte+1
                else
                     kount=kount+1
                end if
                if (i.eq.ifun) call pick_undo_sensitize (vel,1)
           end if
      end do
      msg=' '
      write (msg,1000) kount,kounte
1000  format (I5,' function names reset with ',I5,' errors')
      call fbox_messageline (ivfidbox,msg)
      if (kount.gt.0) changeflag=1
      call f_autosave_workfile_cursor (ivfidbox)
      return
      end



      subroutine f_velfun_vfid (iwhich,prefix,i)
c     reset a single velocity function name.

      implicit none
      include 'vel_boxes.inc'
      character*(*) prefix
c     integer ichoice,iwhich,i,j
      integer iwhich,i,j

      if (iwhich.eq.1) then
                     write (vfid(i),100,err=999) prefix,i
      else if (iwhich.eq.2) then
                     write (vfid(i),200,err=999) prefix,nint(xbin(i))
      else
                     write (vfid(i),300,err=999) nint(ybin(i)),
     $                                           nint(xbin(i))
      end if
      do j=1,8
           if (vfid(i)(j:j).eq.'*') go to 999
           if (vfid(i)(j:j).eq.' ') vfid(i)(j:j)='-'
      end do
      return
999   vfid(i)='whoops'
      return
100   format (A3,I5)
200   format (A3,I5)
300   format (I4,I4)
      end



      subroutine f_velfun_headers (ichoice,which)
c     reset selected velocity function headers.

      implicit none
      include 'vel_boxes.inc'
      character*80 msg
      integer ichoice,which(*),kount,i
      logical choose_velfun

      if (nfun.eq.0) then
           msg='no velocity functions'
           call fbox_messageline (iheadbox,msg)
           return
      end if
      kount=0
      do i=1,nfun
           if (choose_velfun(ichoice,i)) then
                if (which(1).ne.0) project(i)=project(nfunmax+3)
                if (which(2).ne.0) line   (i)=line   (nfunmax+3)
                if (which(3).ne.0) rdate  (i)=rdate  (nfunmax+3)
                if (which(4).ne.0) pdate  (i)=pdate  (nfunmax+3)
                if (which(5).ne.0) userid (i)=userid (nfunmax+3)
                if (which(6).ne.0) comment(i)=comment(nfunmax+3)
                if (which(7).ne.0) comment(i)=vfid(i)
                if (i.eq.ifun) call pick_undo_sensitize (vel,1)
                kount=kount+1
           end if
      end do
      msg=' '
      write (msg,1000) kount
1000  format (I5,' velocity function headers reset')
      call fbox_messageline (iheadbox,msg)
      if (kount.gt.0) changeflag=1
      call f_autosave_workfile_cursor (iheadbox)
      return
      end





      subroutine f_misc_actions (ichoice,iwhich,vwater,btime,bvel, ierr)
c     do miscellaneous actions on velocity functions.

      implicit none
      include 'vel_boxes.inc'
      character*80 msg
      integer ichoice,iwhich,ierr,kount,kounte,i,j,p,kstart,kstop
      logical choose_velfun
      real vwater,btime,bvel,factor

      if (nfun.eq.0) then
           msg='no velocity functions'
           call fbox_messageline (imiscbox,msg)
           ierr=1
           return
      else if (iwhich.eq.1) then
           factor=1./3.28084
      else if (iwhich.eq.2) then
           factor=3.28084
      end if
      kount=0
      kounte=0
      do i=1,nfun
           if (choose_velfun(ichoice,i).and.n(i).gt.0) then
                call offset_index_words (p,depths,point(i))
                if (iwhich.eq.1.or.iwhich.eq.2) then  ! feet and meters
                     do j=1,n(i)
                if (vint  (p+j).ne.fnil) vint  (p+j)=factor*vint  (p+j)
                if (vrms  (p+j).ne.fnil) vrms  (p+j)=factor*vrms  (p+j)
                if (vnmo  (p+j).ne.fnil) vnmo  (p+j)=factor*vnmo  (p+j)
                if (vav   (p+j).ne.fnil) vav   (p+j)=factor*vav   (p+j)
                if (depths(p+j).ne.fnil) depths(p+j)=factor*depths(p+j)
                if (thick (p+j).ne.fnil) thick (p+j)=factor*thick (p+j)
                     end do
                else if (iwhich.eq.3.and.errmsg(i).ne.' ') then
                     kounte=kounte+1
                else if (iwhich.eq.3) then         ! remove water velocity
                     do j=1,n(i)
                          vrms(p+j)=max(vrms(p+j),vwater+10.)
                          vrms(p+j)=sqrt(vrms(p+j)**2-vwater**2)
                     end do
                     call update_velfun (i,0,'VTRM',   msg,ierr)
                     if (ierr.ne.0) kounte=kounte+1
c               else if (iwhich.eq.4) then           ! reset bottom time
c                    if (times(p+n(i)).eq.fnil.or.
c    $                   times(p+n(i)).ge.btime-timetol.or.n(i).eq.nmax)
c    $                                                             then
c                         times(p+n(i))=btime
c                    else
c                         n(i)=n(i)+1
c                         times(p+n(i))=btime
c                         vnmo (p+n(i))=vnmo(p+n(i)-1)
c                    end if
c                    call update_velfun (i,0,'VTNM',   msg,ierr)
c                    if (ierr.ne.0) kounte=kounte+1
c               else if (iwhich.eq.5) then           ! reset bottom vel
c                    vnmo(p+n(i))=bvel
c                    call update_velfun (i,0,'VTNM',   msg,ierr)
c                    if (ierr.ne.0) kounte=kounte+1
                end if
                if (i.eq.ifun) call pick_undo_sensitize (vel,1)
                kount=kount+1
                if (kount.eq.1) kstart=i
                kstop=i
           end if
      end do
      msg=' '
      write (msg,1000) kount,kounte
1000  format (I5,' velocity functions changed with',I5,' errors')
      call fbox_messageline (imiscbox,msg)
      if (kount.gt.0) changeflag=1
      call f_autosave_workfile_cursor (imiscbox)
      ierr=0
      if (kounte.gt.0) ierr=1
      if (kount.gt.0) call fbroadcast
     $    (vel,SENDER_WBOX,MESSAGE_MODIFY,999,kstart,kstop,1,n(kstart))
      return
      end





      subroutine f_delete_velfuns (ichoice,   ierr)
c     delete selected velocity functions.

      implicit none
      include 'vel_boxes.inc'
      integer ichoice,ierr
      character*80 msg
      logical deleted

      call f_save_tempfile (idelbox,   ierr)
      if (ierr.ne.0) return
      call delete_velfuns (ichoice,   deleted,msg,ierr)
      if (ierr.eq.0) then
           call register_active_velfun
           if (deleted) call copy_velfun (ifun,nfunmax+2)
           if (deleted) call fbox_set_focus (ipickbox,21,1)
           call fbox_set_focus (ifunbox,14,ifun)
           call adjust_arrow_sensitivity (vel)
      end if
      call fbox_messageline (idelbox,msg)
      call fbox_messageline (ifunbox,msg)
      call f_autosave_workfile_cursor (idelbox)
      if (ierr.eq.0)
     $      call fbroadcast (vel,SENDER_WBOX,MESSAGE_NUMBER,0,0,0,0,0)
      return
      end

      integer function read_ref_velocities (buffer, muffer)
c     read a velocity function file for the reference, display only
c     ehs --- 12jun96

      implicit none
      include 'vel_boxes.inc'
      integer buffer(*), muffer(*)
      character*80 filename, msg
      integer ierr, kount, lun, i, icheck

      icheck=am_in_va

      call convert_hh2cc (buffer, 0, filename, 0)

      call open_old_velfile (lun, filename, 1, kount, icheck, msg, ierr)
      if (ierr .eq. 0) then
           do i=1, kount
                call read_velfun (lun, msg, ierr)
                if (ierr .ne. 0) go to 999
           end do
           call close_old_velfile (lun, filename, kount, msg, ierr)
      end if

999   call convert_cc2hh (msg, 0, muffer, 0)
      read_ref_velocities = ierr
      return
      end




      subroutine f_multiply_velfuns (ichoice,type2,constant,ta,tb,xa,xb,
     $                                                        ierr)
c     multiply selected velocity functions by a constant.
c     ta and tb are top and bottom time window to operate on.
c     xa and xb are top and bottom depth window to operate on.

      implicit none
      include 'vel_boxes.inc'
      character*(*) type2
      real constant,ta,tb,xa,xb,xxa,xxb,x,v
      character*80 msg
      integer ichoice,ierr,kount,kounte,i,kstart,kstop,j
      logical choose_velfun

      if (nfun.eq.0) then
           msg='no velocity functions'
           ierr=1
           call fbox_messageline (imultbox,msg)
           return
      end if
      if (type2(1:2).eq.'VT') then
           if (tb.lt.ta) then
                msg='maximum time is less than minimum time'
                ierr=1
                call fbox_messageline (imultbox,msg)
                return
           else if (ta.lt.0.0) then
                msg='minimum time is negative'
                ierr=1
                call fbox_messageline (imultbox,msg)
                return
           end if
           xxa=ta
           xxb=tb
      else if (type2(1:2).eq.'VZ') then
           if (xb.lt.xa) then
                msg='maximum depth is less than minimum depth'
                ierr=1
                call fbox_messageline (imultbox,msg)
                return
           else if (xa.lt.0.0) then
                msg='minimum depth is negative'
                ierr=1
                call fbox_messageline (imultbox,msg)
                return
           end if
           xxa=xa
           xxb=xb
      else
           msg='invalid velocity function type'
           ierr=1
           call fbox_messageline (imultbox,msg)
           return
      end if

      call f_save_tempfile (imultbox,   ierr)
      if (ierr.ne.0) return
      call start_wait_cursor (vel)
      kount=0
      kounte=0
      do i=1,nfun
           if (choose_velfun(ichoice,i).and.n(i).gt.0) then
                do j=1,n(i)
                     v=0.0
                     x=-999999.0
                     call get_velfun_pick (i,type2,j,x,v,   msg,ierr)
                     if (ierr.eq.0.and.x.ge.xxa.and.x.le.xxb) then
                       v=constant*v
                       call put_velfun_pick (i,type2,j,x,v,   msg,ierr)
                     end if
                end do
                call update_velfun (i,0,type2,   msg,ierr)
                kount=kount+1
                if (ierr.ne.0) kounte=kounte+1
                if (kount.eq.1) kstart=i
                kstop=i
                if (i.eq.ifun) call pick_undo_sensitize (vel,1)
           end if
      end do

      write (msg,1000) kount,kounte
1000  format ('operated on ',I5,' functions with',I5,' errors')
      call fbox_messageline (imultbox,msg)
      if (kount.gt.0) changeflag=1
      call f_autosave_workfile (imultbox)
      call stop_wait_cursor (vel)
      ierr=0
      if (kount.eq.0.or.kounte.gt.0) ierr=1
      if (kount.gt.0) call fbroadcast
     $    (vel,SENDER_WBOX,MESSAGE_MODIFY,999,kstart,kstop,1,n(kstart))
      return
      end

