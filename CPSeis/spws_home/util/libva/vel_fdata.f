
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


ccccccccccccc   vel_fdata.f   ccccccccccccccccc


ccc    start_vel_data    (vdpoint)
ccc    change_file_size  (nfunwant,   msg,ierr)
ccc    get_select_counts (kount)
ccc    get_errmsg_counts (kount)
ccc    get_raymsg_counts (kount)

ccc    clear_velfun        (i)
ccc    move_velfun         (i,j)
ccc    copy_velfun         (i,j)
ccc    insert_velfun       (i,                 msg,ierr)
ccc    remove_velfun       (i,                 msg,ierr)
ccc    delete_velfuns      (ichoice,   deleted,msg,ierr)
ccc    alloc_last_velfun   (                   msg,ierr)
ccc    dealloc_last_velfun (                   msg,ierr)
ccc    verify_xbin         (i,                     ierr)
ccc    verify_ybin         (i,                     ierr)

ccc    adjust_select_codes (i)
ccc    fix_select_codes
ccc    delete_select_codes
ccc    choose_velfun       (ichoice,i)           <-- logical function

ccc    put_velfun_pick  (i,       type2, j,x,v,       msg,ierr)
ccc    get_velfun_pick  (i,       type2,        j,x,v,msg,ierr)
ccc    put_velfun_picks (i,       type2,n2,x,v,       msg,ierr)
ccc    get_velfun_picks (i,       type2,       n2,x,v,msg,ierr)
ccc    update_velfun    (i,invoke,type2,              msg,ierr)

ccc    inquire_velfile   (    filename,    iexist,                msg,ierr)
ccc    open_old_velfile  (lun,filename,ichoice,kount,icheck,      msg,ierr)
ccc    open_new_velfile  (lun,filename,ichoice,kount,kounte,      msg,ierr)
ccc    read_velfun       (lun,                                    msg,ierr)
ccc    save_velfun       (lun,   i,type2,                         msg,ierr)
ccc    close_old_velfile (lun,filename,        kount,             msg,ierr)
ccc    close_new_velfile (lun,filename, kount,kounte,kountf,type2,msg,ierr)

ccc    validate_velfile  (filename,    nhx,nhy,nfun,info,ierr)
ccc    validate_workfile (filename,    nhx,nhy,nfun,info,ierr)
ccc    save_workfile     (filename,ichoice,type2,    msg,ierr)
ccc    read_workfile     (filename,ichoice,icheck,   msg,ierr)
ccc    save_tempfile     (lun,                       msg,ierr)
ccc    read_tempfile     (lun,                       msg,ierr)





      subroutine start_vel_data (vdpoint)
c     initialize velocity data in common block vd in vel_data.inc.
c     also return pointer to common block vd, so that C routines can access it.

      implicit none
      integer vdpoint,location,ierr,i,ntypes
      character*1 msg,descrip
      include 'vel_data.inc'

      data nfun,ifun/0,1/
      data nhx,nhy,changeflag/37,38,0/
      data order,nhosign,nhoexp/2,1.0,2.0/
      data offmin,offmax,muteflag,nmute/0.,10000.,2,0/
      data startvel,timetol,depthtol/5000.,0.02,10./
      data xcenter,xwidth,ycenter,ywidth/0.0,5.0,0.0,5.0/
      data xtol,ytol,xtol_request,ytol_request/1.5,9999.0,1.5,9999.0/
      data auto_xtol,auto_ytol/1,0/
      data ybin_request,xdirwant,ydirwant/0.0,0,0/
c     data veltype/'VTNM','VTRM','VZRM','VLRM',
c    $                    'VTAV','VZAV','VLAV',
c    $                    'VTIN','VZIN','VLIN','VTDP'/

      vdpoint=location(nfun)   ! nfun = first variable in common block vd.

      do i=1,numtypes
           call velfun_type_ii2cc (i,   veltype(i),descrip,ntypes)
      end do

      call set_fnil (-999.)
      call get_fnil (fnil)
      call get_inil (inil)
      startvel=fnil                 ! added 3/13/97
      bottomtime=fnil

      call alloc_words (point(1)        ,8*nmax)
      call alloc_words (point(nfunmax+1),8*nmax)
      call alloc_words (point(nfunmax+2),8*nmax)

      xbin   (nfunmax+3)=0.
      ybin   (nfunmax+3)=0.
      n      (nfunmax+3)=0
      point  (nfunmax+3)=0 
      vfid   (nfunmax+3)=' '
      select (nfunmax+3)=' '
      type   (nfunmax+3)='VTNM'
      errmsg (nfunmax+3)='N'
      raymsg (nfunmax+3)=' '
      project(nfunmax+3)='none'
      line   (nfunmax+3)='none'
      rdate  (nfunmax+3)='none'
      pdate  (nfunmax+3)='none'
      userid (nfunmax+3)='none'
      comment(nfunmax+3)='none'

      call clear_velfun (1)
      call clear_velfun (nfunmax+1)
      call clear_velfun (nfunmax+2)

      call change_file_size  (0,   msg,ierr)
      return
      end






      subroutine change_file_size (nfunwant,   msg,ierr)
c     change number of velocity functions in the file in memory.
c     if nfunwant = nfun, nothing is done.
c     if nfunwant < nfun, velocity functions are removed.
c     if nfunwant > nfun, empty velocity functions are added.
c     upon return, nfun is set to nfunmax (but not outside of 0,nfunmax).
c     upon return, ifun is changed only as necessary.

      implicit none
      integer nfunwant,ierr,kount,i
      character*(*) msg
      include 'vel_data.inc'

      kount=min(nfunmax,max(nfunwant,0))-nfun
      if (kount.gt.0) then                            ! add functions
           do i=1,kount
                call alloc_last_velfun (msg,ierr)
           end do
      else if (kount.lt.0) then                       ! remove functions
           do i=1,-kount
                call dealloc_last_velfun (msg,ierr)
           end do
      end if
      ifun=max(1,min(nfun+1,nfunmax,ifun))
      return
      end




      subroutine get_select_counts (kount)

      implicit none
      integer kount,i
      include 'vel_data.inc'
      
      kount=0
      if (nfun.eq.0) return
      do i=1,nfun
           if (select(i)(1:1).ne.' ') kount=kount+1
      end do
      return
      end




      subroutine get_errmsg_counts (kount)

      implicit none
      integer kount,i
      include 'vel_data.inc'
      
      kount=0
      if (nfun.eq.0) return
      do i=1,nfun
           if (errmsg(i).ne.' ') kount=kount+1
      end do
      return
      end




      subroutine get_raymsg_counts (kount)

      implicit none
      integer kount,i
      include 'vel_data.inc'
      
      kount=0
      if (nfun.eq.0) return
      do i=1,nfun
           if (raymsg(i).ne.' ') kount=kount+1
      end do
      return
      end






      subroutine clear_velfun (i)
C     Call this routine to clear an existing velocity function to zero.
C     The cleared values are in nfunmax+3.
C     Normally, i must lie between 1 and min(nfun+1,nfunmax).
C     If i = nfunmax+1 or +2, a velocity function buffer is cleared.
C     No checks are made on the validity of i.
C     Does nothing to point(i).

      implicit none
      integer i,keep
      include 'vel_data.inc'

      keep=point(i)
      call move_velfun (nfunmax+3,i)
      point(i)=keep
      return
      end





      subroutine move_velfun (i,j)
c     Call this routine to copy a velocity function from location i to
c        location j.
C     Normally, i and j must lie between 1 and min(nfun+1,nfunmax).
C     If i or j = nfunmax+1 or +2, a velocity function buffer is referred to.
C     No checks are made on the validity of i and j.
c     The picks in allocated memory are not touched; only the pointer
C        is copied.

      implicit none
      integer i,j
      include 'vel_data.inc'

      xbin   (j)=xbin   (i)
      ybin   (j)=ybin   (i)
      n      (j)=n      (i)
      point  (j)=point  (i)
      vfid   (j)=vfid   (i)
      select (j)=select (i)
      type   (j)=type   (i)
      errmsg (j)=errmsg (i)
      raymsg (j)=raymsg (i)
      project(j)=project(i)
      line   (j)=line   (i)
      rdate  (j)=rdate  (i)
      pdate  (j)=pdate  (i)
      userid (j)=userid (i)
      comment(j)=comment(i)
      return
      end





      subroutine copy_velfun (i,j)
c     Call this routine to copy velocity function from location i
c        to location j.
C     Normally, i and j must lie between 1 and min(nfun+1,nfunmax).
C     If i or j = nfunmax+1 or +2, a velocity function buffer is referred to.
C     No checks are made on the validity of i and j.
c     The pointer is not copied; the picks themselves are moved.
c     It is assumed that the pointers for locations i and j are already
c        allocated.

      implicit none
      integer i,j,keep,p,q,k
      include 'vel_data.inc'

      keep=point(j)
      call move_velfun (i,j)
      point(j)=keep

      call offset_index_words (p,depths,point(i))
      call offset_index_words (q,depths,point(j))
      do k=1,n(i)
           depths (q+k)=depths (p+k)
           times  (q+k)=times  (p+k)
           vnmo   (q+k)=vnmo   (p+k)
           vrms   (q+k)=vrms   (p+k)
           vav    (q+k)=vav    (p+k)
           vint   (q+k)=vint   (p+k)
           thick  (q+k)=thick  (p+k)
           offpick(q+k)=offpick(p+k)
      end do
      return
      end






      subroutine insert_velfun (i,   msg,ierr)
c     Call this routine to insert a velocity function from the buffer into
C      location i by moving the functions from locations i   thru nfun+1
C                                           to locations i+1 thru nfun+2.
C     i should lie within range 1 thru nfun+1.
c     nfun is then incremented by 1.
c     ifun is not changed unless it exceeds i, in which case it is
c        incremented by one to keep it pointing to the same function.
c     If ifun=i, the function pointed to by ifun will change.
c     It is assumed that a pointer is already allocated for location
c      nfun+1 unless this exceeds nfunmax.
C     This routine uses buffer nfunmax+1.
c     This routine changes xbin and ybin to lie between the flanking functions.

      implicit none
      integer i,ierr,j,ipoint,last
      character*(*) msg
      include 'vel_data.inc'

      if (nfun.eq.nfunmax) then
           msg='ran out of space - cannot insert velocity function'
           ierr=1
           return
      else if (i.lt.1.or.i.gt.nfun+1) then
           msg='invalid location for inserting velocity function'
           ierr=1
           return
      end if
      if (nfun+1.lt.nfunmax) then
           call alloc_words (ipoint,8*nmax)
           if (ipoint.eq.0) then
             msg='ran out of memory - cannot insert velocity function'
             ierr=1
             return
           end if
      else                          ! now nfun+1 = nfunmax.
           ipoint=point(nfunmax)
      end if
      last=min(nfun+1,nfunmax-1)
      if (i.le.last) then
           do j=last,i,-1
                call move_velfun (j,j+1)
           end do
      end if
      nfun=nfun+1
      point(i)=ipoint
      call copy_velfun (nfunmax+1,i)        ! pointer not copied.
      call verify_ybin (i,   ierr)
      if (ierr.ne.0) vfid(i)='blank'
      call verify_xbin (i,   ierr)
      if (ierr.ne.0) vfid(i)='blank'
      if (ifun.gt.i) ifun=ifun+1
      call fix_select_codes
      msg='velocity function inserted from buffer'
      changeflag=1
      ierr=0
      return
      end






      subroutine remove_velfun (i,   msg,ierr)
c     Call this routine to remove a velocity function from location i into
C      the buffer by moving the functions from locations i+1 thru nfun+1
C                                           to locations i   thru nfun.
C     i should lie within range 1 thru nfun.
c     nfun is then decremented by 1.
c     ifun is not changed unless it exceeds i, in which case it is
c        decremented by one to keep it pointing to the same function.
c     If ifun=i, the function pointed to by ifun will change.
c     It is assumed that a pointer is already allocated for location
c      nfun+1 unless this exceeds nfunmax.
C     This routine uses buffer nfunmax+1.

      implicit none
      integer i,ierr,j,ipoint,last
      character*(*) msg
      include 'vel_data.inc'

      if (i.eq.nfun+1) then
           msg='no velocity function to remove at this location'
           ierr=1
           return
      else if (nfun.eq.0) then
           msg='no velocity functions to remove'
           ierr=1
           return
      else if (i.lt.1.or.i.gt.nfun) then
           msg='invalid location for removing velocity function'
           ierr=1
           return
      end if
      call copy_velfun (i,nfunmax+1)        ! pointer not copied.
      if (nfun.lt.nfunmax) then
           call dealloc_memory (point(i))
      else
           ipoint=point(i)
      end if
      last=min(nfun+1,nfunmax)
      if (i.lt.last) then
           do j=i+1,last
                call move_velfun (j,j-1)
           end do
      end if
      if (nfun.eq.nfunmax) then
           point(nfun)=ipoint
           call clear_velfun (nfun)          ! pointer not cleared.
      end if
      nfun=nfun-1
      if (ifun.gt.i) ifun=ifun-1
      call fix_select_codes
      msg='velocity function removed into buffer'
      changeflag=1
      ierr=0
      return
      end






      subroutine delete_velfuns (ichoice,   deleted,msg,ierr)
c     Call this routine to delete velocity functions. 
c     ichoice = 1 means to delete selected velocity functions.
c     ichoice = 2 means to delete all velocity functions.
c     ichoice = 3 means to delete the active velocity function only.
c     nfun is then reset as necessary.
c     If the active function is not deleted, ifun is changed to still
c     point to the same function.
c     If the active function is deleted, ifun is not changed except
c        to keep it within bounds.
c     Returns deleted=.TRUE.  if the active function was deleted.
c     Returns deleted=.TRUE.  if the active function was at nfun+1.
c     Returns deleted=.FALSE. if the active function was retained.
c     Returns deleted=.FALSE. if there are no velocity functions.

      implicit none
      integer ichoice,ierr,i,j,jfun,kount
      character*(*) msg
      logical deleted,choose_velfun
      include 'vel_data.inc'

      if (nfun.eq.0) then
           deleted=.FALSE.
           msg='no velocity functions'
           ierr=1
           return
      end if
      j=0
      jfun=0
      do i=1,nfun
           if (choose_velfun(ichoice,i)) then       ! deleting.
                if (i.eq.ifun) jfun=-j-1
           else                                     ! retaining.
                j=j+1
c               call move_velfun (i,j)   ! removed 8/11/95
                call copy_velfun (i,j)   ! added   8/11/95
                if (i.eq.ifun) jfun=j
           end if
      end do
      if (jfun.eq.0) then
           ifun=min(j+1,nfunmax)
           deleted=.TRUE.             ! active function at nfun+1.
      else if (jfun.lt.0) then
           ifun=iabs(jfun)
           deleted=.TRUE.             ! active function deleted.
      else
           ifun=jfun
           deleted=.FALSE.            ! active function retained.
      end if
      kount=nfun-j                  ! number  of functions deleted.
      if (kount.gt.0) then
           do i=1,kount
                call dealloc_last_velfun (msg,ierr)
           end do
           call fix_select_codes
           write (msg,1000) kount
1000       format (I5,' functions deleted')
           changeflag=1
           ierr=0
      else
           msg='no velocity functions deleted'
           ierr=1
      end if
      return
      end





      subroutine alloc_last_velfun (msg,ierr)
c     Call this routine to allocate and clear a new velocity function
c         at the end of the velocity function list.
c     nfun if incremented if successful.
c     A new pointer is created to point to the new function.
c     This velocity function is not made the current active one.
c     It is assumed that a pointer is already allocated at nfun+1,
c        and this routine allocates at nfun+2 (the new nfun+1).

      implicit none
      character*(*) msg
      integer ierr
      include 'vel_data.inc'

      if (nfun.eq.nfunmax) then
           msg='ran out of space - cannot add new velocity function'
           ierr=1
           return
      else if (nfun+2.le.nfunmax) then
           call alloc_words (point(nfun+2),8*nmax)
           if (point(nfun+2).eq.0) then
             msg='ran out of memory - cannot add new velocity function'
             ierr=1
             return
           end if
           call clear_velfun (nfun+2)      ! pointer not cleared.
      end if
      nfun=nfun+1
      msg='new velocity function added'
      ierr=0
      return
      end




      subroutine dealloc_last_velfun (msg,ierr)
c     Call this routine to de-allocate the last velocity function
c         at the end of the velocity function list.
c     nfun if decremented if successful.
c     The pointer to this function is freed.
c     If this function is the active one, ifun is changed.
c     It is assumed that a pointer is already allocated at nfun+1,
c        and this routine de-allocates at nfun+1 (the new nfun+2).

      implicit none
      character*(*) msg
      integer ierr
      include 'vel_data.inc'

      if (nfun.eq.0) then
           msg='no velocity functions to remove'
           ierr=1
           return
      else if (nfun.lt.nfunmax) then
           call dealloc_memory (point(nfun+1))
      end if
      call clear_velfun (nfun)       ! pointer not cleared.
      nfun=nfun-1
      ifun=min(ifun,nfun+1,nfunmax)
      msg='last velocity function removed'
      ierr=0
      return
      end





      subroutine verify_ybin (i,   ierr)
c     Call this routine to verify that the coordinate ybin(i) falls
c        between the preceding and following coordinates.
c     Index i can be from 1 thru nfun+1.
c     The valid previous range of indices is from 0 thru nfun.
c     It is assumed that the rest of the ybin coordinates are ok.
c     Also rounds ybin(i) to the nearest 0.01 in value.
c     Changes ybin(i) and returns error if the test fails.
c     Allows both ascending and descending order.

      implicit none
      integer i,ierr,ifirst,ilast
      real factor,keep
      include 'vel_data.inc'

c----------------do rounding and check trivial cases.
      ierr=0
c     ybin(i)=nint(100.*ybin(i))/100.
      if (nfun.le.1) return
c----------------get ybin direction.
      ifirst=1
      ilast=nfun
      if (i.eq.1) ifirst=2
      if (i.eq.nfun) ilast=nfun-1
      if (ilast.le.ifirst) then
           factor=0.
      else if (ybin(ilast).gt.ybin(ifirst)) then
           factor=1.
      else if (ybin(ilast).lt.ybin(ifirst)) then
           factor=-1.
      else
           factor=0.
      end if
c----------------now check ybin(i).
      if (factor.eq.0.) then
           if (i.gt.1.and.i.lt.nfun) then
                if (ybin(i).ne.ybin(i-1).or.ybin(i).ne.ybin(i+1)) then
                     ybin(i)=ybin(i-1)
                     ierr=1
                end if
           end if
      else
           if (i.gt.1) then
                if (factor*ybin(i).lt.factor*ybin(i-1)) then
                     ybin(i)=ybin(i-1)
                     ierr=1
                end if
           end if
           if (i.lt.nfun) then
                if (factor*ybin(i).gt.factor*ybin(i+1)) then
                     ybin(i)=ybin(i+1)
                     ierr=1
                end if
           end if
      end if
c----------------check compatibility of ybin(i) with xbin(i).
      if (ierr.eq.0) then
           keep=xbin(i)
           call verify_xbin (i,   ierr)
           xbin(i)=keep
      end if
      return
      end



      subroutine verify_xbin (i,   ierr)
c     Call this routine to verify that the coordinate xbin(i) falls
c        between the preceding and following coordinates.
c     Index i can be from 1 thru nfun+1.
c     The valid previous range of indices is from 0 thru nfun.
c     It is assumed that the ybin coordinates are ok.
c     It is assumed that the rest of the xbin coordinates are ok.
c     Also rounds xbin(i) to the nearest 0.01 in value.
c     Changes xbin(i) and returns error if the test fails.
c     Allows both ascending and descending order.

      implicit none
      integer i,ierr,j,k
      real factor
      logical xlower,xupper
      include 'vel_data.inc'

c----------------do rounding and check trivial cases.
      ierr=0
c     xbin(i)=nint(100.*xbin(i))/100.
      if (nfun.le.1) return
c----------------get xbin direction.
      factor=0.
      k=0
      do j=1,nfun
           if (j.ne.i) then
                if (k.eq.0) then
                     k=j
                else if (ybin(j).eq.ybin(k)) then
                     if (xbin(j).gt.xbin(k)) then
                          factor=1.
                          go to 10
                     else if (xbin(j).lt.xbin(k)) then
                          factor=-1.
                          go to 10
                     end if
                else
                     k=j
                end if
           end if
      end do
c----------------determine existence of valid boundaries.
10    xlower=.false.
      xupper=.false.
      if (i.gt.1)    xlower=ybin(i).eq.ybin(i-1)
      if (i.lt.nfun) xupper=ybin(i).eq.ybin(i+1)
c----------------now check xbin(i).
      if (factor.eq.0.) then
           if (xlower.and.xupper) then
                if (xbin(i).ne.xbin(i-1)) then
                     xbin(i)=xbin(i-1)
                     ierr=1
                end if
           end if
      else
           if (xlower) then
                if (factor*xbin(i).le.factor*xbin(i-1)) then
                     if (xupper) then
                          xbin(i)=0.5*(xbin(i-1)+xbin(i+1))
                     else
                          xbin(i)=xbin(i-1)+factor
                     end if
                     ierr=1
                end if
           end if
           if (xupper) then
                if (factor*xbin(i).ge.factor*xbin(i+1)) then
                     if (xlower) then
                          xbin(i)=0.5*(xbin(i-1)+xbin(i+1))
                     else
                          xbin(i)=xbin(i+1)-factor
                     end if
                     ierr=1
                end if
           end if
      end if
      return
      end





      subroutine adjust_select_codes (i)
c     increment the select code for velfun i, and adjust all select
c        codes accordingly.

      implicit none
      integer i,j,k,flag
      include 'vel_data.inc'
      character*6 choices(7)
      save choices
      data choices/' ','*yes',' no','*start','*stop',' stop','*'/

      do k=1,7
           if (select(i).eq.choices(k)) go to 20
      end do
      k=6
20    if (k.eq.7) k=1
      select(i)=choices(mod(k,6)+1)

      entry fix_select_codes
      flag=1
      do j=1,nfun
           if (select(j).eq.choices(4)) then
                flag=7
           else if (select(j)(2:6).eq.choices(5)(2:6)) then
                flag=1
           else if (select(j)(2:6).eq.' ') then
                select(j)=choices(flag)
           end if
      end do
      return
      end





      subroutine delete_select_codes
c     delete all select codes.

      implicit none
      integer i
      include 'vel_data.inc'

      do i=1,nfun
           select(i)=' '
      end do
      return
      end

  


      logical function choose_velfun (ichoice,i)

      implicit none
      integer ichoice,i
      include 'vel_data.inc'

      choose_velfun=(ichoice.eq.1.and.select(i)(1:1).ne.' ')
     $          .or.(ichoice.eq.2                          )
     $          .or.(ichoice.eq.3.and.i.eq.ifun            )
      return
      end
   





      subroutine put_velfun_pick (i,type2,j,x,v,  msg,ierr)
c     Call this routine to put a single pick into an existing velocity function,
c        and set the pick for other velocity types to nil.
C     The type is not reset to type2.
C     The pick j must lie between 1 and n(i).
c     If x or v is nil, returns ierr non-zero.

      implicit none
      integer i,j,ierr,p
      character*(*) type2,msg
      real x,v
      include 'vel_data.inc'

      call offset_index_words (p,depths,point(i))
      p=p+1
      call velfun_put_pick (type2,j,x,v,
     $    depths(p),times(p),vnmo(p),vrms(p),vav(p),vint(p),thick(p),
     $    msg,ierr)
      return
      end





      subroutine get_velfun_pick (i,type2,   j,x,v,msg,ierr)
c     Call this routine to get a single pick from an existing velocity function.
C     The type is not reset to type2.
C     The pick j must lie between 1 and n(i).
c     If x or v is nil, returns ierr non-zero.

      implicit none
      integer i,j,ierr,p
      character*(*) type2,msg
      real x,v
      include 'vel_data.inc'

      call offset_index_words (p,depths,point(i))
      p=p+1
      call velfun_get_pick (type2,j,x,v,
     $    depths(p),times(p),vnmo(p),vrms(p),vav(p),vint(p),thick(p),
     $    msg,ierr)
      return
      end




      subroutine put_velfun_picks (i,type2,n2,x,v,  msg,ierr)
c     Call this routine to put picks into an existing velocity function,
c        and set the other velocity types to nil.
C     The type is not reset to type2.
C     The number of picks is of course reset to n2.
c     If some of x,v are nil, returns ierr non-zero.

      implicit none
      integer i,n2,ierr,p
      character*(*) type2,msg
      real x(*),v(*)
      include 'vel_data.inc'

      call offset_index_words (p,depths,point(i))
      p=p+1
      call velfun_put_picks (type2,n2,x,v,
     $    depths(p),times(p),vnmo(p),vrms(p),vav(p),vint(p),thick(p),
     $    msg,ierr)
      n(i)=n2
      return
      end





      subroutine get_velfun_picks (i,type2,   n2,x,v,msg,ierr)
c     Call this routine to get picks from an existing velocity function.
C     The type is not reset to type2.
C     The number of picks n2 is set to n(i).
c     If some of x,v are nil, returns ierr non-zero.

      implicit none
      integer i,n2,ierr,p
      character*(*) type2,msg
      real x(*),v(*)
      include 'vel_data.inc'

      call offset_index_words (p,depths,point(i))
      p=p+1
      call velfun_get_picks (type2,n(i),x,v,
     $    depths(p),times(p),vnmo(p),vrms(p),vav(p),vint(p),thick(p),
     $    msg,ierr)
      n2=n(i)
      return
      end





      subroutine update_velfun (i,invoke,type2,   msg,ierr)
c     Call this routine whenever a velocity function needs to be updated
c         due to the addition, deletion, or modification of picks.
c     i = any valid (existing) velocity function number.
c     If a velocity conversion error occurs, this routine returns ierr=1.
c     This velocity function is not made the current active one.
C     The type is not reset to type2.

      implicit none
      integer i,invoke,ierr,p
      character*(*) type2,msg
      include 'vel_data.inc'

      call offset_index_words (p,depths,point(i))
      p=p+1
      if (n(i).gt.0) then
           call velfun_fixup_first_pick (type2,n(i),nmax,
     $       startvel,timetol,depthtol,
     $       depths(p),times(p),vnmo(p),vrms(p),vav(p),vint(p),thick(p))
      end if
c     if (n(i).gt.0.and.type2(1:2).eq.'VT'.and.bottomtime.ne.fnil) then
c         if (times(p+n(i)-1).gt.bottomtime-timetol)
c    $                                       times(p+n(i)-1)=bottomtime
c     end if
c     if(n(i).ge.2.and.bottomtime.ne.fnil.and.
c    $   times(p+n(i)-1).ne.fnil.and.times(p+n(i)-1).ge.bottomtime) then
c       if(times(p+n(i)-1).eq.times(p+n(i)-2)) then
c         call velfun_remove_pick (n(i)-1,n(i),
c    $      depths(p),times(p),vnmo(p),vrms(p),vav(p),vint(p),thick(p))
c       end if
c     end if
      call velfun_update (invoke,type2,n(i),
     $    muteflag,offmin,offmax,nmute,omute,tmute,
     $    depths(p),times(p),vnmo(p),vrms(p),vav(p),vint(p),thick(p),
     $    offpick(p),nray,oray,tray,   msg,ierr)
      errmsg(i)=' '
      raymsg(i)=' '
      if (ierr.ne.0) errmsg(i)='E'
      if (n(i).lt.2) errmsg(i)='N'
      if (invoke.ne.0) then
           raymsg(i)='R'
           if (type2.eq.'VTNM') raymsg(i)='I'
      end if
      changeflag=1
      return
      end





      subroutine inquire_velfile (filename,iexist,   msg,ierr)
c     inquire about the existence of a velocity file.
c     also add .vel extension to filename if it has none.

      implicit none
      character*(*) filename,msg
      character*80 temporary
      integer iexist,ierr

      if (filename.eq.' ') then
           iexist=0
           msg='blank file name'
           ierr=1
           return
      end if
      call addext (filename,'vel')
      call textfile_inquire (filename,iexist,ierr)
      temporary=filename
      call addext_replace (temporary,'work')
      if (ierr.ne.0) then
           iexist=0
           msg='error while trying to inquire about the file'
      else if (iexist.eq.1) then
           msg='CPS velocity file found'
           if (temporary.eq.filename) msg='workfile found'
      else
           msg='CPS velocity file not found'
           if (temporary.eq.filename) msg='workfile not found'
      end if
      return
      end

          



      subroutine open_old_velfile (lun,filename,ichoice,kount,icheck,
     $                                                       msg,ierr)
c     open an old velocity file to read.
c     lun       = returned logical unit number.
c     filename  = file name to read.
c     ichoice   = whether to add or replace functions in memory.
c     icheck    = how to check headers.
c     kount     = number of velocity functions to read from file (returned).
c     msg       = returned message.
c     ierr      = error return (0 if no error).
C     nhx and nhy are updated in the database if necessary.
C     order and nhosign and nhoexp are updated in the database if necessary.
c     the velocity file is closed if an error occurs.

      implicit none
      integer lun
      integer ichoice,icheck,ierr,nvpp,kount,nhx2,nhy2,iexist
      real nhosign2,nhoexp2
      character*(*) filename,msg
      include 'vel_data.inc'

      call inquire_velfile (filename,iexist,   msg,ierr)
      if (iexist.eq.0) ierr=1
      if (ierr.ne.0) return
      call getlun (lun,*992)
      call cpsvf_setup (.false.,.false.)
      call open_cps_velfile3 (lun,filename,'READ',nvpp,kount,
     $                     *999,*999,nhx2,nhy2,nhosign2,nhoexp2)
      call check_input_file (kount,nhx2,nhy2,nhosign2,nhoexp2,
     $                                       ichoice,icheck,   msg,ierr)
      if (ierr.ne.0) then
           call close_cps_velfile (lun,'READ')
           return
      end if
      msg='old velocity file successfully opened'
      ierr=0
      return

992   msg='no logical unit numbers available'
      ierr=1
      return

999   call close_cps_velfile (lun,'READ')
      msg='open/read error on old velocity file'
      ierr=1
      return
      end






      subroutine open_new_velfile (lun,filename,ichoice,kount,kounte,
     $                                                        msg,ierr)
c     open a new velocity file to save.
c     lun       = logical unit number (returned).
c     filename  = file name to save.
c     ichoice = 1 means to save selected velocity functions.
c     ichoice = 2 means to save all velocity functions.
c     ichoice = 3 means to save the active velocity function only.
c     kount     = number of velocity functions to save on file (returned).
c     kounte    = number of functions with 0 picks(not to save)(returned).
c     msg       = returned message.
c     ierr      = error return (0 if no error).
c     the velocity file is closed if an error occurs.


      implicit none
      integer lun
      integer ichoice,nvpp,i,ierr,kount,iexist,kounte
      character*(*) filename,msg
      logical choose_velfun
      include 'vel_data.inc'

      call inquire_velfile (filename,iexist,   msg,ierr)
      if (ierr.ne.0) return

      kount=0
      kounte=0
      do i=1,nfun
           if (choose_velfun(ichoice,i)) then
                if (n(i).gt.0) then
                     kount=kount+1
                else
                     kounte=kounte+1
                end if
           end if
      end do
      if (kount.eq.0) then
           if (kounte.gt.0) then
                msg='no velocity functions (with picks) to save'
           else
                msg='no velocity functions to save'
           end if
           ierr=1
           return
      end if

      call getlun (lun,*992)
      call cpsvf_setup (.false.,.false.)
      nvpp=2
      call open_cps_velfile3 (lun,filename,'WRITE',nvpp,kount,
     $                        *999,*999,nhx,nhy,nhosign,nhoexp)
      msg='new velocity file successfully opened'
      ierr=0
      return

992   msg='no logical unit numbers available'
      ierr=1
      return

999   call close_cps_velfile (lun,'WRITE')
      msg='open/write error on new velocity file'
      ierr=1
      return
      end

    




      subroutine read_velfun (lun,   msg,ierr)
c     Call this routine to read a velocity function from a file,
c        and add it to the end of the functions you already have in
c        the database.
c     Returns ierr non-zero only if the file read should be aborted.
c     Closes the file before returning ierr=1.

      implicit none
      integer lun,ierr,nvpp
      parameter (nvpp=2)
      real x(500),v(500),sv
      character*(*) msg
      include 'vel_data.inc'

      call alloc_last_velfun (msg,ierr)
      if (ierr.ne.0) then
           call close_cps_velfile (lun,'READ')
           return
      end if
      call read_cps_velfile (lun,nvpp,vfid(nfun),n(nfun),
     $        xbin(nfun),ybin(nfun),type(nfun),x,v,
     $        *999,*999,project(nfun),line(nfun),rdate(nfun),
     $        pdate(nfun),userid(nfun),comment(nfun),sv)
      if (n(nfun).gt.nmax) then
           call dealloc_last_velfun (msg,ierr)
           call close_cps_velfile (lun,'READ')
           msg='too many picks - remaining velocity functions lost'
           ierr=1
           return
      end if
      call put_velfun_picks (nfun,type(nfun),n(nfun),x,v,   msg,ierr)
      call update_velfun (nfun,0,type(nfun),   msg,ierr)
      if (nfun.eq.1) then
           type   (nfunmax+3)=type   (nfun)
           project(nfunmax+3)=project(nfun)
           line   (nfunmax+3)=line   (nfun)
           rdate  (nfunmax+3)=rdate  (nfun)
           pdate  (nfunmax+3)=pdate  (nfun)
           userid (nfunmax+3)=userid (nfun)
           comment(nfunmax+3)=comment(nfun)
           call clear_velfun (nfunmax+1)
           call clear_velfun (nfunmax+2)
      end if
      ierr=0
      return

999   call dealloc_last_velfun (msg,ierr)
      call close_cps_velfile (lun,'READ')
      msg='file read error - remaining velocity functions lost'
      ierr=1
      return
      end




      subroutine get_good_velfun_picks (i,type3,   n2,x,v,msg,ierr)
c     Call this routine to get picks from an existing velocity function.
c     Returns the function in this priority order:
C        (1) Returns type3 if type3 is not blank and type3 has no nil values.
c        (2) Returns type(i) if type3 is blank or type3 has nil values.
c        (3) Returns any type it can if type(i) has nil values.
c        (4) Returns type(i) if all types have nil values.
c     If type3 is blank, gets the individual type given by type(i).
c     If type3 is not blank, gets the type given by type3.
c     If nil values are encountered, tries to find a different type
C       that has no nil values, and returns ierr=1 whether or not
C       it succeeds.
c     The value of type3 is reset to the actual type returned.
C     The individual type(i) is not reset to type3.
C     The number of picks n2 is set to n(i).
c     The values of x(n2) and v(n2) are set for the returned type.
c     If n(i) is zero, ierr=1 is returned.

      implicit none
      integer i,n2,ierr,ierr2,ntypes,j
      character*(*) type3,msg
      character*1 descrip
      real x(*),v(*)
      include 'vel_data.inc'

      n2=n(i)
      if (type3.eq.' ') type3=type(i)
      call get_velfun_picks (i,type3,n2,x,v,   msg,ierr)
      if (ierr.eq.0.or.n2.eq.0) return
      if (type3.ne.type(i)) then
           type3=type(i)
           call get_velfun_picks (i,type3,n2,x,v,   msg,ierr2)
           if (ierr2.eq.0) return
      end if
      do j=1,numtypes
           call velfun_type_ii2cc (j,   type3,descrip,ntypes)
           call get_velfun_picks (i,type3,n2,x,v,   msg,ierr2)
           if (ierr2.eq.0) return
      end do
      type3=type(i)
      call get_velfun_picks (i,type3,n2,x,v,   msg,ierr2)
      return
      end





      subroutine save_velfun (lun,i,type2,   msg,ierr)
c     Call this routine to save a velocity function to a file.
c     If type2 is blank, saves the individual type given by type(i).
c     If type2 is not blank, saves the type given by type2.
c     Returns ierr=1 if the type saved is not the desired type because
C        of nil values encountered, or if the type saved has nil values.

      implicit none
      integer lun,ierr,nvpp,i
      parameter (nvpp=2)
      real x(500),v(500),sv
      character*(*) type2,msg
      character*4 type3
      include 'vel_data.inc'

      type3=type2
      call get_good_velfun_picks (i,type3,n(i),x,v,   msg,ierr)
      call write_cps_velfile (lun,nvpp,vfid(i),n(i),
     $        xbin(i),ybin(i),type3,x,v,
     $        project(i),line(i),rdate(i),
     $        pdate(i),userid(i),comment(i),sv)
      return
      end





      subroutine close_old_velfile (lun,filename,kount,msg,ierr)
c     close an old velocity file which has been read.

      implicit none
      integer lun,kount,ierr
      character*(*) filename,msg

      call close_cps_velfile (lun,'READ')
      msg=' '
      write (msg,1000) kount,filename
1000  format (I5,' functions read from file ',A40)
      ierr=0
      return
      end




      subroutine close_new_velfile (lun,filename,kount,kounte,kountf,
     $                                             type2,   msg,ierr)
c     close a new velocity file which has been written.

      implicit none
      integer lun,kount,kounte,kountf,ierr
      character*(*) filename,type2,msg
      character*150 msg2

      call close_cps_velfile (lun,'WRITE')
      msg2=' '
      if (kounte.eq.0.and.kountf.eq.0) then
           write (msg2,1000) kount,type2,filename
1000       format (I5,' functions ',A4,' saved to file ',A80)
      else if (kountf.eq.0) then
           write (msg2,2000) kounte,filename
2000       format (I5,' functions without picks not saved to file ',A80)
      else if (kounte.eq.0) then
           write (msg2,3000) kountf,filename
3000       format (I5,' functions with errors included in file ',A80)
      else
           write (msg2,4000) kounte,kountf
4000       format ('skipped ',I5,' empty functions and included ',
     $                  I5,' functions with errors')
      end if
      msg=msg2
      ierr=0
      return
      end




      subroutine save_workfile (filename,ichoice,type2,   msg,ierr)
c     save all velocity functions on a workfile.
c     ichoice=1 means to save selected functions.
c     ichoice=2 means to save all functions.
c     ichoice=3 means to save active function.

      implicit none
      integer ichoice,lun,ierr,i,j,nfun2,version
      character*(*) filename,type2,msg
      character*4 type3
      include 'vel_data.inc'
      real x(nmax),v(nmax)
      logical choose_velfun
      save version
      data version/-1/

      if (nfun.eq.0) then
           msg='no functions to save on workfile'
           ierr=0
           return
      end if
      if (filename.eq.' ') then
           msg='trying to save workfile with blank name'
           ierr=1
           return
      end if
      nfun2=0
      do i=1,nfun
           if (choose_velfun(ichoice,i)) nfun2=nfun2+1
      end do
      if (nfun2.eq.0) then
           msg='no selected functions to save on workfile'
           ierr=0
           return
      end if
      call getlun (lun,*992)
      open (lun,file=filename,err=999,status='unknown',
     $                                            form='unformatted')
      write (lun) version
      write (lun) nfun2,nhx,nhy,nhosign,nhoexp
      do i=1,nfun
           if (choose_velfun(ichoice,i)) then
                type3=type2
                call get_good_velfun_picks (i,type3,n(i),x,v,  msg,ierr)
                write (lun) vfid(i),n(i),xbin(i),ybin(i),type3,
     $             project(i),line(i),rdate(i),pdate(i),userid(i),
     $             comment(i)
                if(n(i).gt.0) write (lun) (x(j),v(j),j=1,n(i))
           end if
      end do
      msg=' '
      write (msg,1000) nfun2,type2,filename
1000  format (I5,' functions ',A4,' saved to workfile ',A40)
      ierr=0
      close (lun)
      return
992   msg='getlun error trying to save workfile'
      lun=0
      ierr=1
      return
999   msg='open error trying to save workfile'
      ierr=1
      close (lun)
      return
      end



      subroutine check_input_file (nfun2,nhx2,nhy2,nhosign2,nhoexp2,
     $                                       ichoice,icheck,   msg,ierr)
c     first forces nhx2, nhy2, nhosign2, nhoexp2 to be within range.
c     then checks velocity file (cps file or workfile) for empty file or
c        header word mismatch.
c     also empties out the previous data if ichoice = 1.
c     also changes header words to match file if no error and if necessary.
c     the first record of the file has already been read, giving 
c        nfun2,nhx2,nhy2,nhosign2,nhoexp2 on the file.
c     ichoice = 1 means to replace velocity functions in memory.
c     ichoice = 2 means to add to velocity functions in memory.
c     icheck = 0 requires header word match ONLY if there are existing
c                   functions in memory which will not be deleted.
c     icheck = 1 requires header word match EVEN if there are NO existing
c                   functions in memory which will not be deleted.
c     temporarily, icheck=1 automatically resets nhy2 to nhy.
c     returns ierr=1 if no functions on file, or if header word mismatch.

      implicit none
      integer nfun2,nhx2,nhy2,ichoice,icheck,ierr,order2
      real nhosign2,nhoexp2
      character*(*) msg
      character*80  msg2
      include 'vel_data.inc'

      if (nhx2.lt.1.or.nhx2.gt.64) nhx2=37
      if (nhy2.lt.1.or.nhy2.gt.64) nhy2=38
      if (icheck.eq.1) nhy2=nhy                ! temporary
      if (nhoexp2.le.3.0) then
           nhosign2= 1.0
           nhoexp2 = 2.0
           order2  = 2
      else
           nhosign2=-1.0
           nhoexp2 = 4.0
           order2  = 4
      end if
      if (nfun2.eq.0) then
           msg='no velocity functions on this file'
           ierr=1
           return
      end if
      if (icheck.eq.1.and.(nhx2.ne.nhx.or.nhy2.ne.nhy)) then
ccc        write (msg2,1000) nhx2,nhy2   ! temporarily replaced
           write (msg2,2000) nhx2        ! temporary
           msg=msg2
           ierr=1
           return
      end if
      if (icheck.eq.1.and.(nhosign2.ne.nhosign.or.
     $                     nhoexp2.ne.nhoexp.or.order2.ne.order)) then
           write (msg2,3000) order2
           msg=msg2
           ierr=1
           return
      end if
      if (ichoice.eq.1)  call change_file_size  (0,   msg,ierr)
      if (nfun.gt.0.and.(nhx2.ne.nhx.or.nhy2.ne.nhy)) then
ccc        write (msg2,1000) nhx2,nhy2   ! temporarily replaced
           if (icheck.eq.0) write (msg2,1000) nhx2,nhy2 ! temporary
           if (icheck.eq.1) write (msg2,2000) nhx2      ! temporary
           msg=msg2
           ierr=1
           return
      end if
      if (nfun.gt.0.and.(nhosign2.ne.nhosign.or.
     $                   nhoexp2.ne.nhoexp.or.order2.ne.order)) then
           write (msg2,3000) order2
           msg=msg2
           ierr=1
           return
      end if
      nhx    =nhx2
      nhy    =nhy2
      nhosign=nhosign2
      nhoexp =nhoexp2
      order  =order2
      ierr=0
      msg=' '
      return
1000  format ('header word mismatch - file has NHX = ',I2,
     $                                   ' and NHY = ',I2)
2000  format ('header word mismatch - file has NHX = ',I2)
3000  format ('NMO order mismatch - file has ORDER = ',I2)
      end




      subroutine read_workfile (filename,ichoice,icheck,   msg,ierr)
c     read all velocity functions from a workfile.
c     ichoice = whether to add or replace functions in memory.
c     icheck  = how to check headers.

c     if the first word on the workfile is a negative number,
c     that number is a version number for the workfile format:
c     (first record contains version number only)
c     (second record contains nfun,nhx,nhy,nhosign,nhoexp)

c     if the first word on the workfile is a positive or zero number,
c     the workfile is in the original (obsolete) format:
c     (first record contains nfun,nhx,nhy)

      implicit none
      integer ichoice,icheck,lun,ierr,i,j,k,version,nfun2,nhx2,nhy2
      real nhosign2,nhoexp2
      character*(*) filename,msg
      include 'vel_data.inc'
      real x(nmax),v(nmax)

      if (filename.eq.' ') then
           msg='trying to read workfile with blank name'
           ierr=1
           return
      end if
      call getlun (lun,*992)
      open (lun,file=filename,err=999,status='old',form='unformatted')
      read (lun) version
      if (version.ge.0) then
           rewind lun
           read (lun) nfun2,nhx2,nhy2
           nhosign2=0.0
           nhoexp2 =0.0
      else
           read (lun) nfun2,nhx2,nhy2,nhosign2,nhoexp2
      end if
      call check_input_file (nfun2,nhx2,nhy2,nhosign2,nhoexp2,
     $                                       ichoice,icheck,   msg,ierr)
      if (ierr.ne.0) then
           close(lun)
           return
      end if
      do k=1,nfun2
           call alloc_last_velfun (msg,ierr)
           if (ierr.ne.0) go to 997
           i=nfun
           read (lun) vfid(i),n(i),xbin(i),ybin(i),type(i),
     $        project(i),line(i),rdate(i),pdate(i),userid(i),comment(i)
           if(n(i).gt.0) read (lun) (x(j),v(j),j=1,n(i))
ccccc         print *, nfun2,k,i,n(i)
           call put_velfun_picks (i,type(i),n(i),x,v,   msg,ierr)
           call update_velfun (i,0,type(i),   msg,ierr)
           if (nfun.eq.1) then
                type   (nfunmax+3)=type   (nfun)
                project(nfunmax+3)=project(nfun)
                line   (nfunmax+3)=line   (nfun)
                rdate  (nfunmax+3)=rdate  (nfun)
                pdate  (nfunmax+3)=pdate  (nfun)
                userid (nfunmax+3)=userid (nfun)
                comment(nfunmax+3)=comment(nfun)
                call clear_velfun (nfunmax+1)
                call clear_velfun (nfunmax+2)
           end if
           if (k.eq.1) ifun=nfun
      end do
      ierr=0
      msg=' '
      write (msg,1000) nfun2,filename
1000  format (I5,' functions read from workfile ',A40)
997   close (lun)
      ifun=min(nfun+1,nfunmax,max(ifun,1))
      return
992   msg='getlun error trying to read workfile'
      ierr=1
      return
999   msg='open error trying to read workfile'
      ierr=1
      return
      end





      subroutine validate_velfile (filename,   nhx,nhy,nfun,info,ierr)

      implicit none
      integer lun,nvpp,ierr,nfun,nhx,nhy
      real nhosign,nhoexp
      character*(*) filename,info
      character*8 nonhyp

      nhx=0
      nhy=0
      nfun=0
      if (filename.eq.' ') then
           info='trying to validate velfile with blank name'
           ierr=1
           return
      end if
      call getlun (lun,*992)
      call cpsvf_setup (.false.,.false.)
      call open_cps_velfile3 (lun,filename,'READ',nvpp,nfun,
     $                               *999,*999,nhx,nhy,nhosign,nhoexp)
      if(nfun.ge.1.and.nfun.le.99999.and.
     $   nhx.ge.1.and.
     $   nhy.ge.1) then
           nonhyp=' '
           if (nhosign.ne.1.0.or.nhoexp.ne.2.0) nonhyp='NONHYP'
           if (nhosign.eq.0.0.and.nhoexp.eq.0.0) nonhyp=' '
           write (info,1000) nhx,nhy,nfun,nonhyp
1000       format ('headerwords:',2I3,' #functions:',I6,1X,A8)
           ierr=0
      else
           info='bad values in header of velfile'
           ierr=1
      end if
      call close_cps_velfile (lun,'READ')
      return
992   info='getlun error trying to validate velfile'
      ierr=1
      return
999   info='open error trying to validate velfile'
      ierr=1
      return
      end




      subroutine validate_workfile (filename,   nhx,nhy,nfun,info,ierr)

      implicit none
      integer lun,ierr,nfun,nhx,nhy,version
      real nhosign,nhoexp
      character*(*) filename,info
      character*8 nonhyp

      nhx=0
      nhy=0
      nfun=0
      if (filename.eq.' ') then
           info='trying to validate workfile with blank name'
           ierr=1
           return
      end if
      call getlun (lun,*992)
      open (lun,file=filename,err=999,status='old',form='unformatted')
      read (lun,err=666) version
      if (version.ge.0) then
           rewind lun
           read (lun,err=666) nfun,nhx,nhy
           nhosign=0.0
           nhoexp =0.0
      else
           read (lun,err=666) nfun,nhx,nhy,nhosign,nhoexp
      end if
      if(nfun.ge.1.and.nfun.le.99999.and.
     $   nhx.ge.1.and.
     $   nhy.ge.1) then
           nonhyp=' '
           if (nhosign.ne.1.0.or.nhoexp.ne.2.0) nonhyp='NONHYP'
           if (nhosign.eq.0.0.and.nhoexp.eq.0.0) nonhyp=' '
           write (info,1000) nhx,nhy,nfun,nonhyp
1000       format ('headerwords:',2I3,' #functions:',I6,1X,A8)
           ierr=0
      else
           info='bad values in header of workfile'
           ierr=1
      end if
      close(lun)
      return
992   info='getlun error trying to validate workfile'
      ierr=1
      return
999   info='open error trying to validate workfile'
      ierr=1
      return
666   info='error trying to validate header of workfile'
      close (lun)
      ierr=1
      return
      end








      subroutine save_tempfile (lun,   msg,ierr)
c     save all velocity functions on a temporary file.
c     lun is assigned, opened, and returned.

      implicit none
      integer lun,ierr,i,j,p
      character*(*) msg
      include 'vel_data.inc'

      if (nfun.eq.0) then
           msg='no functions to temporarily save'
           ierr=0
           lun=0
           return
      end if
      call getlun (lun,*992)
      open (lun,err=999,status='scratch',form='unformatted')
      write (lun) nfun,ifun
      do i=1,nfun
           call offset_index_words (p,depths,point(i))
           write (lun) vfid(i),n(i),xbin(i),ybin(i),type(i),
     $        project(i),line(i),rdate(i),pdate(i),userid(i),comment(i),
     $        select(i),errmsg(i),raymsg(i)
           write (lun) (depths(p+j),times(p+j),thick(p+j),vrms(p+j),
     $        vav(p+j),vint(p+j),vnmo(p+j),offpick(p+j),j=1,n(i))
      end do
      rewind lun
      msg='temporary file successfully saved'
      ierr=0
      return
992   msg='getlun error on temporary file'
      lun=0
      ierr=1
      return
999   msg='open error on temporary file'
      ierr=1
      close (lun)
      lun=0
      return
      end



      subroutine read_tempfile (lun,   msg,ierr)
c     read all velocity functions from a temporary file.
c     functions are added to the end of the functions in memory.
c     lun is closed and set to zero when finished.
c     active function will be the one that was active upon saving the file.

      implicit none
      integer lun,ierr,kount,i,j,p,k
      character*(*) msg
      include 'vel_data.inc'

      if (lun.eq.0) then
           msg='lun is zero'
           ierr=1
           return
      end if
      rewind lun
      read (lun) kount,ifun
      ifun=nfun+ifun
      do k=1,kount
           call alloc_last_velfun (msg,ierr)
           if (ierr.ne.0) go to 999
           i=nfun
           call offset_index_words (p,depths,point(i))
           read (lun) vfid(i),n(i),xbin(i),ybin(i),type(i),
     $        project(i),line(i),rdate(i),pdate(i),userid(i),comment(i),
     $        select(i),errmsg(i),raymsg(i)
           read (lun) (depths(p+j),times(p+j),thick(p+j),vrms(p+j),
     $        vav(p+j),vint(p+j),vnmo(p+j),offpick(p+j),j=1,n(i))
      end do
      ifun=min(nfun+1,nfunmax,max(ifun,1))
      msg='temporary file successfully read'
      ierr=0
999   close (lun)
      lun=0
      ifun=min(nfun+1,nfunmax,max(ifun,1))
      return
      end





