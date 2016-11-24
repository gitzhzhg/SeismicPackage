* Copyright (c) Colorado School of Mines, 2004.
* All rights reserved.

c23456789012345678901234567890123456789012345678901234567890123456789012
      subroutine util_setr(n,x,x0)
      implicit none
      integer  i,n
      real     x(1),x0
      do i = 1 , n
        x(i) = x0
      enddo
      return
      end

c23456789012345678901234567890123456789012345678901234567890123456789012
      subroutine util_seti(n,x,x0)
      implicit none
      integer  i,n
      integer  x(1),x0
      do i = 1 , n
        x(i) = x0
      enddo
      return
      end

c23456789012345678901234567890123456789012345678901234567890123456789012
      subroutine util_lini(n,x,x0,dx)
      implicit none
      integer  i,n
      integer  x(1),x0,dx
      do i = 1 , n
        x(i) = x0 + (i - 1 ) * dx
      enddo
      return
      end
							   
c23456789012345678901234567890123456789012345678901234567890123456789012
      subroutine util_invert(n,x)
      implicit none
      integer  i,n
      real     x(1)
      do i = 1 , n
      if (x(i) .ne. 0.) x(i) = 1. / x(i)
      enddo
      return
      end

c23456789012345678901234567890123456789012345678901234567890123456789012
      subroutine util_min_max(x_min,x_max,n,x)
      implicit none
      integer  i,n
      real     x(1),x_min,x_max
      x_min = 0.
      x_max = 0.
      if (n .eq. 0) return

      x_min = x(1)
      x_max = x(1)
      do i = 1 , n
        x_min = min(x_min,x(i))
        x_max = max(x_max,x(i))
      enddo
      return
      end

c23456789012345678901234567890123456789012345678901234567890123456789012
      subroutine util_min_max_i(x_min,x_max,n,x)
      implicit none
      integer  i,n
      integer  x(1),x_min,x_max
      x_min = 0
      x_max = 0
      if (n .eq. 0) return

      x_min = x(1)
      x_max = x(1)
      do i = 1 , n 
        x_min = min(x_min,x(i))
        x_max = max(x_max,x(i))
      enddo   
      return  
      end

c23456789012345678901234567890123456789012345678901234567890123456789012
      subroutine util_open_file(
     1i_file,file,status,form,n_rec,i_err)
      implicit  none
      integer   i_file
      character file*(*),status*(*),form*(*)
      integer   n_rec,i_err

      i_err = 0

      call util_get_lun(i_file,i_err)
      if (i_err .ne. 0) goto 998

      if (form .eq. 'direct' .or. form .eq. 'DIRECT') then

        open(i_file,file=file,status=status,form=form,err=999)

      else

        open(i_file,file=file,status=status,form=form
     1,recl=n_rec,err=999)

      endif

      return

  998 continue
      print'(/,'' error in util_open_file getting unit number'')'
      goto 999

  999 continue
      print'(/,'' error in util_open_file''
     1,/,'' i_file='',i5,'' file='',a
     1,/,'' status='',a16,'' form='',a16,'' recl='',i5)'
     1,i_file,file,status,form,n_rec
      i_err = -1
      return
      end


c23456789012345678901234567890123456789012345678901234567890123456789012
      subroutine util_get_file_name(prompt,file,ext)
      character *(*) prompt,file,ext
      character crd80*80
    1 continue

      crd80 = ' '
      call util_add_character(prompt,crd80)
      call util_add_character(' -default=',crd80)
      call util_add_character(file,crd80)
      call util_add_character(' ext=',crd80)
      call util_add_character(ext,crd80)
      print'(a)',crd80
      crd80 = ' '
      read (5,'(a)',err=1) crd80
      if (crd80 .ne. ' ') read(crd80,'(a)')file
      if (file .eq. ' ') goto 1
      call util_add_ext(file,ext)

      return
      end

c23456789012345678901234567890123456789012345678901234567890123456789012
      subroutine util_add_character(c1,c2)
c  add c1 to end of c2
      integer   util_r
      character c1*(*),c2*(*)
      integer   lc1,lc2,n

      lc1 = util_r(c1)
      lc2 = util_r(c2)
      n = min(lc1,len(c2)-lc2)
      write(c2(lc2+1:lc2+n),'(a)')c1(1:n)

      return
      end

c23456789012345678901234567890123456789012345678901234567890123456789012
      subroutine util_add_ext(fname,ext)
      character*(*) fname,ext
      ientry = 1
      go to 1
      entry util_add_extrp (fname,ext)
      entry util_add_ext_replace (fname,ext)
      ientry = 2
   1  iclose = index (fname,']')
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
      subroutine util_work(i_work,m_work,j_work,n_work)
c  assign work space within an array
c
c  i_work = current pointer within work array
c  m_work = total memory in work array
c  j_work = pointer for this memory allocation
c  n_work = amount of memory to allocate in this call
c
c  i_work and m_work are initialized by util_wors and modified by
c  other routines, they should never be altered by the user outside
c  of util_wor...
      implicit none

      integer i_work,m_work,j_work,n_work,i_err

      j_work = i_work
      i_work = i_work + n_work

      return
	
c23456789012345678901234567890123456789012345678901234567890123456789012
      entry util_wors(i_work,m_work,n_work)
c  initalize the number of word savaliable to n_work
c  and the pointer to 1
      i_work = 1
      m_work = n_work

      return

c23456789012345678901234567890123456789012345678901234567890123456789012
      entry util_worl(i_work,m_work,n_work)
c  return the number of words remaining
      n_work = m_work - i_work + 1

      return     

c23456789012345678901234567890123456789012345678901234567890123456789012
      entry util_woru(i_work,m_work,n_work)
c  return the number of word used
      n_work = i_work - 1

      return

c23456789012345678901234567890123456789012345678901234567890123456789012
      entry util_worc(i_work,m_work,i_err)
c  check if we have used more memory than allowed

      i_err = 0
      if (i_work-1 .gt. m_work) then
        print'(/,'' error in work used='',i8,'' have='',i8)'
     1,i_work-1,m_work
        i_err = -1
      endif    ! if (i_work-1 .gt. m_work) then

      return
      end
	

c23456789012345678901234567890123456789012345678901234567890123456789012
      subroutine util_get_lun(lun,i_err)
      implicit none
      integer lun,i,i_err
      logical quest
      integer lstart,lstop
      parameter (lstart=20,lstop=99)
      integer last,list(lstop)
      save    last,list
      data    last,list/lstop,lstop*0/

      i_err = 0
      do i=lstart,lstop
           last=last+1
           if (last.gt.lstop) last=lstart
           inquire (last,named=quest,err=999)
           if (.not.quest) then
                list(last)=1
                lun=last
                return
           end if
      end do

999   print *, 'util_get_lun failed'
      i_err = -1
      return

c23456789012345678901234567890123456789012345678901234567890123456789012
      entry util_get_lun_s
      do i=lstart,lstop
           if (list(i).gt.0) then
                close (i)
                list(i)=0
           end if
      end do
      last=lstop
      return
      end

c23456789012345678901234567890123456789012345678901234567890123456789012
      subroutine util_caps(c_inp,c_out)
c capitalize an ascii string
      implicit  none

      character c_inp*(*),c_out*(*)

      character c_tmp*132
      character small*26,big*26
      integer   nc,ic1,ic2
      data      small/'abcdefghijklmnopqrstuvwxyz'/
      data      big/'ABCDEFGHIJKLMNOPQRSTUVWXYZ'/

      c_tmp = c_inp
      c_out = ' '
      c_out = c_tmp
      nc = len(c_out)

      do ic1 = 1 , nc

        do ic2 = 1 , 26

          if (c_out(ic1:ic1) .eq. small(ic2:ic2)) then

            c_out(ic1:ic1) = big(ic2:ic2)
            goto 1

          endif    ! if (c_out(ic1:ic1).eq.small(ic2:ic2)) then

        enddo    ! do ic2 = 1 , 26

    1   continue

      enddo    ! do ic1 = 1 , nc

      return
      end

c23456789012345678901234567890123456789012345678901234567890123456789012
      subroutine util_dcod(value, card)
      real value
      character*(*) card
c decode a real number, perhaps in scientific notation.
cndxi dcod( i
      integer n,i,n1,n2
      logical ee
      real r1,r2
      character*20 c20
c shift to left
      call util_clft(card)
c find out if in exponential notation
      n = len(card)
      ee = .false.
      do 100 i=1,n
        ee = (card(i:i).eq.'e'.or.card(i:i).eq.'E')
        if(ee) then
          n1 = i - 1
          n2 = i + 1
          goto 200
        endif
  100 continue
  200 continue
      if(.not.ee) then
c...        no exponent
        c20 = ' '
        c20 = card
        call util_cadp(c20,20)
        read(c20,1401,err=1301) value
      else
c...        have an exponent, read mantissa first
        c20 = ' '
        c20 = card(:n1)
        call util_cadp(c20,20)
        read(c20,1401,err=1301) r1
c...        read exponent second
        c20 = ' '
        c20 = card(n2:)
        call util_cadp(c20,20)
        read(c20,1401,err=1301) r2
        value = r1 * exp(log(10.)*r2)
      endif
      return
 1301 continue
c this is commoned because util_heyu() is commoned
c       call util_heyu('dcod: problem decoding card')
c       call util_heyu(card)
      return
 1401 format(f20.7)
      end

c23456789012345678901234567890123456789012345678901234567890123456789012
      subroutine util_lenl(n, c)
      character*(*) c
      integer n
c find the length of a string before first blank, from left.
cndxc lenl( c
      integer nc,i
      nc = len(c)
      do 100 i=1,nc
        n = i
        if(c(n:n).eq.' ') goto 200
  100 continue
      n = nc + 1
c break
  200 continue
      n = n - 1
      return
      end

c23456789012345678901234567890123456789012345678901234567890123456789012
      subroutine util_lenp(n, c)
      character*(*) c
      integer n
c find the length of a string before first period, from right
cndxc lenp( c
c stop at first bracket ]. if no period stop before first blank.
      integer nc,ncb,i
      nc = len(c)
      ncb = nc + 1
      do 100 i=nc,1,-1
        n = i
        if(c(n:n).eq.' ') ncb = n
        if(c(n:n).eq.']') goto 150
        if(c(n:n).eq.'.') goto 200
  100 continue
  150 continue
      n = ncb
  200 continue
      n = n - 1
      return
      end

c23456789012345678901234567890123456789012345678901234567890123456789012
      subroutine util_lenr(n, c,nc)
      character*(*) c
      integer n,nc
c find the lenth of a string before first blank, from right
cndxc lenr( c
      integer i
      do 100 i=nc,1,-1
        n = i
        if(c(n:n).ne.' ') goto 200
  100 continue
      n = 1
  200 continue
      return
      end

c23456789012345678901234567890123456789012345678901234567890123456789012
      subroutine util_decode_value_1(value,n_value, card,name)
      character*(*) value,card,name
      integer n_value
c get character value of a variable 'name' from card.  name=value.
cndxc decode_value_1( c
      character ccard*160,cname*80
      integer ic1,ic2,in1,nname,jc1,ncard,igap,ibegin
      logical lquote,util_spac
c
      ncard = len(card)

c  modify so # sign comments out following characters dwh 05-03-94
      do ic1 = 1 , ncard
        if (card(ic1:ic1) .eq. '#') then
          ncard = ic1 - 1
          goto 301
        endif    ! if (card(ic1:ic1) .eq. '#') then
      enddo    ! do ic1 = 1 , ncard
  301 continue

c capitalize name and card
      call util_caps(card, ccard)
      call util_caps(name, cname)
      call util_clft(cname)
      n_value = 0
      value = ' '
      call util_lenl(nname, name)
      nname = min(80, nname)
      do 300 ic1=1,ncard
        if(card(ic1:ic1).eq.'=') then
c count igap, the number of intervening blanks
          igap = 0
   50     continue
          ic2 = ic1 - igap - 1
          if(ic2.lt.1) goto 300
          if(ccard(ic2:ic2).eq.' ') then
            igap = igap + 1
            goto 50
          endif
c see if string matches before blanks
          do 100 in1=1,nname
            ic2 = ic1 - nname - 1 + in1 - igap
              if(ic2.lt.1) goto 300
              if(ccard(ic2:ic2).ne.cname(in1:in1)) goto 300
  100     continue
c check to see if cname is not the last part of another name.
          ic2 = ic1 - nname - 1 - igap
          if(ic2.gt.0) then
            if (.not.util_spac(card(ic2:ic2))) goto 300
c            if(      ccard(ic2:ic2).ne.' '.and.
c     &               ccard(ic2:ic2).ne.','.and.
c     &               ccard(ic2:ic2).ne.';'.and.
c     &               ccard(ic2:ic2).ne.'('         ) goto 300
          endif
c have a match ; ignore first blanks after equals sign
          ibegin = ic1 + 1
  150     continue
            if(card(ibegin:ibegin).eq.' ') then
              ibegin = ibegin + 1
              if(ibegin.gt.ncard) goto 300
              goto 150
            endif
c check to see if string is in quotes
          lquote = (card(ibegin:ibegin).eq.'"')
          if(lquote) ibegin = ibegin + 1
c start loop to set output string
          value = ' '
          jc1 = ibegin - 1
          n_value = 1
  200     continue
            ic2 = jc1 + n_value
            if(ic2.le.ncard) then
              if( ((.not.lquote).and.(.not.util_spac(card(ic2:ic2))))
     &      .or. (lquote.and.card(ic2:ic2).ne.'"') ) then
c              if( ((.not.lquote).and.
c     &           card(ic2:ic2).ne.' '.and.
c     &           card(ic2:ic2).ne.')'.and.
c     &           card(ic2:ic2).ne.';'.and.
c     &           card(ic2:ic2).ne.',').or.
c     &           (lquote.and.card(ic2:ic2).ne.'"') ) then
                value(n_value:n_value) = card(ic2:ic2)
                n_value = n_value + 1
                goto 200
              endif
            endif
          n_value = n_value - 1
        endif
  300 continue
cc    write(6,*) 'name value ',cname(:nname),' ',value(:n_value), n_value
      return
      end

c23456789012345678901234567890123456789012345678901234567890123456789012
      subroutine util_decode_value_2(value,n_value, name,i_file)
      character*(*) value,name
      integer n_value,i_file
c save is changed to integer!
c     save mlines,nforl
      integer mlines,nforl
c get the value of a variable 'name' from device i_file
c  use format   'name=value',  returned value is a string.
c  string is delimited on right by blank, comma, or parenthesis.
c  keep last assignment in file
c first open i_file with
      character card*160, val*80
c save is changed to integer!
c     integer nname,nval,nlines,jlines,mlines,iforl,jforl,nforl
      integer nname,nval,nlines,jlines,iforl,jforl
      integer i_rewind
      data mlines,nforl/200,1/
      data    i_rewind/0/
      if (i_rewind .ne. 1) rewind(i_file)
      n_value = 0
      nlines = 1
      iforl = 0
  100 continue
        read(i_file,1401,end=200,err=1301) card
C      PRINT'('' decode_value_2 CARD='',A60)',CARD(1:MIN(LEN(CARD),60))
        call util_decode_value_1(val,nval, card,name)
        if(nval.gt.0) then
	  value = val(:nval)
	  n_value = nval
	  iforl = iforl + 1
	  if (iforl .eq. nforl) goto 200
        endif
      nlines = nlines + 1
      if(nlines.lt.mlines) goto 100
  200 continue
        call util_lenl(nname,name)
cc        write(6,*) 'name value ',name(:nname),' ',value(:n_value)
      return
 1301 continue
c      write(6,*) 'error reading device ',i_file
      return
 1401 format(160a)
      entry util_plin(jlines)
c set the number of lines to read in decode_value_2
      mlines = jlines
      return
      entry util_glin(jlines)
c return the number of lines to read in decode_value_2
      jlines = mlines
      return
      entry util_porl(jforl)
c  se the number of occurences to search for
      nforl = jforl
      return
      entry util_gorl(jforl)
c  return the number of occurences to search for
      jforl = nforl
      return

      entry util_set_rewind
      i_rewind = 0
      return

      entry util_set_no_rewind
      i_rewind = 1
      return

      end

c23456789012345678901234567890123456789012345678901234567890123456789012
      subroutine util_decode_r(rvalue, name)
      real rvalue
      integer ivalue,nchar
      character*(*) name,cvalue
c get real value from file; "name=rvalue" oepn set i_file, close
      integer n_value,i_file
      real r
      character*80 value
c     character*20 c20
      data i_file/-1/
        if(i_file.lt.0) return
cc this is commoned because util_decode_value_2() is commoned
        call util_decode_value_2(value,n_value, name,i_file)
        if(n_value.ge.1) then
          call util_dcod(rvalue, value(:n_value))
        endif
C      PRINT'('' GVR VALUE='',G16.3,'' NAME='',A16)',RVALUE,NAME
      return
c23456789012345678901234567890123456789012345678901234567890123456789012
      entry util_decode_i(ivalue, name)
c get an integer value from a parameter file
        if(i_file.lt.0) return
cc this is commoned because util_decode_value_2() is commoned
        call util_decode_value_2(value,n_value, name,i_file)
        if(n_value.ge.1) then
          call util_dcod(r,value(:n_value))
          ivalue = int(r+0.499)
        endif
C      PRINT'('' GVI IVALUE='',I8,'' NAME='',A16)',IVALUE,NAME
      return
c23456789012345678901234567890123456789012345678901234567890123456789012
      entry util_decode_c(cvalue,nchar, name)
c get a character string from a parameter file
        if(i_file.lt.0) then
          nchar = 0
          return
        endif
cc this is commoned because util_decode_value_2() is commoned
        call util_decode_value_2(value,n_value, name,i_file)
        if(n_value.ge.1) then
          cvalue = ' '
          cvalue = value(:n_value)
          nchar = n_value
        endif
C      PRINT'('' GVC VALUE='',A16,'' NAME='',A16)',CVALUE,NAME
      return

c23456789012345678901234567890123456789012345678901234567890123456789012
      entry util_get_device(j_file)
c call this to find out what the device (unit) number is open
        j_file = i_file
      return

c23456789012345678901234567890123456789012345678901234567890123456789012
      entry util_put_device(j_file)
c call this to find out what the device (unit) number is open
        i_file = j_file
      return

      end

c23456789012345678901234567890123456789012345678901234567890123456789012
      subroutine util_cadp(c, nc)
      character*(*) c
      integer nc
c add a period to a string, so that resembles floating point number.
cndxc cadp( c
      integer i
      character*80 card
      card = ' '
      card = c(:nc)
      call util_clft(card)
      c(:nc) = card(:nc)
      do 100 i=1,nc
        if(c(i:i).eq.' '.or.c(i:i).eq.'.') goto 200
  100 continue
      nc = nc + 1
      i = nc
  200 continue
      c(i:i) = '.'
      return
      end

c23456789012345678901234567890123456789012345678901234567890123456789012
      subroutine util_clft(c)
      character*(*) c
c get rid of leading blanks, only one word moved, up to first blank.
cndxc clft( c
      integer i,i1,n,i2,nall,nc,ncrest
      nc = len(c)
      do 100 i=1,nc
        if(c(i:i).ne.' ') goto 200
  100 continue
      return
  200 continue
      if(i.eq.1) return
      ncrest = nc-i+1
      call util_lenr(n, c(i:nc),ncrest)
      nall = i+n-1
      do 300 i1=1,nc
        i2 = i1 + i - 1
        if(i1.le.n) then
          c(i1:i1) = c(i2:i2)
        else
          c(i1:i1) = ' '
        endif
  300 continue
      return
      end

c23456789012345678901234567890123456789012345678901234567890123456789012
      function util_invert_1(x)
      implicit none
      real util_invert_1
      real x
c     real eps
c     data eps/1.e-10/
      util_invert_1 = 0.
      if (x .ne. 0.) util_invert_1 = 1. / x

c      if (abs(x) .ge. eps) then

c        util_invert_1 = 1. / x

c      else    ! if (abs(x) .ge. eps) then

c        util_invert_1 = 0.

c      endif    ! if (abs(x) .ge. eps) then

      return
      end

c23456789012345678901234567890123456789012345678901234567890123456789012
      integer function util_len_r(str)
c  find the last non blank character
      character str*(*)
      do j = len(str) , 1 , -1
        util_len_r = j
        if (str(j:j) .ne. ' ') return
      enddo
      util_r = 0
      return
      end

c23456789012345678901234567890123456789012345678901234567890123456789012
      logical function util_spac(card)
c  is card oone of the delimiters
      character *1 card
      parameter (mc=3)
      character c(mc)*1
      data c/' ',',','('/
      util_spac = .false.
      do 1 i = 1 , mc
        util_spac = util_spac .or. (card .eq. c(i))
    1 continue
      return
      end

c23456789012345678901234567890123456789012345678901234567890123456789012
      real function util_rd(x)
      implicit none
      real     x
      util_rd = x * 90. / asin(1.)
      return
      end

c23456789012345678901234567890123456789012345678901234567890123456789012
      real function util_dr(x)
      implicit none
      real     x
      util_dr = x * asin(1.) / 90.
      return
      end

c23456789012345678901234567890123456789012345678901234567890123456789012
      integer function util_fetch_i(name,value)
      implicit  none

      character name*(*)
      integer   value

      integer   x

      integer i_call
      data    i_call/0/
      i_call = i_call + 1

      x = -999

        call util_decode_i(x,name)

      if (x .ne. -999) then
        util_fetch_i = 1
        value = x
      else
        util_fetch_i = 0
      endif

      return
      end

c23456789012345678901234567890123456789012345678901234567890123456789012
      integer function util_fetch_r(name,value)
      implicit  none

      character name*(*)
      real      value

      real      x

      integer i_call
      data    i_call/0/
      i_call = i_call + 1

      x = -999.

        call util_decode_r(x,name)

      if (abs(x+999.) .gt. .01) then
        util_fetch_r = 1
        value = x
      else
        util_fetch_r = 0
      endif

      return
      end

c23456789012345678901234567890123456789012345678901234567890123456789012
      integer function util_fetch_c(name,value)
      implicit  none

      character name*(*)
      character value*(*)

      character x*132
      integer nx

      integer i_call
      data    i_call/0/
      i_call = i_call + 1

      x = '999'

        call util_decode_c(x,nx,name)

      if (x(1:3) .ne. '999') then
        util_fetch_c = 1
        value = x
      else
        util_fetch_c = 0
      endif

      return
      end
c23456789012345678901234567890123456789012345678901234567890123456789012
      integer function util_r(str)
c  find the last non blank character
      character str*(*)
      do j = len(str) , 1 , -1
        util_r = j
        if (str(j:j) .ne. ' ') return
      enddo
      util_r = 0
      return
      end   

