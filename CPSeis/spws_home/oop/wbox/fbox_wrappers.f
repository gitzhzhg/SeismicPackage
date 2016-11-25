
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

CCC                    file fbox_wrappers.f


ccc   This file contains public Fortran-callable routines which are
ccc   wrappers around the public C-language windowbox routines.

ccc   All routines in this file call private C-language routines in the
ccc   cbox_wrappers.cc file.

ccc   If character variables are to be passed BY VALUE to or from the
ccc   windowbox routines, they are converted to or from null-terminated
ccc   hollerith strings by calling convert_cc2hh or convert_hh2cc.

ccc   If character variables are to be passed BY ADDRESS to the
ccc   windowbox routines, their length is also passed, to enable the
ccc   windowbox routines to deal with the lack of null-termination.
ccc   The routines in cbox_wrappers.cc also deal with the VMS problem
ccc   (passing by descriptor).



cc----------------- verify fortran word sizes.
cc----------------- verify fortran word sizes.
cc----------------- verify fortran word sizes.

ccc   private subroutine (not to be called by users).
ccc   called when registering a set of linked arrays.
ccc   could also be called at other times.
ccc   needs to be called only once.

      subroutine fbox_verify_fortran_word_sizes
      implicit none
      integer          ivar(2)
      real             fvar(2)
      double precision dvar(2)

      call cbox_verify_fortran_integer (ivar(1),ivar(2))
      call cbox_verify_fortran_real    (fvar(1),fvar(2))
      call cbox_verify_fortran_double  (dvar(1),dvar(2))
      call cbox_verify_fortran_wrapup
      return
      end



c------------------set and get resources.
c------------------set and get resources.
c------------------set and get resources.


      subroutine fbox_set_maxrows (maxrows)
c     set maximum number of rows needed for arrays.
      implicit none
      integer maxrows
      call cbox_set_maxrows (maxrows)
      return
      end


      subroutine fbox_get_maxrows (maxrows)
c     get maximum number of rows needed for arrays.
      implicit none
      integer maxrows
      call cbox_get_maxrows (maxrows)
      return
      end



c     debug=0 prints nothing.
c     debug=1 prints a line just before calling boxtrap (except motion
c                events and printable-keypress events).
c     debug=2 also prints printable-keypress and expose details.
c     debug=3 also prints motion events.


      subroutine fbox_set_debug (debug)
      implicit none
      integer debug
      call cbox_set_debug (debug)
      return
      end
  

      subroutine fbox_get_debug (debug)
      implicit none
      integer debug
      call cbox_get_debug (debug)
      return
      end



      subroutine fbox_set_keymode (keymode)
      implicit none
      integer keymode
      call cbox_set_keymode (keymode)
      return
      end


      subroutine fbox_get_keymode (keymode)
      implicit none
      integer keymode
      call cbox_get_keymode (keymode)
      return
      end


      subroutine fbox_toggle_keymode
      implicit none
      call cbox_toggle_keymode
      return
      end



c------------------ display message at bottom of windowbox.
c------------------ display message at bottom of windowbox.
c------------------ display message at bottom of windowbox.


      subroutine fbox_maybe (msg)
      implicit none
      character*(*) msg
      integer buffer(60)
      call convert_cc2hh (msg,0,   buffer,60)
      call cbox_maybe (buffer)
      return
      end



      subroutine fbox_message (msg)
      implicit none
      character*(*) msg
      integer buffer(60)
      call convert_cc2hh (msg,0,   buffer,60)
      call cbox_message (buffer)
      return
      end



      subroutine fbox_immediate (msg)
c     immediately prints a message in the last line on the screen.
c     prints to the windowbox which has called a trap.
c     does nothing if not in a windowbox trap.
      implicit none
      character*(*) msg
      integer buffer(60)
      call convert_cc2hh (msg,0,   buffer,60)
      call cbox_immediate (buffer)
      return
      end



      subroutine fbox_messageline (ibox,msg)
c     immediately prints a message in the last line on the screen.
c     prints to the windowbox specified by box.
c     does nothing if box is not valid or not managed.
c     designed to be called from C.
      implicit none
      integer ibox
      character*(*) msg
      integer buffer(60)
      call convert_cc2hh (msg,0,   buffer,60)
      call cbox_messageline (ibox, buffer)
      return
      end




c----------------helpful functions.
c----------------helpful functions.
c----------------helpful functions.


      subroutine fbox_flush_buffer
      implicit none
      call cbox_flush_buffer
      return
      end


      subroutine fbox_ring_bell
      implicit none
      call cbox_ring_bell
      return
      end


      subroutine fbox_waste_time (n)
      implicit none
      integer n
      call cbox_waste_time (n)
      return
      end


      subroutine fbox_get_name (ibox,boxname)
      implicit none
      integer ibox
      character *(*) boxname
      integer buffer(60)
      call cbox_get_name (ibox,buffer)
      call convert_hh2cc (buffer,60,   boxname,0)
      return
      end


      subroutine fbox_get_num_boxes (totnumbox)
      implicit none
      integer totnumbox,cbox_get_num_boxes
      totnumbox=cbox_get_num_boxes()
      return
      end



c----------------manage or unmanage or destroy.
c----------------manage or unmanage or destroy.
c----------------manage or unmanage or destroy.


      subroutine fbox_manage (boxname)
      implicit none
      character *(*) boxname
      integer buffer(60)
      call convert_cc2hh (boxname,0,   buffer,60)
      call cbox_manage (buffer)
      return
      end


      subroutine fbox_unmanage (boxname)
      implicit none
      character *(*) boxname
      integer buffer(60)
      call convert_cc2hh (boxname,0,   buffer,60)
      call cbox_unmanage (buffer)
      return
      end


      subroutine fbox_destroy (boxname)
      implicit none
      character *(*) boxname
      integer buffer(60)
      call convert_cc2hh (boxname,0,   buffer,60)
      call cbox_destroy (buffer)
      return
      end


      subroutine fbox_manage_box (ibox)
      implicit none
      integer ibox
      call cbox_manage_box (ibox)
      return
      end


      subroutine fbox_unmanage_box (ibox)
      implicit none
      integer ibox
      call cbox_unmanage_box (ibox)
      return
      end


      subroutine fbox_destroy_box (ibox)
      implicit none
      integer ibox
      call cbox_destroy_box (ibox)
      return
      end



      logical function fbox_managed (ibox)
c     returns true if this windowbox is currently managed; false otherwise.
      implicit none
      integer ibox,value,cbox_managed
      value=cbox_managed(ibox)
      if (value.eq.0) fbox_managed=.FALSE.
      if (value.ne.0) fbox_managed=.TRUE.
      return
      end



c---------------------miscellaneous.
c---------------------miscellaneous.
c---------------------miscellaneous.


      subroutine fbox_set_focus (ibox,ident,index)
      implicit none
      integer ibox,ident,index
      call cbox_set_focus (ibox,ident,index)
      return
      end



      logical function fbox_in_trap ()
c     returns true if called from within a windowbox trap, false otherwise.
      implicit none
      integer value,cbox_in_trap
      value=cbox_in_trap()
      if (value.eq.0) fbox_in_trap=.FALSE.
      if (value.ne.0) fbox_in_trap=.TRUE.
      return
      end



      integer function fbox_is_visible (ibox,ident)
c     returns 1 if the given ident is currently visible.
c     returns 0 if the given ident is not currently visible.
      implicit none
      integer ibox,ident,value,cbox_is_visible
      value=cbox_is_visible(ibox,ident)
      fbox_is_visible=value
      return
      end



      subroutine fbox_update
c     this call will update all the fields of all windowboxes.
      implicit none
      call cbox_update
      return
      end
  
  
  
      subroutine fbox_event (ibox,endkey)
c     send pseudo-event (not thru server) to specified windowbox.
c     nothing happens if box is out of range.
      implicit none
      integer ibox,buffer(30)
      character*(*) endkey
      call convert_cc2hh (endkey,0,   buffer,30)
      call cbox_event (ibox,buffer)
      return
      end


      subroutine fbox_send_event (ibox,endkey)
c     send client message event (thru server) to specified windowbox.
c     nothing happens if box is out of range.
      implicit none
      integer ibox,buffer(30)
      character*(*) endkey
      call convert_cc2hh (endkey,0,   buffer,30)
      call cbox_send_event (ibox,buffer)
      return
      end


      subroutine fbox_send_message (ibox)
c     send client message event (thru server) to specified windowbox.
c     nothing happens if ibox is out of range.
      implicit none
      integer ibox
      call cbox_send_message (ibox)
      return
      end



c---------------register pairs of scalars.
c---------------register pairs of scalars.
c---------------register pairs of scalars.


      subroutine nbox_creg2 (id,pr,iswp,cvar,isw,irow,icol,nchar,ndec)
      implicit none
      integer id,iswp,isw,irow,icol,nchar,ndec
      character*(*) pr,cvar

      call cbox_creg2 (id,len(pr),len(cvar),irow,icol,nchar,ndec)
      call cbox_set_cpoint       (-id,pr)
      call cbox_set_spoint       (-id,iswp)
      call cbox_set_cpoint       ( id,cvar)
      call cbox_set_spoint       ( id,isw)

      return
      end


      subroutine fbox_creg2
     $               (trap, id,pr,iswp,cvar ,isw,irow,icol,nchar,ndec)
      implicit none
      integer id,iswp,isw,irow,icol,nchar,ndec,trap
      character*(*) pr,cvar
      external trap

      call nbox_creg2 (id,pr,iswp,cvar,isw,irow,icol,nchar,ndec)
      call cbox_set_generic_trap (-id,trap)
      call cbox_set_generic_trap ( id,trap)
      return
      end


      subroutine nbox_ireg2 (id,pr,iswp,ivar ,isw,irow,icol,nchar,ndec)
      implicit none
      integer id,iswp,isw,irow,icol,nchar,ndec,ivar
      character*(*) pr
      call cbox_ireg2 (id,len(pr),irow,icol,nchar,ndec)
      call cbox_set_cpoint       (-id,pr)
      call cbox_set_spoint       (-id,iswp)
      call cbox_set_ipoint       ( id,ivar)
      call cbox_set_spoint       ( id,isw)
      return
      end


      subroutine fbox_ireg2
     $               (trap, id,pr,iswp,ivar ,isw,irow,icol,nchar,ndec)
      implicit none
      integer id,iswp,isw,irow,icol,nchar,ndec,ivar,trap
      character*(*) pr
      external trap
      call nbox_ireg2 (id,pr,iswp,ivar,isw,irow,icol,nchar,ndec)
      call cbox_set_generic_trap (-id,trap)
      call cbox_set_generic_trap ( id,trap)
      return
      end


      subroutine nbox_freg2 (id,pr,iswp,fvar ,isw,irow,icol,nchar,ndec)
      implicit none
      integer id,iswp,isw,irow,icol,nchar,ndec
      real fvar
      character*(*) pr
      call cbox_freg2 (id,len(pr),irow,icol,nchar,ndec)
      call cbox_set_cpoint       (-id,pr)
      call cbox_set_spoint       (-id,iswp)
      call cbox_set_fpoint       ( id,fvar)
      call cbox_set_spoint       ( id,isw)
      return
      end


      subroutine fbox_freg2
     $               (trap, id,pr,iswp,fvar ,isw,irow,icol,nchar,ndec)
      implicit none
      integer id,iswp,isw,irow,icol,nchar,ndec,trap
      real fvar
      character*(*) pr
      external trap
      call nbox_freg2 (id,pr,iswp,fvar,isw,irow,icol,nchar,ndec)
      call cbox_set_generic_trap (-id,trap)
      call cbox_set_generic_trap ( id,trap)
      return
      end


      subroutine nbox_dreg2 (id,pr,iswp,dvar ,isw,irow,icol,nchar,ndec)
      implicit none
      integer id,iswp,isw,irow,icol,nchar,ndec
      double precision dvar
      character*(*) pr
      call cbox_dreg2 (id,len(pr),irow,icol,nchar,ndec)
      call cbox_set_cpoint       (-id,pr)
      call cbox_set_spoint       (-id,iswp)
      call cbox_set_dpoint       ( id,dvar)
      call cbox_set_spoint       ( id,isw)
      return
      end


      subroutine fbox_dreg2
     $               (trap, id,pr,iswp,dvar ,isw,irow,icol,nchar,ndec)
      implicit none
      integer id,iswp,isw,irow,icol,nchar,ndec,trap
      double precision dvar
      character*(*) pr
      external trap
      call nbox_dreg2 (id,pr,iswp,dvar,isw,irow,icol,nchar,ndec)
      call cbox_set_generic_trap (-id,trap)
      call cbox_set_generic_trap ( id,trap)
      return
      end


      subroutine nbox_ireg3(id,pr,iswp,ivar,isw,irow,icol,nchar,ndec)
      implicit none
      integer id,iswp,isw,irow,icol,nchar,ndec,ivar
      character*(*) pr
      call cbox_ireg3 (id,len(pr),irow,icol,nchar,ndec)
      call cbox_set_cpoint       (-id,pr)
      call cbox_set_spoint       (-id,iswp)
      call cbox_set_ipoint       ( id,ivar)
      call cbox_set_spoint       ( id,isw)
      return
      end


      subroutine fbox_ireg3
     $                 (trap, id,pr,iswp,ivar,isw,irow,icol,nchar,ndec)
      implicit none
      integer id,iswp,isw,irow,icol,nchar,ndec,ivar
      character*(*) pr
      external trap
      call nbox_ireg3 (id,pr,iswp,ivar,isw,irow,icol,nchar,ndec)
      call cbox_set_generic_trap (-id,trap)
      call cbox_set_generic_trap ( id,trap)
      return
      end





c---------------register linked arrays.
c---------------register linked arrays.
c---------------register linked arrays.


      subroutine fbox_rega (n,nmax,irow,numrow)
      implicit none
      integer n,nmax,irow,numrow
      call fbox_verify_fortran_word_sizes
      call cbox_rega (n,nmax,irow,numrow)
      return
      end



      subroutine fbox_crega (trap,id,pr,iswp,cvar ,isw,icol,nchar,ndec)
      implicit none
      external               trap
      integer                trap,id,   iswp,      isw,icol,nchar,ndec
      character*(*)                  pr,     cvar
      call nbox_crega (id,pr,iswp,cvar,isw,icol,nchar,ndec)
      call cbox_set_generic_trap (-id,trap)
      call cbox_set_generic_trap ( id,trap)
      return
      end



      subroutine nbox_crega(      id,pr,iswp,cvar ,isw,icol,nchar,ndec)
      implicit none
      integer                     id,   iswp,      isw,icol,nchar,ndec
      character*(*)                  pr,     cvar
      call cbox_crega (id,  len(pr),len(cvar), icol,nchar,ndec)
      call cbox_set_cpoint       (-id,pr)
      call cbox_set_spoint       (-id,iswp)
      call cbox_set_cpoint       ( id,cvar)
      call cbox_set_spoint       ( id,isw)
      return
      end




      subroutine fbox_irega (trap,id,pr,iswp,ivar ,isw,icol,nchar,ndec)
      implicit none
      external               trap
      integer                trap,id,   iswp,ivar ,isw,icol,nchar,ndec
      character*(*)                  pr
      call nbox_irega (id,pr,iswp,ivar,isw,icol,nchar,ndec)
      call cbox_set_generic_trap (-id,trap)
      call cbox_set_generic_trap ( id,trap)
      return
      end



      subroutine nbox_irega(      id,pr,iswp,ivar ,isw,icol,nchar,ndec)
      implicit none
      integer                     id,   iswp,ivar ,isw,icol,nchar,ndec
      character*(*)                  pr
      call cbox_irega (id,  len(pr),           icol,nchar,ndec)
      call cbox_set_cpoint       (-id,pr)
      call cbox_set_spoint       (-id,iswp)
      call cbox_set_ipoint       ( id,ivar)
      call cbox_set_spoint       ( id,isw)
      return
      end



      subroutine fbox_xrega (trap,id,pr,iswp,ivar ,isw,icol,nchar,ndec)
      implicit none
      external               trap
      integer                trap,id,   iswp,ivar ,isw,icol,nchar,ndec
      character*(*)                  pr
      call nbox_xrega (id,pr,iswp,ivar,isw,icol,nchar,ndec)
      call cbox_set_generic_trap (-id,trap)
      call cbox_set_generic_trap ( id,trap)
      return
      end



      subroutine nbox_xrega(      id,pr,iswp,ivar ,isw,icol,nchar,ndec)
      implicit none
      integer                     id,   iswp,ivar ,isw,icol,nchar,ndec
      character*(*)                  pr
      call cbox_irega (id,  len(pr),           icol,nchar,ndec)
      call cbox_set_cpoint         (-id,pr)
      call cbox_set_spoint         (-id,iswp)
      call cbox_set_index_behavior ( id)
      call cbox_set_spoint         ( id,isw)
      return
      end



      subroutine fbox_rrega (trap,id,pr,iswp,ivar ,isw,icol,nchar,ndec)
      implicit none
      external               trap
      integer                trap,id,   iswp,ivar ,isw,icol,nchar,ndec
      character*(*)                  pr
      call nbox_rrega (id,pr,iswp,ivar,isw,icol,nchar,ndec)
      call cbox_set_generic_trap (-id,trap)
      call cbox_set_generic_trap ( id,trap)
      return
      end



      subroutine nbox_rrega(      id,pr,iswp,ivar ,isw,icol,nchar,ndec)
      implicit none
      integer                     id,   iswp,ivar ,isw,icol,nchar,ndec
      character*(*)                  pr
      call cbox_irega (id,  len(pr),           icol,nchar,ndec)
      call cbox_set_cpoint  (-id,pr)
      call cbox_set_spoint  (-id,iswp)
      call cbox_set_rpoint  ( id,ivar)
      call cbox_set_spoint  ( id,isw)
      return
      end



      subroutine fbox_frega (trap,id,pr,iswp,fvar ,isw,icol,nchar,ndec)
      implicit none
      external               trap
      integer                trap,id,   iswp,      isw,icol,nchar,ndec
      character*(*)                  pr
      real                                   fvar
      call nbox_frega (id,pr,iswp,fvar,isw,icol,nchar,ndec)
      call cbox_set_generic_trap (-id,trap)
      call cbox_set_generic_trap ( id,trap)
      return
      end



      subroutine nbox_frega(      id,pr,iswp,fvar ,isw,icol,nchar,ndec)
      implicit none
      integer                     id,   iswp,      isw,icol,nchar,ndec
      character*(*)                  pr
      real                                   fvar
      call cbox_frega (id,  len(pr),           icol,nchar,ndec)
      call cbox_set_cpoint       (-id,pr)
      call cbox_set_spoint       (-id,iswp)
      call cbox_set_fpoint       ( id,fvar)
      call cbox_set_spoint       ( id,isw)
      return
      end



      subroutine fbox_drega (trap,id,pr,iswp,dvar ,isw,icol,nchar,ndec)
      implicit none
      external               trap
      integer                trap,id,   iswp,      isw,icol,nchar,ndec
      character*(*)                  pr
      double precision                       dvar
      call nbox_drega (id,pr,iswp,dvar,isw,icol,nchar,ndec)
      call cbox_set_generic_trap (-id,trap)
      call cbox_set_generic_trap ( id,trap)
      return
      end



      subroutine nbox_drega(      id,pr,iswp,dvar ,isw,icol,nchar,ndec)
      implicit none
      integer                     id,   iswp,      isw,icol,nchar,ndec
      character*(*)                  pr
      double precision                       dvar
      call cbox_drega (id,  len(pr),           icol,nchar,ndec)
      call cbox_set_cpoint       (-id,pr)
      call cbox_set_spoint       (-id,iswp)
      call cbox_set_dpoint       ( id,dvar)
      call cbox_set_spoint       ( id,isw)
      return
      end



c------------------register single scalars.
c------------------register single scalars.
c------------------register single scalars.


      subroutine nbox_mreg (pr,irow,icol)
      implicit none
      character*(*) pr
      integer irow,icol
      call cbox_mreg (len(pr),irow,icol)
      call cbox_set_cpoint       (0,pr)
      return
      end


      subroutine fbox_mreg (pr,irow,icol)
      implicit none
      integer irow,icol
      character*(*) pr
      call nbox_mreg (pr,irow,icol)
      return
      end


      subroutine fbox_creg (trap,id,cvar,isw,irow,icol,nchar,ndec)
      implicit none
      integer id,isw,irow,icol,nchar,ndec,trap
      character*(*) cvar
      external trap
      call nbox_creg (id,cvar,isw,irow,icol,nchar,ndec)
      call cbox_set_generic_trap (-id,trap)
      call cbox_set_generic_trap ( id,trap)
      return
      end

      subroutine nbox_creg (     id,cvar,isw,irow,icol,nchar,ndec)
      implicit none
      integer id,isw,irow,icol,nchar,ndec
      character*(*) cvar
      call cbox_creg (id,len(cvar),irow,icol,nchar,ndec)
      call cbox_set_cpoint    (id,cvar)
      call cbox_set_spoint    (id,isw)
      return
      end


      subroutine fbox_ireg (trap,id,ivar,isw,irow,icol,nchar,ndec)
      implicit none
      integer id,isw,irow,icol,nchar,ndec,ivar,trap
      external trap
      call nbox_ireg (id,ivar,isw,irow,icol,nchar,ndec)
      call cbox_set_generic_trap (-id,trap)
      call cbox_set_generic_trap ( id,trap)
      return
      end

      subroutine nbox_ireg (     id,ivar,isw,irow,icol,nchar,ndec)
      implicit none
      integer id,isw,irow,icol,nchar,ndec,ivar
      call cbox_ireg (id,irow,icol,nchar,ndec)
      call cbox_set_ipoint    (id,ivar)
      call cbox_set_spoint    (id,isw)
      return
      end


      subroutine fbox_freg (trap,id,fvar,isw,irow,icol,nchar,ndec)
      implicit none
      integer id,isw,irow,icol,nchar,ndec,trap
      real fvar
      external trap
      call nbox_freg (id,fvar,isw,irow,icol,nchar,ndec)
      call cbox_set_generic_trap (-id,trap)
      call cbox_set_generic_trap ( id,trap)
      return
      end

      subroutine nbox_freg (     id,fvar,isw,irow,icol,nchar,ndec)
      implicit none
      integer id,isw,irow,icol,nchar,ndec
      real fvar
      call cbox_freg (id,irow,icol,nchar,ndec)
      call cbox_set_fpoint    (id,fvar)
      call cbox_set_spoint    (id,isw)
      return
      end


      subroutine fbox_dreg (trap,id,dvar,isw,irow,icol,nchar,ndec)
      implicit none
      integer id,isw,irow,icol,nchar,ndec,trap
      double precision dvar
      external trap
      call nbox_dreg (id,dvar,isw,irow,icol,nchar,ndec)
      call cbox_set_generic_trap (-id,trap)
      call cbox_set_generic_trap ( id,trap)
      return
      end

      subroutine nbox_dreg (     id,dvar,isw,irow,icol,nchar,ndec)
      implicit none
      integer id,isw,irow,icol,nchar,ndec
      double precision dvar
      call cbox_dreg (id,irow,icol,nchar,ndec)
      call cbox_set_dpoint    (id,dvar)
      call cbox_set_spoint    (id,isw)
      return
      end




c--------------- re-register pointers.
c--------------- re-register pointers.
c--------------- re-register pointers.


      subroutine fbox_nnewreg (ibox,ident,n)
      implicit none
      integer ibox,ident,n
      call cbox_nnewreg (ibox,ident,n)
      return
      end


      subroutine fbox_cnewreg (ibox,ident,cvar)
      implicit none
      integer ibox,ident
      character*(*) cvar
      call cbox_cnewreg (ibox,ident,cvar)
      return
      end


      subroutine fbox_inewreg (ibox,ident,ivar)
      implicit none
      integer ibox,ident,ivar
      call cbox_inewreg (ibox,ident,ivar)
      return
      end


      subroutine fbox_fnewreg (ibox,ident,fvar)
      implicit none
      integer ibox,ident
      real fvar
      call cbox_fnewreg (ibox,ident,fvar)
      return
      end


      subroutine fbox_dnewreg (ibox,ident,dvar)
      implicit none
      integer ibox,ident
      double precision dvar
      call cbox_dnewreg (ibox,ident,dvar)
      return
      end




c--------------------restore previous value.
c--------------------restore previous value.
c--------------------restore previous value.

      subroutine restore_previous_value (ibox,ident,index,istat)
      implicit none
      integer ibox,ident,index,istat
      call cbox_restore_previous_value (ibox,ident,index,istat)
      return
      end



c-----------------------fbox_boxtrap11a.
c-----------------------fbox_boxtrap11a.
c-----------------------fbox_boxtrap11a.

ccc   private subroutine (not to be called by users).
ccc   interface to Fortran standard traps.
ccc   called from cbox_fortran_trap_handler in cbox_wrappers.cc.
ccc   the trap is of type WboxFortranTrap.

      subroutine fbox_boxtrap11a
     $                 (trap,ibox,ident,index,tttt,nread,eeeeee)
      implicit none
      integer ibox,ident,index,tttt(*),nread,eeeeee(*)
      external trap
      character*101 text
      character*8 endkey

      call convert_hh2cc (tttt,0,   text,0)
      call convert_hh2cc (eeeeee,0,   endkey,0)

      call trap (ibox,ident,index,   text,nread,endkey)

      call convert_cc2hh (endkey,0,   eeeeee,-5)
      return
      end




c-----------------------fbox_boxtrap22a.
c-----------------------fbox_boxtrap22a.
c-----------------------fbox_boxtrap22a.

ccc   private subroutine (not to be called by users).
ccc   interface to Fortran EZED traps.
ccc   called from cbox_fortran_trap_handler in cbox_wrappers.cc.
ccc   the trap is of type WboxEzedTrap.

      subroutine fbox_boxtrap22a
     $                    (trap,ident,index,tttt,nread,eeeeee)
      implicit none
      integer ident,index,tttt(*),nread,eeeeee(*)
      external trap
      character*8 endkey
      integer identkeep,indexkeep,iexsw

      identkeep=ident
      indexkeep=index
      iexsw=0

      call trap (tttt,nread,index,   ident,iexsw)

      call convert_hh2cc (eeeeee,0,   endkey,0)
      if (ident.ne.identkeep) endkey=' '
      if (index.ne.indexkeep) endkey=' '
      if (iexsw.eq.2) then
           endkey='JUMP'
      else if (iexsw.eq.3) then
           ident=identkeep
           index=indexkeep
           endkey=' '
      end if
      call convert_cc2hh (endkey,0,   eeeeee,-5)
      return
      end




c------------------------ end.
c------------------------ end.
c------------------------ end.

