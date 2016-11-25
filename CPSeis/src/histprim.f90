!<CPS_v1 type="PRIMITIVE"/>
!!------------------------------- histprim.f90 -------------------------------!!
!!------------------------------- histprim.f90 -------------------------------!!
!!------------------------------- histprim.f90 -------------------------------!!
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
!                        C P S   P R I M I T I V E
!
! Name       : HISTPRIM 
! Category   : main_prog
! Written    : 2001-08-03   by: Karen Goodger
! Revised    : 2006-06-12   by: B. Menger
! Maturity   : production
! Purpose    : Primitives common to history routines.
! Portability: No known limitations.
!
!-------------------------------------------------------------------------------
!</brief_doc>


!<descript_doc>
!-------------------------------------------------------------------------------
!                         GENERAL DESCRIPTION            
!
!    Histprim is a collection of primitives which are common to history
!    routines, such as manhist and atblk.
!-------------------------------------------------------------------------------
!</descript_doc>


!<calling_doc>
!-------------------------------------------------------------------------------
!                      INPUT AND OUTPUT ARGUMENTS        
!
! For each subroutine or function documented below, each argument is
! flagged as follows:
!   i = intent(in)    = value required upon INPUT.
!   o = intent(out)   = value set by the routine upon OUTPUT.
!   b = intent(inout) = value BOTH required upon input and changed upon output.
!
! Optional arguments are also flagged as follows:
!   opt = this argument is optional.
!
!-------------------------------------------------------------------------------
!                          CALLING SEQUENCE               
!
!                            i     o    i    i        i
!         call histprim_gans(parm, ans, buf, cardnum, nc)
!
!          Looks for the name iparm in the history file buffer.
!          Returns the information following iparm between the
!          equal sign and comma or end of card.
!
! character(len=*)    parm   = The parameter to look for in ibuf
! character(len=*)    ans    = The answer returned.
! character(len=*)    buf(:) = The history file buffer
! integer            cardnum = The card number to begin looking in buf
! integer             nc     = The total number of cards in buf
!
!
!
!
!
!                             i     i     o    o
!         call histprim_gsprm(pnum, card, str, len) 
! 
!          Get sequential parameter number pnum from a text card where 
!           parameters are separated by commas
!
! integer            pnum    = The parameter number to retrieve
! character(len=*)   card    = The card of text with parameters separated by
!                              commas.
! character(len=*)   str     = The parameter returned.
! integer            len     = The number of character in str, returned.
!
!
!
!
!                                   i    o
!        call histprim_process_name(card,process_name)
!
!          Pulls the process name from card on a version 1.0 or greater history
!
! character(len=*)   card         = A history card
! character(len=*)   process_name = process_name returned
!
! 
!
!
!-------------------------------------------------------------------------------
!</calling_doc>


!<advice_doc>
!-------------------------------------------------------------------------------
!                            ADVICE FOR USERS             
!
!
!-------------------------------------------------------------------------------
!</advice_doc>


!<history_doc>
!-------------------------------------------------------------------------------
!                             REVISION HISTORY              
!
!     Date        Author         Description
!     ----        ------         -----------
!  3. 2006-06-12  B. Menger      Removed Unused Variables.
!  2. 2002-08-26  Karen Goodger  Add routine histprim_validate_parameter.
!  1. 2001-08-27  Karen Goodger  Initial version.
!
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

!-------------------------------------------------------------------------------




!!-------------------------- start of module ------------------------------!!
!!-------------------------- start of module ------------------------------!!
!!-------------------------- start of module ------------------------------!!


      module histprim_module

      implicit none

      public

      character(len=100),public,save :: HISTPRIM_IDENT = &
'$Id: histprim.f90,v 1.3 2006/06/12 13:03:51 Menger prod sps $'




!!------------------------------ data -------------------------------------!!
!!------------------------------ data -------------------------------------!!
!!------------------------------ data -------------------------------------!!


      contains



!!----------------------------- subroutines -------------------------------!!
!!----------------------------- subroutines -------------------------------!!
!!----------------------------- subroutines -------------------------------!!

      subroutine histprim_gans(iparm, ians, ibuf, icard, nc) 
      implicit none
!-----------------------------------------------
!   d u m m y   a r g u m e n t s
!-----------------------------------------------
      integer , intent(in) :: icard 
      integer , intent(in) :: nc 
      character(len=*),intent(in) :: iparm
      character(len=*),intent(out) :: ians
      character(len=*),intent(in) :: ibuf(*)
!-----------------------------------------------
!   l o c a l   v a r i a b l e s
!-----------------------------------------------
      integer :: n, i, k,k2, ndx, icol, i1, i2 
      integer :: istat,nextcard
      logical :: commafound

!-----------------------------------------------
!
!
!          looks for the name iparm in the history file buffer
!          returns the information following iparm between the
!          equal sign and comma or end of card
!
!
!          iparm = the parameter to look for in ibuf
!          ians  = the answer returned
!          ibuf  = the history file buffer
!         icard  = the card to begin looking in ibuf
!            nc  = total number of cards in ibuf
!
!
!
!
      ians=' '
      n=len_trim(iparm)
      nextcard=icard
 100  continue
      do i = nextcard, nc 
        k = index(ibuf(i),iparm(1:n)) 
        if (k /= 0) go to 150 
      end do 
      return  
!
  150 continue 
!          Insure this string is a valid parameter name format
      call histprim_validate_parameter(iparm,n,k,ibuf(i),istat)
      if(istat.ne.0)then
        nextcard=nextcard+1
        go to 100
      endif
      ndx = i 
      icol = k 
      icol = index(ibuf(ndx)(icol+1:),'=') + icol 
      i1 = icol + 1 
      k = index(ibuf(ndx)(icol+1:),',') + icol 
      i2 = k - 1 
      if (k == icol)then
!               did not find a comma - set i2 to last non blank
         i2 = len_trim(ibuf(ndx))
         commafound=.false.
      else
         commafound=.true.
      endif
!
      i2 = min0(80,i2) 

!
      ians = ibuf(ndx)(i1:i2) 
      if(ians.eq.' '.and..not.commafound)then
!             look for answer on next card
        ndx=ndx+1
        k=index(ibuf(ndx),',')
        if(k.eq.0)return
        k2=index(ibuf(ndx),'=')
        if(k2.eq.0.or.k.lt.k2)ians=ibuf(ndx)(1:k)        
      endif
!
!
!
      return  
      end subroutine histprim_gans 

      subroutine histprim_gsprm(ipnum, kard, str, len) 
      implicit none
!-----------------------------------------------
!   d u m m y   a r g u m e n t s
!-----------------------------------------------
      integer , intent(in) :: ipnum 
      integer , intent(out) :: len 
      character , intent(in) :: kard*(*) 
      character , intent(out) :: str*(*) 
!-----------------------------------------------
!   l o c a l   v a r i a b l e s
!-----------------------------------------------
      integer :: icol, noc, k1, i, k, k2 
      character(len=1) :: comma=','

!-----------------------------------------------
!
!          get sequential parameter from a text card where parameters
!           are spearated by commas
!
!          **given**
!          ipnum = the parameter number to retrieve
!          kard  = the card of text with parameters separated by commas
!
!          **returning**
!          str   = the parameter
!          len   = the number of characters in str
!
!
!
!
!
!
      icol = 0 
      noc = ipnum - 1 
      if (noc == 0) then 
        k1 = 1 
        go to 350 
      endif 
!
      do i = 1, noc 
        k = index(kard(icol+1:),comma) + icol 
        if (k == icol) go to 400 
        icol = k 
      end do 
      k1 = k + 1 
!
  350 continue 
      k = index(kard(icol+1:),comma) + icol 
      if (k /= icol) then 
        k2 = k - 1 
        if (k1 <= k2) then 
          str = kard(k1:k2) 
          len = k2 - k1 + 1 
!
!
          return  
!
        endif 
      endif 
  400 continue 
      str = ' ' 
      len = 0 
      return  
      end subroutine histprim_gsprm 

!!!!!!!!!!!!!!!!!!!!!!!!!!!! histprim_process_name !!!!!!!!!!!!!!!!!!!!!!!!!!
      subroutine histprim_process_name(card,procname)

      character(len=*),intent(in) :: card
      character(len=*),intent(out) :: procname

      integer :: k1,k2



!         Parse out the process name from card
!         The process name is between PROCESS:blank and blank*




      procname=' '
      k1=index(card,'PROCESS:')
      if(k1.eq.0)return
      k2=index(card,' **********')

      k1=k1+9
      k2=k2-1

      procname=card(k1:k2)


      return
      end subroutine histprim_process_name

      subroutine histprim_validate_parameter(parm,plen,col,card,istat)

      character(len=*),intent(in) :: parm,card
      integer,intent(in)  :: col,plen
      integer,intent(out) :: istat
!

!           parm   = the parameter to validate
!           plen   = the number of characters in parm
!           col    = the column in card where the string parm begins
!           card   = the history card where the string parm was found
!           istat  = status returned
!                    0 = valid
!                    1 = not valid
!
!        Local...
      integer :: i     ,ieq 

!       A parameter name will be followed by an equal sign, a parameter answer,
!         and a comma or end of card.

      istat=0
      ieq = index(card(col+1:),'=') + col 
      if(ieq.eq.col)then
!         Invalid - no equal sign
        istat=1
        return
      endif

!          There should be nothing but blanks between the parameter and the =
      do i=col+plen,ieq-1
        if(card(i:i).ne.' ')then
          istat=1
          return
        endif
      enddo


      end subroutine histprim_validate_parameter



!!----------------------------- end of module -----------------------------!!
!!----------------------------- end of module -----------------------------!!
!!----------------------------- end of module -----------------------------!!


      end module histprim_module


!!--------------------------------- end -----------------------------------!!
!!--------------------------------- end -----------------------------------!!
!!--------------------------------- end -----------------------------------!!

