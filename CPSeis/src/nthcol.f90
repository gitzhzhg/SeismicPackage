!<CPS_v1 type="PRIMITIVE"/>
!!------------------------------- nthcol.f90 ------------------------------!!
!!------------------------------- nthcol.f90 ------------------------------!!
!!------------------------------- nthcol.f90 ------------------------------!!
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
! Name       : nthcol 
! Category   : character
! Written    : 2005-06-14   by: Karen Goodger
! Revised    : 2005-07-11   by: Karen Goodger
! Maturity   : production
! Purpose    : Return the nth column in a string
! Portability: No known limitations.
!
!-------------------------------------------------------------------------------
!</brief_doc>


!<descript_doc>
!-------------------------------------------------------------------------------
!                         GENERAL DESCRIPTION            
!
! Returns the nth column on a card, where the columns are separated by blanks
!
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
!                   i    i   o     o
!     call nthcol(card,col,field,istat)    
!
!     character(len=*) :: card  = card to parse
!     integer          :: col   = which column to return in field
!     character(len=*) :: field = The colth field on card
!     integer          :: istat = 0 is all OK
!                               = 1 if a problem          
!
!!!
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
!     Date        Author     Description
!     ----        ------     -----------
!  1. 2005-07-11  Goodger    Initial version.
!
!
!-------------------------------------------------------------------------------
!</history_doc>


!<portability_doc>
!-------------------------------------------------------------------------------
!                         PORTABILITY LIMITATIONS         
!
! No known limitations.
!-------------------------------------------------------------------------------
!</portability_doc>





!!-------------------------- start of module ------------------------------!!
!!-------------------------- start of module ------------------------------!!
!!-------------------------- start of module ------------------------------!!


      module nthcol_module
      implicit none


      character(len=100),public,save :: nthcol_IDENT = &
'$Id: nthcol.f90,v 1.1 2005/07/11 12:42:40 Goodger prod sps $'


!!-------------------------- data structure -------------------------------!!
!!-------------------------- data structure -------------------------------!!
!!-------------------------- data structure -------------------------------!!



!!------------------------------ data -------------------------------------!!
!!------------------------------ data -------------------------------------!!
!!------------------------------ data -------------------------------------!!


      contains


!!----------------------------- subroutines -------------------------------!!
!!----------------------------- subroutines -------------------------------!!
!!----------------------------- subroutines -------------------------------!!


      subroutine nthcol(card,col,field,istat)



      character(len=*),intent(in)  :: card
      character(len=*),intent(out) :: field
 
      integer,intent(in)  :: col
      integer,intent(out) :: istat

!        Local variables
      integer :: i,i1,i2,j,knt,nc

      nc=len(card)
      field=' '
      istat=0
      i1=0
      if(col.eq.1)i1=1
      i2=nc
      knt=1
      i=0
      DO 
        i=i+1
        if(i.gt.nc)exit
        if(card(i:i).eq.' ')then
           knt=knt+1
           do j=i,nc
             if(card(j:j).eq.' ')cycle
             i=j
             exit
           enddo
        endif
        if(knt.eq.col)then
          i1=i
          do j=i,nc
            if(card(j:j).eq.' ')then
              i2=j-1
               go to 500
             endif
          enddo
        endif
      ENDDO
 500  continue
      if(i1.eq.0)then
        istat=1
        return
      endif
      field=card(i1:i2)

      end subroutine nthcol


!!----------------------------- end of module -----------------------------!!
!!----------------------------- end of module -----------------------------!!
!!----------------------------- end of module -----------------------------!!


      end module nthcol_module


!!--------------------------------- end -----------------------------------!!
!!--------------------------------- end -----------------------------------!!
!!--------------------------------- end -----------------------------------!!

