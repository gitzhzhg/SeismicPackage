C***************************** COPYRIGHT NOTICE ********************************
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
C*                                                                             *
C*                 CONFIDENTIAL AND PROPRIETARY INFORMATION                    *
C*                              OF CONOCO INC.                                 *
C*                      PROTECTED BY THE COPYRIGHT LAW                         *
C*                          AS AN UNPUBLISHED WORK                             *
C*                                                                             *
C***************************** COPYRIGHT NOTICE ********************************
C\USER DOC
C-----------------------------------------------------------------------
C                         CRAY PROCESSING SYSTEM
C                     EXPLORATION RESEARCH DIVISION
C                              CONOCO, INC.
C
C                       C P S   P R I M I T I V E
C
C    Primitive name:  GETLUN             (get logical unit number)
C  Source directory:  primitives/prim_io
C           Library:  conlib
C            Author:  Doug Hanson
C      Last revised:  95/04/13  Stoeckley
C
C  Purpose:       Read and increment the next available unit number.
C-----------------------------------------------------------------------
C                           CALLING SEQUENCE
C
C        CALL GETLUNS             -  SETUP (CAN BE CALLED BY INIT)
C        CALL GETLUN (LUN,*ERR)
C  
C  LUN   = NEXT AVAILABLE LOGICAL UNIT NUMBER
C  *ERR  = ERROR RETURN ADDRESS
C_______________________________________________________________________
C                                 NOTES
c 1. GETLUN searches all unit numbers and returns the first free one 
C    it finds. It starts the search just after the last unit number
C    it returned.  Entry GETLUNS, which is an optional entry, closes 
C    all files whose unit numbers were previously returned by GETLUN.
C
C 2. The "named=" keyword in the "inquire" statement does not work
C    correctly on the Hewlett Packard.  Therefore, a work-around (similar
C    to the old way GETLUN used to work) has been added on 94/11/29,
C    making use of the subroutine get_machine() which has just now been
C    written to return an integer identifying the machine the program
C    is running on.  This work-around will be removed when a solution
C    to the Hewlett Packard problem is found.
C
C 3. Hewlett Packard suggested using the "opened=" keyword instead of
C    the "named=" keyword in the "inquire" statement.  Therefore, this
C    change was put into place on 95/01/10, replacing the temporary
C    code mentioned in note 2 above.  This keyword might work for all
C    machines, but I've made the change only for the Hewlett Packard
C    to keep from having to test it everywhere.
C-----------------------------------------------------------------------
C                         REVISION HISTORY
C     Date     Author       Description
C     ----     ------       -----------
C 5.  95/04/13 Stoeckley    Modified print statements for failed attempts.
C 4.  95/01/10 Stoeckley    Changed code to work with Hewlett Packard
C                             and other machines, based on reply from
C                             Hewlett Packard.  See note 3 above.
C 3.  94/11/29 Stoeckley    Added work-around for Hewlett Packard.  See
C                             note 2 above.
C 2.  92/05/22 Stoeckley    Entire routine replaced.  See note 1 above.
C 1.  92/04/06 R.Day        Changes made to run under Unix.
C_______________________________________________________________________
C\END DOC

c     SUBROUTINE GETLUN(LUN1,*)
c     PARAMETER ( MAXLUN = 99 , LUN0 = 20 )
c     DATA LUN/0/
c     COMMON /GETLUN1/ LUN
c     LUN  = MAX(LUN0,LUN)
c     LUN1 = LUN
c     LUN = LUN + 1
c       IF (LUN1 .GT. MAXLUN) THEN
c       PRINT*,' ERROR IN ASSIGNING UNIT NUMBER ',LUN1
c       RETURN 1
c       ENDIF
c     RETURN
c     ENTRY GETLUNS
c     IF(LUN .GE. LUN0) THEN
c      DO 10 N=LUN0,LUN
c10    CLOSE(UNIT= N)
c     END IF
c     LUN = LUN0
c     RETURN
c     END


      subroutine getlun (lun,*)
      implicit none
      integer lun,i
      logical quest
      integer get_machine                           ! added for HP 94/11/29
      integer lstart,lstop,first
      parameter (lstart=20,lstop=99)
      integer last,list(lstop)
      save    last,list
      data    last,list/lstop,lstop*0/

      do i=lstart,lstop
           last=last+1
           if (last.gt.lstop) last=lstart
           if (i.eq.lstart) first=last
           if (get_machine().eq.6) then             ! added for HP 94/11/29
cccc            quest=(list(last).gt.0)             ! added for HP 94/11/29
                inquire (last,opened=quest,err=999) ! added for HP 95/01/10
           else                                     ! added for HP 94/11/29
                inquire (last,named=quest,err=999)
           end if                                   ! added for HP 94/11/29
           if (.not.quest) then
                list(last)=1
                lun=last
c               if(get_machine().eq.6)
c    $                print *,
c    $                 'GETLUN successfully called on HP: lun = ',lun
                return
           end if
      end do

999   if(get_machine().eq.6) then
            print *, 'GETLUN failed on HP: lun = ',first,' thru ',last
      else
            print *, 'GETLUN failed: lun = ',first,' thru ',last
      end if
      return 1

      entry getluns
      do i=lstart,lstop
           if (list(i).gt.0) then
                close (i)
                list(i)=0
           end if
      end do
      last=lstop
      return
      end

 
