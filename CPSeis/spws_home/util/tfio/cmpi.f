c       subroutine cmpi
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
C***************************** COPYRIGHT NOTICE ********************************
C*                                                                             *
C*                 CONFIDENTIAL AND PROPRIETARY INFORMATION                    *
C*                              OF CONOCO INC.                                 *
C*                      PROTECTED BY THE COPYRIGHT LAW                         *
C*                          AS AN UNPUBLISHED WORK                             *
C*                                                                             *
C***************************** COPYRIGHT NOTICE ********************************
C        1         2         3         4         5         6         7
C23456789012345678901234567890123456789012345678901234567890123456789012
C\USER DOC
C-----------------------------------------------------------------------
C                        CONOCO PROCESSING SYSTEM
C                 EXPLORATION RESEARCH & SERVICES DIVISION
C                               CONOCO INC.
C
C                        C P S   P R I M I T I V E
C
CPrimitive name:  CMPI - T3E version
C        Author:  Douglas Hanson Shells around MPI calls.
C  Last revised:  98/07/09  Day
C
C  Purpose:  Find the CMPI value of a list of unsorted values.
C-----------------------------------------------------------------------
C                           CALLING SEQUENCE
C
C       CALL CMPI_ see individual usage
C
C Name   Type*   Valid     Description      *Type: I=IN, O=OUT, B=BOTH
C ----   ----    -----     -----------
C-----------------------------------------------------------------------
C                                 NOTES
C
C 1. These routine provide convenient wrappers around MPI calls.
C 2. On the T3E they call the original MPI routines
C 3. On the J90 they avoid use of the MPI routines
C    and work like single pe versions of T3E versions.
C
C The main differences between the two version are delimited by the 
C following three lines:
C
C Top of T3E version
C Top of J90 version
C End of T3E, J90 differences
C
C On the J90 the code between the lines 
C Top of T3E version and Top of J90 version          is active 
C        and the code between the lines
C Top of J90 version and End of T3E, J90 differences is commented out
C 
C On the J90 the code between the lines 
C Top of J90 version and End of T3E, J90 differences is active 
C        and the code between the lines
C Top of T3E version and Top of J90 version          is commented out
C
C In addition the line include   'mpif.h' is commented out on the J90.
C
C-----------------------------------------------------------------------
C                           REVISION HISTORY
C
C Date      Author       Description
C ----      ------       -----------
C 98/07/09  Day          Added cmpi_rec_i,cmpi_send_i,cmpi_comm_create,
C                        cmpi_group_free,cmpi_comm_group,cmpi_group_incl
C                        cmpi_group_free,cmpi_comm_rank,cmpi_bcast
C 98/05/06  Hanson       Add cmpi_prn_get, cmpi_prn_put
C 98/04/17  Hanson       Improve documentation
C 97/11/01  Hanson       Original version
C-----------------------------------------------------------------------
C                  SUBROUTINE NAMES IN THIS MODULE
C
C CMPI_BCAST_R         CMPI_BCAST_I         CMPI_ALLREDUCE_R    
C CMPI_ALLREDUCE_I     CMPI_REDUCE_R        CMPI_REDUCE_R4      
C CMPI_REDUCE_I        CMPI_ALLREDUCE_R1    CMPI_ALLREDUCE_I1   
C CMPI_ABORT           CMPI_LINE            CMPI_LINE_PARIO     
C CMPI_LINE_PEL        CMPI_PARIO_PUT       CMPI_PARIO_GET      
C CMPI_COPI            CMPI_COPY            CMPI_SEND_I
C CMPI_RECV_I          CMPI_COMM_GROUP      CMPI_GROUP_INCL
C CMPI_COMM_CREATE     CMPI_GROUP_FREE
C
C-----------------------------------------------------------------------
C                  ENTRY NAMES IN THIS MODULE
C
C-----------------------------------------------------------------------
C                  FUNCTION BLOCK NAMES IN THIS MODULE
C
C CMPI_INIT            CMPI_FINALIZE        CMPI_I_PEL          
C CMPI_N_PEL           CMPI_BARRIER         CMPI_WTIME          
C CMPI_ALLINTMAX       CMPI_PRN             CMPI_PARIO          
C CMPI_R               CMPI_COMM_RANK
C
C-----------------------------------------------------------------------
C                  COMMON BLOCK NAMES IN THIS MODULE
C
C CMPI_PARIO_A
C
C-----------------------------------------------------------------------
C                  EXTERNALS REFERENCED BY THIS MODULE
C
C MPI_INIT             MPI_BARRIER          MPI_FINALIZE        
C MPI_COMM_RANK        MPI_COMM_SIZE        MPI_BCAST            
C MPI_ALLREDUCE        MPI_REDUCE           MPI_ABORT           
C SECOND              
C
C-----------------------------------------------------------------------
C                         MEMORY REQUIREMENTS
C  STORAGE       - 0
C  HEAP(dynamic) - 0
C-----------------------------------------------------------------------
C\END DOC
C23456789012345678901234567890123456789012345678901234567890123456789012
      integer function cmpi_init()
c initialize the mpi calls
c     The MPI_Init routine initializes the MPI execution environment.  
      implicit  none

c     include   "mpif.h"

      integer   i_err

      i_err = 0

c  Top of T3E version
c     call mpi_init(i_err)
c  Top of J90 version

c  End of T3E, J90 differences

      cmpi_init = i_err

      return
      end

C23456789012345678901234567890123456789012345678901234567890123456789012
      integer function cmpi_finalize()
c finalize the mpi calls
      implicit  none

c     include   "mpif.h"

      integer   i_err

      i_err = 0

c  Top of T3E version
c     call mpi_barrier(mpi_comm_world,i_err)
c     call mpi_finalize(i_err)
c  Top of J90 version

c  End of T3E, J90 differences

      cmpi_finalize = i_err

      return
      end

C23456789012345678901234567890123456789012345678901234567890123456789012
      integer function cmpi_i_pel()
c return the pe number 0 = first cmpi_n_pel()-1 = last
c     The MPI_Comm_rank routine determines the rank of the calling process
c     in the communicator.  
      implicit  none

c     include   "mpif.h"

      integer i_pel
      integer i_err

      i_pel = 0

c  Top of T3E version
c     call mpi_comm_rank(mpi_comm_world,i_pel,i_err)
c  Top of J90 version

c  End of T3E, J90 differences

      cmpi_i_pel = i_pel

      return
      end

C23456789012345678901234567890123456789012345678901234567890123456789012
      integer function cmpi_n_pel()
c return the number of pes.
c     The MPI_Comm_size routine determines the number of the calling processes
c     in the communicator.  
      implicit  none

c     include   "mpif.h"

      integer n_pel
      integer i_err

      n_pel = 1

c  Top of T3E version
c     call mpi_comm_size(mpi_comm_world,n_pel,i_err)
c  Top of J90 version

c  End of T3E, J90 differences

      cmpi_n_pel = n_pel

      return
      end

C23456789012345678901234567890123456789012345678901234567890123456789012
      integer function cmpi_barrier()
c  set an mpi barrier, wait at this point until all pes reach it
c     The MPI_Barrier routine blocks until all processes have reached the
c     routine at which the barrier is placed.  It blocks the caller until
c     all group members have called it; the call returns at any process only
c     after all group members have entered the call.  
      implicit  none

c     include   "mpif.h"
      integer   i_err

      i_err = 0

c  Top of T3E version
c     call mpi_barrier(mpi_comm_world,i_err)
c  Top of J90 version

c  End of T3E, J90 differences

      cmpi_barrier = i_err

      return
      end

C23456789012345678901234567890123456789012345678901234567890123456789012
      subroutine cmpi_send_i(buff,num,dest,tag,i_err)
      implicit none
      integer buff(*),num,dest,tag,d,t,i_err
c     include "mpif.h"

      d=dest
c     if(dest.lt.0) d=MPI_ANY_SOURCE
      t=tag
c     if(tag.lt.0) t=MPI_ANY_TAG
c     call mpi_send(buff,num,mpi_integer,d,t,
c    1 mpi_comm_world,i_err)
      return
      end
 
C23456789012345678901234567890123456789012345678901234567890123456789012
      subroutine cmpi_recv_i(buff,num,source,tag,rsource,rtag,i_err)
      implicit none
      integer source,tag,rtag,rsource,s,t
      integer buff(*),status(4),num,i_err
c     include "mpif.h"

      s=source
c     if(source.lt.0) s=MPI_ANY_SOURCE
      t=tag
c     if(tag.lt.0) t=MPI_ANY_TAG
c     call mpi_recv(buff,num,mpi_integer,s,t,
c    1 mpi_comm_world,status, i_err)
c     rtag    = status(MPI_TAG)
c     rsource = status(MPI_SOURCE)
      return
      end

C23456789012345678901234567890123456789012345678901234567890123456789012
      real function cmpi_wtime()
c return cpu time in seconds
c     The MPI_Wtime routine returns an elapsed time on the calling
c     processor.  This routine is intended to be a high-resolution, elapsed
c     (or wall) clock.  See MPI_WTICK to determine the resolution of
c     MPI_WTIME.
      implicit  none

c     include   "mpif.h"

      real      cpu_time,second

      cpu_time=0.
c  Top of T3E version
c     cpu_time = mpi_wtime()
c  Top of J90 version
c      cpu_time = second()
c  End of T3E, J90 differences

      cmpi_wtime = cpu_time

      return
      end

C23456789012345678901234567890123456789012345678901234567890123456789012
      subroutine cmpi_bcast_r(i_pel,nx_inp,x_inp)
c  broadcast nx_inp elements of real array x_inp 
c  from pe i_pel to the rest of the Pes.
c     The MPI_Bcast routine broadcasts a message from the process with a
c     specified rank (called a root) to all other processes of the group.
      implicit  none

c     include   "mpif.h"

      integer   i_pel
      integer   nx_inp
      real      x_inp(nx_inp)

      integer   mpi_return

c  Top of T3E version
c     call mpi_bcast(x_inp,nx_inp,mpi_real,i_pel
c    1,mpi_comm_world,mpi_return)
c  Top of J90 version

c  End of T3E, J90 differences

      return
      end

C23456789012345678901234567890123456789012345678901234567890123456789012
      subroutine cmpi_bcast_i(i_pel,nx_inp,x_inp)
c  broadcast nx_inp elements of integer array x_inp 
c  from pe i_pel to the rest of the Pes.
c     The MPI_Bcast routine broadcasts a message from the process with a
c     specified rank (called a root) to all other processes of the group.
      implicit  none

c     include   "mpif.h"

      integer   i_pel
      integer   nx_inp
      integer   x_inp(nx_inp)

      integer   mpi_return

c  Top of T3E version
c     call mpi_bcast(x_inp,nx_inp,mpi_integer,i_pel
c    1,mpi_comm_world,mpi_return)
c  Top of J90 version

c  End of T3E, J90 differences

      return
      end

c23456789012345678901234567890123456789012345678901234567890123456789012
      subroutine cmpi_allreduce_r(nx_inp,x_inp,x_out)
c  reduce nx_inp elements of real array x_inp on all pes
c  into real array x_out and broadcast to all pes.
c  The MPI_Allreduce routine combines values from all processes and
c  distributes the result back to all processes.  
      implicit  none

c     include   "mpif.h"

      integer   nx_inp
      real      x_inp(nx_inp)
      real      x_out(nx_inp)

      integer   mpi_return

c  Top of T3E version
c     call mpi_allreduce(x_inp,x_out,nx_inp,mpi_real,mpi_sum
c    1,mpi_comm_world,mpi_return)
c  Top of J90 version
       call cmpi_copy(nx_inp,x_inp,x_out)
c  End of T3E, J90 differences

      return
      end

c23456789012345678901234567890123456789012345678901234567890123456789012
      subroutine cmpi_allreduce_i(nx_inp,x_inp,x_out)
c  reduce nx_inp elements of integer array x_inp on all pes
c  into integer array x_out and broadcast to all pes.
c  The MPI_Allreduce routine combines values from all processes and
c  distributes the result back to all processes.  
      implicit  none

c     include   "mpif.h"

      integer   nx_inp
      integer   x_inp(nx_inp)
      integer   x_out(nx_inp)

      integer   mpi_return

c  Top of T3E version
c     call mpi_allreduce(x_inp,x_out,nx_inp,mpi_integer,mpi_sum
c    1,mpi_comm_world,mpi_return)
c  Top of J90 version
       call cmpi_copi(nx_inp,x_inp,x_out)
c  End of T3E, J90 differences

      return
      end

c23456789012345678901234567890123456789012345678901234567890123456789012
      subroutine cmpi_allreduce_r1(x_inp)
c  reduce the value of a single real value x_inp from all pes to all pes
c     The MPI_Reduce routine reduces values on all processes to a single value.
      implicit  none

c     include   "mpif.h"

      integer   nx_inp
      real      x_inp
      real      x_out

      integer   mpi_return

c  Top of T3E version
c     nx_inp = 1
c     call mpi_allreduce(x_inp,x_out,nx_inp,mpi_real,mpi_sum
c    1,mpi_comm_world,mpi_return)
c     x_inp = x_out
c  Top of J90 version

c  End of T3E, J90 differences

      return
      end

c23456789012345678901234567890123456789012345678901234567890123456789012
      subroutine cmpi_allreduce_i1(x_inp)
c  reduce the value of a single integer value x_inp from all pes to all pes
c     The MPI_Reduce routine reduces values on all processes to a single value.
      implicit  none

c     include   "mpif.h"

      integer   nx_inp
      integer   x_inp
      integer   x_out

      integer   mpi_return

c  Top of T3E version
c     nx_inp = 1
c     call mpi_allreduce(x_inp,x_out,nx_inp,mpi_integer,mpi_sum
c    1,mpi_comm_world,mpi_return)
c     x_inp = x_out
c  Top of J90 version

c  End of T3E, J90 differences

      return
      end

c23456789012345678901234567890123456789012345678901234567890123456789012
      subroutine cmpi_reduce_r(i_pel,nx_inp,x_inp,x_out)
c  reduce nx_inp elements of real array x_inp on all pes
c  into real array x_out on pe i_pel.
c     The MPI_Reduce routine reduces values on all processes to a single value.
      implicit  none

c     include   "mpif.h"

      integer   i_pel
      integer   nx_inp
      real      x_inp(nx_inp)
      real      x_out(nx_inp)

      integer   mpi_return

c  Top of T3E version
c     call mpi_reduce(x_inp,x_out,nx_inp,mpi_real,mpi_sum
c    1,i_pel,mpi_comm_world,mpi_return)
c  Top of J90 version
       call cmpi_copy(nx_inp,x_inp,x_out)
c  End of T3E, J90 differences

      return
      end

c23456789012345678901234567890123456789012345678901234567890123456789012
      subroutine cmpi_reduce_r4(i_pel,nx_inp,x_inp,x_out)
c  reduce nx_inp elements of real array x_inp on all pes
c  into real array x_out on pe i_pel.
c     The MPI_Reduce routine reduces values on all processes to a single value.
      implicit  none

c     include   "mpif.h"

      integer   i_pel
      integer   nx_inp
      real*4   x_inp(nx_inp)
      real*4   x_out(nx_inp)

      integer   mpi_return

c  Top of T3E version
c     call mpi_reduce(x_inp,x_out,nx_inp,mpi_real4,mpi_sum
c    1,i_pel,mpi_comm_world,mpi_return)
c  Top of J90 version
       call cmpi_copy(nx_inp,x_inp,x_out)
c  End of T3E, J90 differences

      return
      end

c23456789012345678901234567890123456789012345678901234567890123456789012
      subroutine cmpi_reduce_i(i_pel,nx_inp,x_inp,x_out)
c  reduce nx_inp elements of integer array x_inp on all pes
c  into integer array x_out on pe i_pel.
c     The MPI_Reduce routine reduces values on all processes to a single value.
      implicit  none

c     include   "mpif.h"

      integer   i_pel
      integer   nx_inp
      integer   x_inp(nx_inp)
      integer   x_out(nx_inp)

      integer   mpi_return

c  Top of T3E version
c     call mpi_reduce(x_inp,x_out,nx_inp,mpi_integer,mpi_sum
c    1,i_pel,mpi_comm_world,mpi_return)
c  Top of J90 version
       call cmpi_copi(nx_inp,x_inp,x_out)
c  End of T3E, J90 differences

      return
      end

c23456789012345678901234567890123456789012345678901234567890123456789012
      subroutine cmpi_abort(mpi_return)
c  terminates the MPI execution environment.  
c     The MPI_Abort routine terminates the MPI execution environment.  This
c     routine terminates all MPI processes associated with the communicator
c     MPI_COMM_WORLD.  MPI allows termination of only the processes
c     associated with the specified communicator, but this implementation
c     always terminates all processes.  
      implicit  none

c     include   "mpif.h"

      integer   mpi_return

c  Top of T3E version
c     call mpi_abort(mpi_comm_world,mpi_return,mpi_return )
c  Top of J90 version

c  End of T3E, J90 differences

      return
      end

c23456789012345678901234567890123456789012345678901234567890123456789012
       integer function cmpi_allintmax(x_inp)
c  return the maximum of integer x_inp from all pes to all pes.
c  The MPI_Allreduce routine combines values from all processes and
c  distributes the result back to all processes.  
      implicit  none

c     include   "mpif.h"

      integer   x_inp,x_out
      integer   mpi_return

      x_out = x_inp

c  Top of T3E version
c     call mpi_allreduce(x_inp,x_out,1,mpi_integer,mpi_max
c    1,mpi_comm_world,mpi_return)
c  Top of J90 version

c  End of T3E, J90 differences

       cmpi_allintmax = x_out

       return
       end

c23456789012345678901234567890123456789012345678901234567890123456789012
      subroutine cmpi_line(lu_out,j_pel,card)
c  print a single line of character data, card to logical unit lu_out
c  if the current pe is j_pel
      implicit  none
      integer   cmpi_i_pel
      integer   cmpi_r

      integer   lu_out,j_pel
      character card*(*)

      if (lu_out .ge. 0 .and. lu_out .le. 99) then

      if (cmpi_i_pel() .eq. j_pel) 
     1write(lu_out,'(a)')card(1:cmpi_r(card))

      endif    ! if (lu_out .ge. 0 .and. lu_out .le. 99) then

      return
      end

c23456789012345678901234567890123456789012345678901234567890123456789012
      subroutine cmpi_line_pario(cmpi_pario,lu_out,j_pel,card)
c  print a single line of character data, card to logical unit lu_out
c  if the current pe is j_pel and the flag cmpi_pario has been set to 1
      implicit  none
      integer   cmpi_i_pel
      integer   cmpi_r

      integer   cmpi_pario
      integer   lu_out,j_pel
      character card*(*)

      if (lu_out .ge. 0 .and. lu_out .le. 99) then

      if (cmpi_i_pel() .eq. j_pel .or. cmpi_pario .eq. 1) 
     1write(lu_out,'(a)')card(1:cmpi_r(card))

      endif    ! if (lu_out .ge. 0 .and. lu_out .le. 99) then

      return
      end

c23456789012345678901234567890123456789012345678901234567890123456789012
      subroutine cmpi_line_pel(lu_out,j_pel,card)
c  print a single line of character data, card to logical unit lu_out
c  with pe identification
c  if the current pe is j_pel
      implicit  none
      integer   cmpi_i_pel
      integer   cmpi_r

      integer   lu_out,j_pel
      character card*(*)

      if (lu_out .ge. 0 .and. lu_out .le. 99) then

      if (cmpi_i_pel() .eq. j_pel) 
     1write(lu_out,'('' i_pel='',i5,1x,a)')
     1 cmpi_i_pel(),card(1:cmpi_r(card))

      endif    ! if (lu_out .ge. 0 .and. lu_out .le. 99) then

      return
      end

c23456789012345678901234567890123456789012345678901234567890123456789012
      integer function cmpi_prn()
c  get the cmpi print io flag
c  if cmpi_pario = 0 io is done by pe 0 and broad cast to other pes
c  if cmpi_pario = 1 io is done by the current pe no broadcasts or barriers 
c  are done.  
      implicit  none
      integer   cmpi_i_pel
      integer   cmpi_pario
      integer   i_print

      call cmpi_prn_get(i_print)
      cmpi_prn = 0
      if (cmpi_i_pel() .eq. 0 .or. cmpi_pario() .eq. 1
     1 .or. i_print .eq. 1) cmpi_prn = 1

      return
      end

c23456789012345678901234567890123456789012345678901234567890123456789012
      subroutine cmpi_prn_get(j_print)
      implicit  none
      integer   j_print
      integer   i_print
      common    /cmpi_prn_0/i_print
      data      i_print/0/
      j_print = i_print
      return

c23456789012345678901234567890123456789012345678901234567890123456789012
      entry cmpi_prn_put(j_print)
      i_print = j_print
      return

      end

c23456789012345678901234567890123456789012345678901234567890123456789012
      integer function cmpi_pario()
c  get the cmpi parrellel io flag
c  if cmpi_pario = 0 io is done by pe 0 and broad cast to other pes
c  if cmpi_pario = 1 io is done by the current pe no broadcasts or barriers 
c  are done.  
      implicit  none
      integer   cmpi_pario_0
      common   /cmpi_pario_a/cmpi_pario_0
      cmpi_pario = cmpi_pario_0
      return
      end

c23456789012345678901234567890123456789012345678901234567890123456789012
      subroutine cmpi_pario_put(cmpi_pario_1)
c  put the cmpi parrellel io flag
c  if cmpi_PARIO = 0 io is done by pe 0 and broad cast to other pes
c  if cmpi_PARIO = 1 io is done by the current pe no broadcasts or barriers 
c  are done.  
      implicit  none
      integer   cmpi_pario_0
      common   /cmpi_pario_a/cmpi_pario_0
      integer   cmpi_pario_1
      cmpi_pario_0 = cmpi_pario_1
      return
      end

c23456789012345678901234567890123456789012345678901234567890123456789012
      subroutine cmpi_pario_get(cmpi_pario_1)
c  get the cmpi parrellel io flag
c  if cmpi_PARIO = 0 io is done by pe 0 and broad cast to other pes
c  if cmpi_PARIO = 1 io is done by the current pe no broadcasts or barriers 
c  are done.  
      implicit  none
      integer   cmpi_pario_0
      common   /cmpi_pario_a/cmpi_pario_0
      integer   cmpi_pario_1
      cmpi_pario_1 = cmpi_pario_0
      return
      end

c23456789012345678901234567890123456789012345678901234567890123456789012
      subroutine cmpi_copy(nx,x1,x2)
c  copy x1 to x2
      implicit  none

      integer  nx
      real     x1(1),x2(1)
      integer  ix

      do ix = 1 , nx

        x2(ix) = x1(ix)

      enddo    ! do ix = 1 , nx

      return
      end

c23456789012345678901234567890123456789012345678901234567890123456789012
      subroutine cmpi_copi(nx,x1,x2)
c  copy x1 to x2
      implicit  none

      integer  nx
      integer  x1(1),x2(1)
      integer  ix

      do ix = 1 , nx

        x2(ix) = x1(ix)

      enddo    ! do ix = 1 , nx

      return
      end

c23456789012345678901234567890123456789012345678901234567890123456789012
      integer function cmpi_r(str)
c  find the last non blank character
      character str*(*)
      do j = len(str) , 1 , -1
        cmpi_r = j
        if (str(j:j) .ne. ' ') return
      enddo
      cmpi_r = 0
      return
      end

c23456789012345678901234567890123456789012345678901234567890123456789012
      integer function cmpi_comm_rank(comm)
c return the pe number 0 = first cmpi_n_pel()-1 = last
c     The MPI_Comm_rank routine determines the rank of the calling process
c     in the communicator, comm.  
      implicit  none

c     include   "mpif.h"

      integer i_pel,comm
      integer i_err

      i_pel  = 0

c  Top of T3E version
c     call mpi_comm_rank(comm,i_pel,i_err)
c  Top of J90 version

c  End of T3E, J90 differences

      cmpi_comm_rank = i_pel

      return
      end

c23456789012345678901234567890123456789012345678901234567890123456789012
      subroutine cmpi_comm_group(comm,group,i_err)
c     The MPI_Comm_group routine determines the group
c     for the communicator, comm.  
      implicit  none

c     include   "mpif.h"

      integer i_pel,comm,group
      integer i_err

c     group = mpi_group_null
c  Top of T3E version
c     call mpi_comm_group(comm,group,i_err)
c  Top of J90 version

c  End of T3E, J90 differences

      return
      end

c23456789012345678901234567890123456789012345678901234567890123456789012
      subroutine cmpi_group_incl(groupin,n,ranks,group,i_err)
c     The MPI_group_incl routine creates group, from groupin,
c     given an integer array ranks of cpu numbers
      implicit  none

c     include   "mpif.h"

      integer groupin,n,ranks(*),group
      integer i_err

c     group = mpi_group_null
c  Top of T3E version
c     call mpi_group_incl(groupin,n,ranks,group,i_err)
c  Top of J90 version

c  End of T3E, J90 differences

      return
      end

c23456789012345678901234567890123456789012345678901234567890123456789012
      subroutine cmpi_comm_create(comm,group,newcomm,i_err)
c     The MPI_comm_create routine creates a new communicator from group
      implicit  none

c     include   "mpif.h"

      integer comm,group,newcomm
      integer i_err


c     newcomm=mpi_comm_world
c  Top of T3E version
c     call mpi_comm_create(comm,group,newcomm,i_err)
c  Top of J90 version
      newcomm=-1

c  End of T3E, J90 differences

      return
      end

c23456789012345678901234567890123456789012345678901234567890123456789012
      subroutine cmpi_group_free(group,i_err)
c     The MPI_group_free call frees a group handle
      implicit  none

c     include   "mpif.h"

      integer comm,group
      integer i_err


c  Top of T3E version
c     call mpi_group_free(group,i_err)
c  Top of J90 version

c  End of T3E, J90 differences

      return
      end

C23456789012345678901234567890123456789012345678901234567890123456789012
      subroutine cmpi_bcast(x_inp,nx_inp,type,i_pel,comm,i_err)
c  broadcast nx_inp elements of integer array x_inp 
c  from pe i_pel to the rest of the Pes. type is an mpi_type
c     The MPI_Bcast routine broadcasts a message from the process with a
c     specified rank (called a root) to all other processes of the group.
      implicit  none

c     include   "mpif.h"

      integer   i_pel,type,comm,i_err
      integer   nx_inp
      integer   x_inp(nx_inp)


      i_err=0
c  Top of T3E version
c     call mpi_bcast(x_inp,nx_inp,type,i_pel
c    1,comm,i_err)
c  Top of J90 version

c  End of T3E, J90 differences

      return
      end

