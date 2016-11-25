!<CPS_v1 type="PRIMITIVE"/>


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
! Name       : TMERGE
! Category   : sorts
! Written    : 2010-02-22   by: Tom Stoeckley
! Revised    : 2010-02-22   by: Tom Stoeckley
! Maturity   : beta
! Purpose    : Merge streams of traces.
! Portability: No known limitations.
! Parallel   : Yes.
!
!-------------------------------------------------------------------------------
!</brief_doc>


!<descript_doc>
!-------------------------------------------------------------------------------
!                         GENERAL DESCRIPTION
!
! TMERGE merges sorted trace streams from various nodes.  The purpose is
! to do parallel sorts of large datasets in a parallel job.  This primitive
! merges sorted trace streams from the various nodes, putting the first traces
! on the first node and the last traces on the last node.  The traces are
! moved to the correct node by MPI.
!
! TMERGE should work correctly even if there is no MPI or only one node.
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
!-------------------------------------------------------------------------------
!                          CALLING SEQUENCE               
!
!                            o     i      i     i       i          i          i
!       call tmerge_create (obj, no_mpi, nwih, ndpt, all_nodes, bin_first, bin_last)
!       call tmerge_delete (obj)
!                            b
!
!       call tmerge_execute (obj, ntr, hdo, tro, bin)
!                             b    b    b    b    i 
!
!       receiver = tmerge_get_output_node (obj, primary_bin)
!                                           b      i
!
! type(tmerge_struct)    obj         = pointer to the data structure.
! logical                no_mpi      = true if there is no MPI.
! integer                nwih        = number of trace headers.
! integer                ndpt        = number of trace values.
! logical                all_nodes   = true if output should go to all nodes.
! integer                bin_first   = first primary bin.
! integer                bin_last    = last primary bin.
! integer                ntr         = number of traces.
! double precision       hdo(nwih)   = trace header words.
! real                   tro(ndpt)   = trace.
! type(triplesort_ints)  bin         = bin in which the input trace resides.
! integer                nranks      = number of nodes.
! integer                rank        = this node (0 to nranks-1).
! integer                primary_bin = primary bin in which the trace resides.
! integer                receiver    = node to which the trace will be sent.
!
! MPI is never called if NO_MPI is true.
!-------------------------------------------------------------------------------
!</calling_doc>


!<advice_doc>
!-------------------------------------------------------------------------------
!                           ADVICE FOR USERS
!
!-------------------------------------------------------------------------------
!</advice_doc>


!<history_doc>
!-------------------------------------------------------------------------------
!                          REVISION HISTORY
!
!      Date       Author     Description
!      ----       ------     -----------
!   1. 2010-02-22 Stoeckley  First version.
!-------------------------------------------------------------------------------
!</history_doc>


!<portability_doc>
!-------------------------------------------------------------------------------
!                        PORTABILITY LIMITATIONS
!
! No known limitations.
!-------------------------------------------------------------------------------
!</portability_doc>


!!--------------------------- start of module ------------------------------!!
!!--------------------------- start of module ------------------------------!!
!!--------------------------- start of module ------------------------------!!


      module tmerge_module

      use named_constants_module
      use triplesort_module

      implicit none
      include "mpif.h"


!!--------------------------------- data -----------------------------------!!
!!--------------------------------- data -----------------------------------!!
!!--------------------------------- data -----------------------------------!!


      type,public :: tmerge_struct              
        private
        logical                       :: no_mpi     ! global
        integer                       :: nwih       ! global
        integer                       :: ndpt       ! global
        logical                       :: all_nodes  ! parameter
        integer                       :: bin_first  ! parameter
        integer                       :: bin_last   ! parameter
        integer                       :: rank       ! mpi
        integer                       :: nranks     ! mpi
        logical                       :: finished
        integer                       :: error
        type(triplesort_ints)         :: binkeep
        double precision     ,pointer :: hdkeep(:)
        real                 ,pointer :: trkeep(:)
        integer              ,pointer :: ntrlist(:)
        type(triplesort_ints),pointer :: binlist(:)
      end type tmerge_struct

      contains


!!----------------------------- create -------------------------------------!!
!!----------------------------- create -------------------------------------!!
!!----------------------------- create -------------------------------------!!


      subroutine tmerge_create (obj,no_mpi,nwih,ndpt,all_nodes,bin_first,bin_last,whoops)
      implicit none
      type(tmerge_struct),pointer     :: obj             ! arguments
      logical            ,intent(in)  :: no_mpi          ! arguments
      integer            ,intent(in)  :: nwih            ! arguments
      integer            ,intent(in)  :: ndpt            ! arguments
      logical            ,intent(in)  :: all_nodes       ! arguments
      integer            ,intent(in)  :: bin_first       ! arguments
      integer            ,intent(in)  :: bin_last        ! arguments
      logical            ,intent(out) :: whoops          ! arguments
      logical                         :: initialized     ! local

      allocate (obj)

      obj%no_mpi    = no_mpi
      obj%nwih      = nwih
      obj%ndpt      = ndpt
      obj%all_nodes = all_nodes
      obj%bin_first = bin_first
      obj%bin_last  = bin_last
      obj%finished  = .false.
      obj%error     = 0
      obj%binkeep   = HUGE(1)
      obj%nranks    = 1        ! reset below by mpi.
      obj%rank      = 0        ! reset below by mpi.
      initialized   = .false.

      if (.not.obj%no_mpi) then
           if (obj%error == 0) call mpi_initialized (initialized, obj%error)

           if (obj%error == 0 .and. .not.initialized) call mpi_init (obj%error)

           if (obj%error == 0) call mpi_comm_size (MPI_COMM_WORLD, obj%nranks, obj%error)
           if (obj%error == 0) call mpi_comm_rank (MPI_COMM_WORLD, obj%rank,   obj%error)
      endif

      allocate (obj%hdkeep(obj%nwih))
      allocate (obj%trkeep(obj%ndpt))
      allocate (obj%ntrlist(obj%nranks))
      allocate (obj%binlist(obj%nranks))

      if(obj%no_mpi) print *, 'TMERGE: no MPI (or only one CPU) in this job.'

      print *, "TMERGE: number of nodes   = ", obj%nranks
      print *, "TMERGE: this very node    = ", obj%rank
      print *, "TMERGE: MPI return status = ", obj%error

      if(obj%error  /= 0) print *, 'TMERGE: MPI error occurred.'
      if(obj%nranks == 1) print *, 'TMERGE: note that only one CPU is being used.'

      whoops = (obj%error /= 0 .or. obj%nranks == 1)
      return
      end subroutine tmerge_create


!!------------------------------- delete -----------------------------------!!
!!------------------------------- delete -----------------------------------!!
!!------------------------------- delete -----------------------------------!!


      subroutine tmerge_delete (obj)
      implicit none
      type(tmerge_struct),pointer :: obj       ! arguments

      deallocate (obj%hdkeep)
      deallocate (obj%trkeep)
      deallocate (obj%ntrlist)
      deallocate (obj%binlist)

      deallocate(obj)
      return
      end subroutine tmerge_delete


!!-------------------------- get output node -------------------------------!!
!!-------------------------- get output node -------------------------------!!
!!-------------------------- get output node -------------------------------!!

      function tmerge_get_output_node (obj, primary_bin) result (receiver)
      implicit none
      type(tmerge_struct),intent(inout) :: obj                ! arguments
      integer            ,intent(in)    :: primary_bin        ! arguments
      integer                           :: receiver           ! result

      if (obj%all_nodes) then
           receiver = 0
      else if (obj%bin_first == obj%bin_last) then
           receiver = 0
      else
           receiver = ((obj%nranks - 1) * (primary_bin - obj%bin_first)) / (obj%bin_last - obj%bin_first)
           if (receiver < 0) receiver = 0
           if (receiver >= obj%nranks) receiver = obj%nranks - 1
      endif
      return
      end function tmerge_get_output_node


!!------------------------------- execute ----------------------------------!!
!!------------------------------- execute ----------------------------------!!
!!------------------------------- execute ----------------------------------!!

! Each node will always have a trace to output when it is waiting at label 33,
! unless there are no more input traces on this node.  BINKEEP will always be
! set to the bin of the trace waiting to output from this node, or will be set
! to HUGE(1) if there are no more input traces on this node.  Therefore, if
! the minimum bin to output from any node is HUGE(1), this means there are no
! more traces to output from any node.

! This subroutine will continue to cycle internally without returning as long
! as this node has a trace waiting to output.  But if this node needs to output
! a trace originating on another node, it will return that trace, but then
! continue to cycle as soon as it is entered again (with ntr = NEED_TRACES).

      subroutine tmerge_execute (obj,ntr,hdo,tro,bin)
      implicit none
      type(tmerge_struct)  ,intent(inout) :: obj                ! arguments
      integer              ,intent(inout) :: ntr                ! arguments
      double precision     ,intent(inout) :: hdo(:)             ! arguments
      real                 ,intent(inout) :: tro(:)             ! arguments
      type(triplesort_ints),intent(in)    :: bin                ! arguments
      type(triplesort_ints)               :: minbin             ! local
      integer                             :: kount,irank        ! local
      integer                             :: sender,receiver    ! local
      integer,parameter                   :: hdtag = 6392       ! local
      integer,parameter                   :: trtag = 7851       ! local

!----------SAVE NEW INPUT TRACE RIGHT AWAY.

      if (ntr == NO_MORE_TRACES) then
           obj%finished = .true.                      ! we still might have a trace to output.
      else if (ntr == 1) then                         ! we have a new trace.
           obj%hdkeep(1:obj%nwih) = hdo(1:obj%nwih)
           obj%trkeep(1:obj%ndpt) = tro(1:obj%ndpt)
           obj%binkeep            = bin
      else if (obj%finished) then                     ! we still might have a trace to output.
           ! do nothing.
      else if (obj%binkeep == HUGE(1)) then           ! we need a trace.
           ntr = NEED_TRACES
           return
      endif

!----------GET BINS OF NEXT TRACE FROM EVERY NODE.

33    if(obj%no_mpi) then
           obj%binlist(1) = obj%binkeep
      else if(obj%error == 0) then
                                ! ----------input---------  ----------output-----------
                                ! value     num   type          values  num    type
                                !   |       |      |              |      |      |  
           call mpi_allgather (obj%binkeep, 3, MPI_INTEGER, obj%binlist, 3, MPI_INTEGER, MPI_COMM_WORLD, obj%error)
      else
           obj%binlist(1) = obj%binkeep
      endif

!----------GET NODE (0 to nranks-1) WHICH HAS THE NEXT TRACE (minimum bin) TO OUTPUT.

      sender = triplesort_minloc (obj%binlist, obj%nranks) - 1

!----------RETURN FROM ALL NODES IF ALL NODES ARE FINISHED.

      if (obj%binlist(sender+1) == HUGE(1)) then
           ntr = NO_MORE_TRACES
           return
      endif

!----------GET NODE (0 to nranks-1) WHICH WILL RECEIVE THE NEXT TRACE.

      receiver = tmerge_get_output_node (obj, bin%primary)

!----------MERGE ACROSS NODES.

      if (sender == receiver) then     ! always true if no mpi.
           if (sender == obj%rank) then
                hdo(1:obj%nwih) = obj%hdkeep(1:obj%nwih)
                tro(1:obj%ndpt) = obj%trkeep(1:obj%ndpt)
                obj%binkeep = HUGE(1)
                ntr = 1
              ! return (below)
           else
                goto 33
           endif
      else if (obj%error /= 0) then
           print *, "TMERGE: previous mpi error when trying to merge across nodes."
           ntr = FATAL_ERROR
         ! return (below)
      else if (sender == obj%rank) then
           call mpi_send (obj%hdkeep, obj%nwih, MPI_DOUBLE_PRECISION, receiver, hdtag, MPI_COMM_WORLD, obj%error)
           call mpi_send (obj%trkeep, obj%ndpt, MPI_REAL            , receiver, trtag, MPI_COMM_WORLD, obj%error)
           obj%binkeep = HUGE(1)
           ntr = NEED_TRACES
         ! return (below)
      else if (receiver == obj%rank) then
           call mpi_recv (hdo, obj%nwih, MPI_DOUBLE_PRECISION, sender  , hdtag, MPI_COMM_WORLD, obj%error)
           call mpi_recv (tro, obj%ndpt, MPI_REAL            , sender  , trtag, MPI_COMM_WORLD, obj%error)
           ntr = 1
         ! return (below)
      else
           goto 33
      endif

      return
      end subroutine tmerge_execute


!!----------------------------- end of module ------------------------------!!
!!----------------------------- end of module ------------------------------!!
!!----------------------------- end of module ------------------------------!!


      end module tmerge_module


!!--------------------------------- end ------------------------------------!!
!!--------------------------------- end ------------------------------------!!
!!--------------------------------- end ------------------------------------!!

