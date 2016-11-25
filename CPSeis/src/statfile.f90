!<CPS_v1 type="PRIMITIVE"/>
!!---------------------------- statfile.f90 --------------------------------!!
!!---------------------------- statfile.f90 --------------------------------!!
!!---------------------------- statfile.f90 --------------------------------!!


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
! Name       : STATFILE 
! Category   : math
! Written    : 2000-06-16   by: Tom Stoeckley
! Revised    : 2006-06-20   by: B. Menger
! Maturity   : production
! Purpose    : Manage a single static file for residual statics processes.
! Portability: No known limitations.
!
!-------------------------------------------------------------------------------
!</brief_doc>


!<descript_doc>
!-------------------------------------------------------------------------------
!                         GENERAL DESCRIPTION            
!
! This primitive encapsulates common code for managing a single static file
! for surface-consistent residual statics processes.  This includes the
! following duties:
!
!   (1) maintaining the cumulative statics from iteration to iteration.
!   (2) saving static files and correlation files.
!
! This primitive does not interact with the parameter cache.
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
!                             o     i         i        i        i        i
!     call statfile_create  (obj,procname,adjective,pathname,pathcorr,pathinc,
!                               i       i        i       i    i   i  i
!                            no_dead,num_iter,converge,ncorr,ipn,lun,dt,
!                             i   i  i  i   i    i   i  i
!                            nhx,nhy,x1,y1,xinc,yinc,nx,ny,
!                             i     i     i      o
!                            nrun,cards,ncards,error)
!
!                                      i
!     call statfile_print_header     (lun)
!
!                                           b    o
!     call statfile_begin_file_iteration  (obj,error)
!     call statfile_end_file_iteration    (obj,error)
!
!                                           b     i       o
!     call statfile_save_files            (obj,lunprint,error)
!     call statfile_close_files           (obj)
!
!                                   b   i   i     i      i    i   i    o
!     call statfile_report_static (obj,igp,corr,static,ccoef,nnt,nnc,error)
!
!        o                            i   i 
!     statcum = statfile_get_static (obj,igp)
!
!                             b
!     call statfile_delete  (obj)
!
!
! type(statfile_struct)  obj = pointer to the STATFILE structure.
! logical              error = error flag (true if an error occurred).
!
! character(len=*)  procname = name of process using this primitive.
! character(len=*) adjective = adjective such as 'SOURCE' or 'RECEIVER'.
! character(len=*)  pathname = name of static file to save.
! character(len=*)  pathcorr = name of correlation file to save.
! character(len=*)   pathinc = name of static increment file to save.
! logical            no_dead = whether to skip dead correlations.
! integer           num_iter = total number of iterations (or 0).
! real              converge = convergence criterion in milliseconds.
! integer              ncorr = number of values in correlation function.
! integer                ipn = process number (normally >=3) for history.
! integer                lun = logical unit number for printing (must be >0).
! real                    dt = sample interval in seconds.
! integer      nhx,nhy,nx,ny = the usual static parameters.
! real       x1,y1,xinc,yinc = the usual static parameters.
! integer               nrun = number of points in running average in X dir.
! character(len=*)  cards(:) = data cards to put in static file history.
! integer             ncards = number of data cards.
!
! integer                igp = index of static value (from 1 thru nx*ny).
! real           corr(ncorr) = trace correlation function.
! real                static = current    static value (sample interval units).
! real               statcum = cumulative static value (sample interval units).
! real                 ccoef = correlation coefficient (normalized).
! integer                nnt = number of traces used in the correlation.
! integer                nnc = number of correlations added together.
!
! STATFILE_CREATE:
!  (1) PATHNAME must be specified.
!  (2) PATHCORR can be STRING_EMPTY.
!  (3) PATHINC can be STRING_EMPTY.
!  (4) If PATHINC is specified, it is assumed that STATIC is a SISC-style
!       static increment, and a static increment file is also saved.
!  (5) The NRUN argument is used only if PATHINC is specified.
!
! STATFILE_REPORT_STATIC:
!  (1) STATIC is the static obtained in the current iteration.
!  (2) STATIC is added to the cumulative static.
!  (3) Should be called for each static value index.
!  (4) Must be called between the BEGIN and END iteration calls.
!  (5) If PATHINC is specified, STATIC is a SISC-style static increment.
!
! STATFILE_GET_STATIC:
!  (1) STATCUM is the cumulative static value summed over all iterations.
!  (2) Must be called between the BEGIN and END iteration calls.
!  (3) If PATHINC is specified, STATCUM will be the cumulative static after
!       integration and running average removal which was performed the last
!       time the END iteration call was made.
!  (4) If PATHINC is not specified, STATCUM will be the cumulative static
!       (without any integration but optionally with running average removed)
!       which existed the last time the END iteration call was made.
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
!                           REVISION HISTORY              
!
!     Date        Author       Description
!     ----        ------       -----------
!  5. 2006-06-20  B. Menger    Removed Unused Variables.
!  4. 2000-10-19  Stoeckley    Improve some error printouts to show file name.
!  3. 2000-08-23  Stoeckley    Change PRESERVE from true to false in
!                               STATFILE_END_FILE_ITERATION.
!  2. 2000-07-20  Stoeckley    Change starting location on correlation traces;
!                               replace PACK8IO with TEMPTFILE.
!  1. 2000-06-16  Stoeckley    Initial version.
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


!!-------------------------- start of module ------------------------------!!
!!-------------------------- start of module ------------------------------!!
!!-------------------------- start of module ------------------------------!!


      module statfile_module
      use named_constants_module
      use temptfile_module
      use permtfile_module
      use statio_module
      use statutil_module
      use string_module
      use mth_module
      use statcc_module
      use addext_module
      implicit none
      public
      private :: statfile_private_corrinit
      private :: statfile_private_addcorr
      private :: statfile_private_corrsave
      private :: statfile_private_statsave
      private :: statfile_statistics_init
      private :: statfile_statistics_update
      private :: statfile_statistics_finalize


      character(len=100),public,save :: STATFILE_IDENT = &
       '$Id: statfile.f90,v 1.5 2006/06/20 13:12:09 Menger prod sps $'


      type,public :: statfile_struct              
        private
        character(len=20)              :: procname,adjective        ! supplied
        character(len=FILENAME_LENGTH) :: pathname,pathcorr,pathinc ! supplied
        logical                        :: no_dead                   ! supplied
        real                           :: converge                  ! supplied
        integer                        :: num_iter,ncorr,ipn,lun    ! supplied
        integer                        :: nhx,nhy,nx,ny             ! supplied
        integer                        :: nrun                      ! supplied
        real                           :: dt,x1,y1,xinc,yinc        ! supplied
        character(len=80)     ,pointer :: cards(:)                  ! supplied
        integer                        :: ncards                    ! supplied
        integer                        :: ngp,ndpt,iter
        integer                        :: ngap,istep,nnmax
        real                           :: dtms
        character(len=80)              :: errmsg
        integer                        :: stat_good
        real                           :: stat_mean,stat_min,stat_max
        real                           :: sum_ccoef
        integer                        :: sum_nnt,sum_nnc,sum_kount
        real                  ,pointer :: cumstatics(:)
        real                  ,pointer :: usestatics(:)
        type(temptfile_struct),pointer :: temp
        type(permtfile_struct),pointer :: perm
      end type statfile_struct


      integer,parameter,private :: NWIH   = HDR_NOMINAL_SIZE
      real   ,parameter,private :: TSTRT  = 0.0
      integer,parameter,private :: NBITS  = 8


      contains


!!------------------------------ delete -----------------------------------!!
!!------------------------------ delete -----------------------------------!!
!!------------------------------ delete -----------------------------------!!


      subroutine statfile_delete (obj)
      implicit none
      type(statfile_struct),pointer :: obj       ! arguments

      if (.not.associated(obj)) return

      if (associated(obj%cards     )) deallocate (obj%cards     )
      if (associated(obj%cumstatics)) deallocate (obj%cumstatics)
      if (associated(obj%usestatics)) deallocate (obj%usestatics)

      call temptfile_close (obj%temp)
      call permtfile_close (obj%perm)

      deallocate(obj)
      return
      end subroutine statfile_delete


!!----------------------------- create ------------------------------------!!
!!----------------------------- create ------------------------------------!!
!!----------------------------- create ------------------------------------!!

      subroutine statfile_create (obj,procname,adjective,        &
                                  pathname,pathcorr,pathinc,     &
                                  no_dead,num_iter,converge,     &
                                  ncorr,ipn,lun,dt,              &
                                  nhx,nhy,x1,y1,xinc,yinc,nx,ny, &
                                  nrun,cards,ncards,error)
      implicit none
      type(statfile_struct),pointer     :: obj                        ! args
      character(len=*)     ,intent(in)  :: procname,adjective         ! args
      character(len=*)     ,intent(in)  :: pathname,pathcorr,pathinc  ! args
      logical              ,intent(in)  :: no_dead                    ! args
      real                 ,intent(in)  :: converge                   ! args
      integer              ,intent(in)  :: num_iter,ncorr,ipn,lun     ! args
      integer              ,intent(in)  :: nhx,nhy,nx,ny              ! args
      real                 ,intent(in)  :: dt,x1,y1,xinc,yinc         ! args
      integer              ,intent(in)  :: nrun                       ! args
      character(len=*)     ,intent(in)  :: cards(:)                   ! args
      integer              ,intent(in)  :: ncards                     ! args
      logical              ,intent(out) :: error                      ! args
      integer                           :: istat                      ! local

!----------initialize variables.

      allocate (obj)

      nullify (obj%cards)
      nullify (obj%cumstatics)
      nullify (obj%usestatics)
      nullify (obj%temp)
      nullify (obj%perm)

      obj%procname     = procname
      obj%adjective    = adjective
      obj%pathname     = pathname
      obj%pathcorr     = pathcorr
      obj%pathinc      = pathinc 
      obj%no_dead      = no_dead
      obj%num_iter     = num_iter
      obj%converge     = converge
      obj%ncorr        = ncorr  
      obj%ipn          = ipn
      obj%lun          = lun
      obj%dt           = dt    
      obj%nhx          = nhx     
      obj%nhy          = nhy     
      obj%x1           = x1       
      obj%y1           = y1       
      obj%xinc         = xinc   
      obj%yinc         = yinc   
      obj%nx           = nx    
      obj%ny           = ny    
      obj%nrun         = nrun
      obj%ncards       = ncards + 1

      call string_to_upper (obj%procname)
      call string_to_upper (obj%adjective)

      allocate (obj%cards(obj%ncards))
      obj%cards(1:obj%ncards-1) = cards(1:obj%ncards-1)
      obj%cards(obj%ncards)     = 'static file saved'
      
      obj%errmsg = 'STATFILE: '//trim(obj%adjective)//' STATIC ERROR'
      obj%ngp    = obj%nx * obj%ny
      obj%dtms   = obj%dt * 1000.0
      obj%ngap   = nint(0.1 / obj%dt)
      obj%istep  = obj%ncorr + 2 * obj%ngap - 1
      obj%ndpt   = 5 * obj%ngap + max(obj%num_iter,1) * obj%istep &
                                           + obj%ncorr + 1
      obj%iter   = 0
      obj%nnmax  = 0

!----------allocate the CUMSTATICS array.

      allocate (obj%cumstatics(obj%ngp),stat=istat)
      if (istat /= 0) then
           write(obj%lun,*) 'STATFILE: error allocating the CUMSTATICS array'
           write(obj%lun,*) trim(obj%errmsg)
           error = .true.
           return
      end if
      obj%cumstatics(:) = FNIL

!----------allocate the USESTATICS array.

      allocate (obj%usestatics(obj%ngp),stat=istat)
      if (istat /= 0) then
           write(obj%lun,*) 'STATFILE: error allocating the USESTATICS array'
           write(obj%lun,*) trim(obj%errmsg)
           error = .true.
           return
      end if
      obj%usestatics(:) = FNIL

!----------initialize the correlation file.

      call statfile_private_corrinit (obj,error)
      return
      end subroutine statfile_create


!!------------------- statfile begin file iteration ------------------------!!
!!------------------- statfile begin file iteration ------------------------!!
!!------------------- statfile begin file iteration ------------------------!!


      subroutine statfile_begin_file_iteration (obj,error)
      implicit none
      type(statfile_struct),intent(inout) :: obj            ! arguments
      logical              ,intent(out)   :: error          ! arguments

!----------initialize iteration.

      obj%iter       =  obj%iter + 1
      error          = .false.

!----------initialize statistics.

      call statfile_statistics_init (obj)
      return
      end subroutine statfile_begin_file_iteration


!!------------------------ statfile report static -------------------------!!
!!------------------------ statfile report static -------------------------!!
!!------------------------ statfile report static -------------------------!!


      subroutine statfile_report_static &
                         (obj,igp,corr,static,ccoef,nnt,nnc,error)
      implicit none
      type(statfile_struct),intent(inout) :: obj             ! arguments
      integer              ,intent(in)    :: igp,nnt,nnc     ! arguments
      real                 ,intent(in)    :: corr(:)         ! arguments
      real                 ,intent(in)    :: static,ccoef    ! arguments
      logical              ,intent(out)   :: error           ! arguments

!----------record the static.

      if (static == FNIL) then
           obj%cumstatics(igp) = FNIL
      else if (obj%cumstatics(igp) == FNIL) then
           obj%cumstatics(igp) = static
      else
           obj%cumstatics(igp) = obj%cumstatics(igp) + static
      end if

!----------update statistics.

      call statfile_statistics_update (obj,static,ccoef,nnt,nnc)

!----------add correlation function to the correlation file.

      call statfile_private_addcorr &
                       (obj,igp,corr,static,ccoef,nnt,nnc,error)
      return
      end subroutine statfile_report_static


!!--------------------- statfile end file iteration ------------------------!!
!!--------------------- statfile end file iteration ------------------------!!
!!--------------------- statfile end file iteration ------------------------!!


      subroutine statfile_end_file_iteration (obj,converged,error)
      implicit none
      type(statfile_struct),intent(inout) :: obj                   ! arguments
      logical              ,intent(out)   :: converged             ! arguments
      logical              ,intent(out)   :: error                 ! arguments


      obj%usestatics(:) = obj%cumstatics(:)

      if (obj%pathinc /= STRING_EMPTY) then
           call statutil_integrate (obj%nx,obj%ny,obj%usestatics, &
                                         preserve = .false.)
                !!!!!!!!!!!!!!!!!!!!     preserve = .true.)
           call statutil_runav     (obj%nx,obj%ny,obj%usestatics, &
                                         nxsmooth = obj%nrun,     &
                                         nysmooth = 1,            &
                                         endopt   = 'NN',         &
                                         trim     = 0.0,          &
                                         preserve = .false.)
                !!!!!!!!!!!!!!!!!!!!     preserve = .true.)
      end if

      call statfile_statistics_finalize (obj,converged)

      error = .false.
      return
      end subroutine statfile_end_file_iteration


!!------------------------- statfile save files ----------------------------!!
!!------------------------- statfile save files ----------------------------!!
!!------------------------- statfile save files ----------------------------!!


      subroutine statfile_save_files (obj,converged,error)
      implicit none
      type(statfile_struct),intent(inout) :: obj                   ! arguments
      logical              ,intent(in)    :: converged             ! arguments
      logical              ,intent(out)   :: error                 ! arguments
      logical                             :: error1,error2,error3  ! local

      call statfile_private_statsave &
                     (obj,obj%pathname,'RESID',obj%usestatics,converged,error1)

      call statfile_private_statsave &
                     (obj,obj%pathinc,'INC',obj%cumstatics,converged,error2)

      call statfile_private_corrsave (obj,converged,error3)

      error = (error1 .or. error2 .or. error3)
      return
      end subroutine statfile_save_files


!!------------------------ statfile close files --------------------------!!
!!------------------------ statfile close files --------------------------!!
!!------------------------ statfile close files --------------------------!!


      subroutine statfile_close_files (obj)
      implicit none
      type(statfile_struct),intent(inout) :: obj              ! arguments

      call temptfile_close (obj%temp)
      return
      end subroutine statfile_close_files


!!------------------------ statfile get static ----------------------------!!
!!------------------------ statfile get static ----------------------------!!
!!------------------------ statfile get static ----------------------------!!


      function statfile_get_static (obj,igp) result (statuse)
      implicit none
      type(statfile_struct),intent(in)    :: obj             ! arguments
      integer              ,intent(in)    :: igp             ! arguments
      real                                :: statuse         ! result
 
      statuse = obj%usestatics(igp)
      return
      end function statfile_get_static


!!---------------------- statfile private corrinit ------------------------!!
!!---------------------- statfile private corrinit ------------------------!!
!!---------------------- statfile private corrinit ------------------------!!


      subroutine statfile_private_corrinit (obj,error)
      implicit none
      type(statfile_struct),intent(inout) :: obj            ! arguments
      logical              ,intent(out)   :: error          ! arguments
      double precision                    :: hd(NWIH)       ! local
      real                                :: tr(obj%ndpt)   ! local
      integer                             :: igp      ,err ! local
      real                                :: xgp,ygp        ! local

!----------return if not outputting correlations.

      if (obj%pathcorr == STRING_EMPTY) then
           error = .false.
           return
      end if

!----------open temporary correlation file.

      call temptfile_open (obj%temp,'statfile_corrs',NWIH,obj%ndpt,obj%lun,err)
      if (err /= TEMPTFILE_OK) then
           write(obj%lun,*) 'STATFILE: temporary correlation file open error'
           write(obj%lun,*) trim(obj%errmsg)
           error = .true.
           return
      end if

!----------initialize the correlation traces.

      hd(:) = 0.0
      tr(:) = 0.0

      hd(39) = 2
      hd(40) = obj%ngp
!     hd(40) = ns(1)+1
!     hd(41) = ns(1)
!     hd(42) = mid+1
!     hd(43) = ns(1)+ns(2)+2
!     hd(44) = ns(2)

!----------initialize the temporary correlation file.

      do igp = 1,obj%ngp

           xgp = obj%x1 + (igp-1) * obj%xinc
           ygp = obj%y1 + (igp-1) * obj%yinc

           hd( 1) = igp
           hd( 9) = xgp     !!!!!!!!! need to deal with possible Y header word.
           hd(37) = xgp     !!!!!!!!! need to deal with possible Y header word.
           hd(46) = xgp     !!!!!!!!! need to deal with possible Y header word.
           hd(47) = xgp     !!!!!!!!! need to deal with possible Y header word.
           call temptfile_write8 (obj%temp,igp,hd,tr,err)
           if (err /= TEMPTFILE_OK) then
             write(obj%lun,*) 'STATFILE: temporary correlation file write error'
             write(obj%lun,*) trim(obj%errmsg)
             error = .true.
             return
           end if

      end do

!----------finish up and return.

      error = .false.
      return
      end subroutine statfile_private_corrinit


!!---------------------- statfile private addcorr --------------------------!!
!!---------------------- statfile private addcorr --------------------------!!
!!---------------------- statfile private addcorr --------------------------!!


      subroutine statfile_private_addcorr &
                         (obj,igp,corr,static,ccoef,nnt,nnc,error)
      implicit none
      type(statfile_struct),intent(inout) :: obj             ! arguments
      integer              ,intent(in)    :: igp,nnt,nnc     ! arguments
      real                 ,intent(in)    :: corr(:)         ! arguments
      real                 ,intent(in)    :: static,ccoef    ! arguments
      logical              ,intent(out)   :: error           ! arguments
      double precision                    :: hd(NWIH)        ! local
      real                                :: tr(obj%ndpt)    ! local
      real                                :: peak            ! local
      integer                             :: istart,icorr    ! local
      integer                             :: kb,kc,err       ! local

!----------return if not outputting correlations.

      if (obj%pathcorr == STRING_EMPTY) then
           error = .false.
           return
      end if

!----------read the correlation trace from the temporary correlation file.

      call temptfile_read8 (obj%temp,igp,hd,tr,err)
      if (err /= TEMPTFILE_OK) then
           write(obj%lun,*) 'STATFILE: temporary correlation file read error'
           write(obj%lun,*) 'STATFILE: during iteration ',obj%iter
           write(obj%lun,*) trim(obj%errmsg)
           error = .true.
           return
      end if

!----------work with ground position and fold of stack.

      if (obj%iter == 1) then
           hd( 5) = nnt
           hd(27) = nnc
           obj%nnmax = max(obj%nnmax,nnt,nnc)
      end if

!----------work with correlation coefficient and static increment.

      istart = 3 * obj%ngap + (obj%iter - 1) * obj%istep
      if (obj%iter <= 9) then
           tr( 2 + obj%iter) = ccoef
           hd(17 + obj%iter) = ccoef
           hd(27 + obj%iter) = (istart+(obj%ncorr+1)/2-1)*obj%dt
           if (static == FNIL) then
                hd(48 + obj%iter) = FNIL
           else
                hd(48 + obj%iter) = static * obj%dtms
           end if
      end if
      if (static == FNIL .and. ccoef == 0.0) then
           go to 888
      else
           hd(6) = 1.0     ! flag to show that the correlation is not dead.
      end if
      if (istart + obj%istep > obj%ndpt) go to 888

!----------plot static increment on correlation trace.

      kb     = istart + obj%ncorr + obj%ngap
      tr(kb) = 0.15
      kc     = kb - nint(static)
      kc     = min(kb+obj%ngap-1,max(kb-obj%ngap+1,kc))
      if (static == FNIL .and. ccoef > 0.0) then
           tr(kc-2:kc+2) = 1.0
      else
           tr(kc) = 1.0
      end if

!----------put correlation function onto correlation trace.

      peak = 0.0
      do icorr = 1,obj%ncorr
           peak = max(abs(corr(icorr)),peak)
      end do
      if (peak == 0.0) go to 888
      do icorr = 1,obj%ncorr
 !!!!      tr(istart+icorr) = corr(icorr)/peak
           tr(istart+icorr) = corr(obj%ncorr-icorr+1)/peak
      end do

!----------write the correlation trace to the temporary correlation file.

888   continue

      call temptfile_write8 (obj%temp,igp,hd,tr,err)
      if (err /= TEMPTFILE_OK) then
           write(obj%lun,*) 'STATFILE: temporary correlation file write error'
           write(obj%lun,*) 'STATFILE: during iteration ',obj%iter
           write(obj%lun,*) trim(obj%errmsg)
           error = .true.
           return
      end if

      error = .false.
      return
      end subroutine statfile_private_addcorr


!!------------------------ statfile private corrsave ---------------------!!
!!------------------------ statfile private corrsave ---------------------!!
!!------------------------ statfile private corrsave ---------------------!!


      subroutine statfile_private_corrsave (obj,converged,error)
      implicit none
      type(statfile_struct),intent(inout) :: obj                 ! arguments
      logical              ,intent(in)    :: converged           ! arguments
      logical              ,intent(out)   :: error               ! arguments
      double precision                    :: hd(NWIH)            ! local
      real                                :: tr(obj%ndpt)        ! local
      integer                             :: err,ka,kb,kc,igp    ! local
      integer                             :: lunprint            ! local

!----------return if not outputting correlations.

      if (obj%pathcorr == STRING_EMPTY) then
           error = .false.
           return
      end if

!----------prepare to output correlations.

      if (obj%iter >= obj%num_iter .or. converged) then
           lunprint = obj%lun
      else
           lunprint = 0
      end if

!----------open permanent correlation file.

      call permtfile_open_write (obj%perm,obj%pathcorr,NWIH,obj%ndpt, &
                                 TSTRT,obj%dt,lunprint,err,obj%ipn,NBITS)
      if (err /= PERMTFILE_OK) then
           write(obj%lun,*) 'STATFILE: permanent correlation file open error'
           write(obj%lun,*) 'STATFILE: after iteration ',obj%iter
           write(obj%lun,*) 'STATFILE: filename = ',trim(obj%pathcorr)
           write(obj%lun,*) trim(obj%errmsg)
           error = .true.
           return
      end if

!----------start loop reading correlations from temporary correlation file.

      do igp = 1,obj%ngp

        call temptfile_read8 (obj%temp,igp,hd,tr,err)
        if (err /= TEMPTFILE_OK) then
             write(obj%lun,*) 'STATFILE: temporary correlation file read error'
             write(obj%lun,*) 'STATFILE: after iteration ',obj%iter
             write(obj%lun,*) trim(obj%errmsg)
             error = .true.
             return
        end if
        if (obj%no_dead .and. hd(6) == 0.0) cycle

!----------plot integrated (or final) static on correlation trace.

        hd(48)   = obj%usestatics(igp) * obj%dtms
        kb       = 2 * obj%ngap + 1
        tr(kb)   = 0.15
        kc       = kb - nint(obj%usestatics(igp))
        kc       = min(kb+obj%ngap-1,max(kb-obj%ngap+1,kc))
        tr(kc)   = 1.0
        tr(kc+1) = 0.3
        tr(kc-1) = 0.3

!----------plot fold of stack on correlation trace.

        kc = obj%ngap + 1
        ka = kc - 0.4 * kc * hd( 5) / obj%nnmax
        kb = kc - 0.4 * kc * hd(27) / obj%nnmax
        tr(ka:kc) = 0.2
        tr(kb:kc) = 0.4

!----------end loop writing correlations to permanent correlation file.

        call permtfile_write (obj%perm,hd,tr,err)
        if (err /= PERMTFILE_OK) then
             write(obj%lun,*) 'STATFILE: permanent correlation file write error'
             write(obj%lun,*) 'STATFILE: while adding correlation ',igp
             write(obj%lun,*) 'STATFILE: after iteration ',obj%iter
             write(obj%lun,*) 'STATFILE: filename = ',trim(obj%pathcorr)
             write(obj%lun,*) trim(obj%errmsg)
             error = .true.
             return
        end if

      end do

!----------close permanent correlation file.

      call permtfile_close (obj%perm)

      error = .false.
      return
      end subroutine statfile_private_corrsave


!!------------------------ statfile private statsave -----------------------!!
!!------------------------ statfile private statsave -----------------------!!
!!------------------------ statfile private statsave -----------------------!!


      subroutine statfile_private_statsave &
                             (obj,path,stattype,statics,converged,error)
      implicit none
      type(statfile_struct),intent(inout) :: obj               ! arguments
      character(len=*)     ,intent(in)    :: path,stattype     ! arguments
      real                 ,intent(inout) :: statics(:)        ! arguments
      logical              ,intent(in)    :: converged         ! arguments
      logical              ,intent(out)   :: error             ! arguments
      integer                             :: igp,err,lunprint  ! local
      character(len=80)                   :: msg               ! local

!----------return if not outputting static file.

      if (path == STRING_EMPTY) then
           error = .false.
           return
      end if

!----------prepare to output static file.

      if (obj%iter >= obj%num_iter .or. converged) then
           lunprint = obj%lun
      else
           lunprint = 0
      end if

      if (obj%num_iter >= 1) then
        if (converged) then
          call string_encode (obj%cards(obj%ncards),  &
          'static file converged after',obj%iter,'of',obj%num_iter,'iterations')
        else
          call string_encode (obj%cards(obj%ncards),  &
          'static file saved after',obj%iter,'of',obj%num_iter,'iterations')
        end if
      end if

!----------output static file.

      do igp = 1,obj%ngp
          if (statics(igp) /= FNIL) then
               statics(igp) = statics(igp) * obj%dtms
          end if
      end do

      call statio_write_file (path,stattype,                      &
                              obj%nhx,obj%nhy,0,0,                &
                              obj%x1,obj%y1,obj%xinc,obj%yinc,    &
                              obj%nx,obj%ny,statics,              &
                              err,msg,obj%cards,obj%ncards,       &
                              obj%procname,lunprint)

      do igp = 1,obj%ngp
          if (statics(igp) /= FNIL) then
               statics(igp) = statics(igp) / obj%dtms
          end if
      end do

      if (err /= STATIO_OK) then
           write(obj%lun,*) 'STATFILE: ',trim(msg)
           write(obj%lun,*) 'STATFILE: error saving static file ',trim(path)
           write(obj%lun,*) 'STATFILE: after iteration ',obj%iter
           write(obj%lun,*) trim(obj%errmsg)
           error = .true.
           return
      end if

      error = .false.
      return
      end subroutine statfile_private_statsave


!!----------------------- private statistics -----------------------------!!
!!----------------------- private statistics -----------------------------!!
!!----------------------- private statistics -----------------------------!!


      subroutine statfile_statistics_init (obj)
      implicit none
      type(statfile_struct),intent(inout) :: obj             ! arguments

      obj%stat_good =  0
      obj%stat_mean =  0.0
      obj%stat_min  =  99999.9
      obj%stat_max  = -99999.9
      obj%sum_ccoef = 0.0
      obj%sum_nnt   = 0
      obj%sum_nnc   = 0
      obj%sum_kount = 0
      return
      end subroutine statfile_statistics_init


                          !!!!!!!!!!!!!!!!!!!!!!!!


      subroutine statfile_statistics_update (obj,static,ccoef,nnt,nnc)
      implicit none
      type(statfile_struct),intent(inout) :: obj             ! arguments
      real                 ,intent(in)    :: static,ccoef    ! arguments
      integer              ,intent(in)    :: nnt,nnc         ! arguments

      if (static /= FNIL) then
           obj%stat_good = obj%stat_good + 1
           obj%stat_mean = obj%stat_mean + abs(static)
           obj%stat_min  = min(obj%stat_min,static)
           obj%stat_max  = max(obj%stat_max,static)
      end if
      if (ccoef > 0.0 .or. nnt > 0 .or. nnc > 0) then
           obj%sum_ccoef = obj%sum_ccoef + ccoef
           obj%sum_nnt   = obj%sum_nnt   + nnt
           obj%sum_nnc   = obj%sum_nnc   + nnc
           obj%sum_kount = obj%sum_kount + 1
      end if
      return
      end subroutine statfile_statistics_update


                          !!!!!!!!!!!!!!!!!!!!!!!!


      subroutine statfile_statistics_finalize (obj,converged)
      implicit none
      type(statfile_struct),intent(inout) :: obj                 ! arguments
      logical              ,intent(out)   :: converged           ! arguments
      integer                             :: igp,nnt,nnc         ! local
      real                                :: static,ccoef        ! local
      integer                             :: cum_good            ! local
      real                                :: cum_mean            ! local
      real                                :: cum_min             ! local
      real                                :: cum_max             ! local
      integer                             :: stat_nil,cum_nil    ! local
      integer                             :: stat_fail           ! local
      character(len=10)                   :: msg                 ! local

      if (obj%sum_kount > 0) then
           ccoef = obj%sum_ccoef / obj%sum_kount
           nnt   = obj%sum_nnt   / obj%sum_kount
           nnc   = obj%sum_nnc   / obj%sum_kount
      else
           ccoef = 0.0
           nnt   = 0
           nnc   = 0
      end if

      cum_good  =  0
      cum_mean  =  0.0
      cum_min   =  99999.9
      cum_max   = -99999.9

      do igp = 1,obj%ngp
           static = obj%cumstatics(igp)
           if (static /= FNIL) then
                cum_good = cum_good + 1
                cum_mean = cum_mean + abs(static)
                cum_min  = min(cum_min,static)
                cum_max  = max(cum_max,static)
           end if
      end do

      if (obj%stat_good > 0) then
           obj%stat_mean = obj%stat_mean / obj%stat_good
      else
           obj%stat_min = 0.0
           obj%stat_max = 0.0
      end if
      if (cum_good > 0) then
           cum_mean = cum_mean / cum_good
      else
           cum_min = 0.0
           cum_max = 0.0
      end if

      stat_nil  = obj%ngp       - obj%stat_good
      cum_nil   = obj%ngp       - cum_good
      stat_fail = obj%sum_kount - obj%stat_good

      converged = (obj%stat_good > 0 .and. &
            max(abs(obj%stat_min),abs(obj%stat_max))*obj%dtms < obj%converge)

      if (converged) then
           msg = 'converged'
      else
           msg = ' '
      end if

      obj%stat_mean = obj%stat_mean * obj%dtms
      obj%stat_min  = obj%stat_min  * obj%dtms
      obj%stat_max  = obj%stat_max  * obj%dtms
      cum_mean      = cum_mean      * obj%dtms
      cum_min       = cum_min       * obj%dtms
      cum_max       = cum_max       * obj%dtms

      write(obj%lun,5000) obj%iter,obj%adjective,obj%ngp,           &
                          obj%stat_mean,obj%stat_min,obj%stat_max,  &
                          obj%stat_good,stat_nil,stat_fail,         &
                          ccoef,nnt,nnc,                            &
                          cum_mean,cum_min,cum_max,                 &
                          cum_good,cum_nil,msg
5000  format (1x,i3,1x,a9,i5,             &
              3f7.1,2x,3i5,1x,f6.2,i6,i5, &
              3f7.1,2x,2i5,1x,a10)
      return
      end subroutine statfile_statistics_finalize


!!------------------------- statfile print header --------------------------!!
!!------------------------- statfile print header --------------------------!!
!!------------------------- statfile print header --------------------------!!


      subroutine statfile_print_header (lunprint)
      implicit none
      integer,intent(in) :: lunprint                  ! arguments

      write(lunprint,*) ' '
      write(lunprint,1000)
      write(lunprint,2000)
      write(lunprint,*) ' '

1000  format (22x,'+++++++++++++++++++++static change++++++++++++++++++++', &
                  '  ++++++cumulative statics++++++')
2000  format (1x,'iteration',6x,'#GP',2x               ,  &
               ' mean    min    max  #good #nil #fail' ,  &
               ' ccoef #trcs #corr '                   ,  &
               ' mean    min    max  #good #nil')
      return
      end subroutine statfile_print_header


!!----------------------------- end of module -----------------------------!!
!!----------------------------- end of module -----------------------------!!
!!----------------------------- end of module -----------------------------!!


      end module statfile_module


!!--------------------------------- end -----------------------------------!!
!!--------------------------------- end -----------------------------------!!
!!--------------------------------- end -----------------------------------!!

