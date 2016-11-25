!<CPS_v1 type="PROCESS"/>
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
!                         C P S   P R O C E S S             
!
! Name       : MFRS    (Model-Free Refraction Statics)
! Category   : statics
! Written    : 1998-06-01   by: Charles Emmons
! Revised    : 2006-10-16   by: D. Glover
! Maturity   : production
! Purpose    : 2D and 3D model-free refraction statics solution program.
! Portability: No known limitations.
! Parallel   : No
!
!-------------------------------------------------------------------------------
!</brief_doc>


!<descript_doc>
!-------------------------------------------------------------------------------
!                          GENERAL DESCRIPTION              
!
!
! MFRS reads a standard first break pick file and uses a Gauss-Seidel iteration
! to decompose the first break times into four independent terms: source,
! receiver, offset and elevation difference.  MFRS does not require the user
! to specify any parameters pertaining to a near surface model, nor does the 
! program build any such model internally.  In particular, no estimates of the
! weathered layer thickness or velocity are required.
!
!
! Gauss-Seidel Setup Steps
!
!     1.  The decomposition starts by assigning to each offset bin the average
!     first break time for the traces associated with that bin.  Discard outlier
!     values (as described in the Discarding Outliers section below) and 
!     recalculate average first break time.  Repeat NUM_OUTLY times.  The 
!     result is the correction term for each offset bin.
!
!     2.  Calculate the source average datum correction terms and the receiver
!     average datum correction terms as described in the Elevation Difference
!     Term section below.  (At this point no source or receiver terms are used
!     since they have not yet been calculated.)
!
!     3.  For each source location, form the average residual first break time
!     after applying the interpolated offset correction, the source average
!     datum correction terms and the receiver average datum correction terms. 
!     Iterate discarding outliers and recalculating the average residual first
!     break time as before.  This is the correction term for each source.
!
!     4.  For each receiver location, form the average residual first break
!     time after applying the interpolated offset correction, the source
!     correction, the source average datum correction term and the receiver
!     average datum correction terms.  Iterate discarding outliers and 
!     recalculating the average residual first break time as before.  This is
!     the correction term for each receiver.
!
!
! Main Gauss-Seidel Iteration Loop 
!
!     1.  For each offset bin, form the average residual first break time after
!     applying the latest source term, receiver term and source and receiver 
!     shifts to average datum.  Iterate discarding outliers and recalculating 
!     the average residual first break time.  This is the new term for each
!     offset bin. 
!
!     2.  Calculate the new source average datum correction terms and the new
!     receiver average datum correction terms as described in the Elevation 
!     Difference Term section below.
!
!     3.  For each source, form the average residual first break time after
!     applying the latest interpolated offset term, receiver term and source 
!     and receiver shifts to average datum.  Iterate discarding outliers and 
!     recalculating the average residual first break time.  This is the new 
!     term for each source.
!    
!     4.  For each receiver, form the average residual first break time after
!     applying the latest interpolated offset term, source term and source 
!     and receiver shifts to average datum.  Iterate discarding outliers and 
!     recalculating the average residual first break time.  This is the new 
!     term for each receiver.
! 
!     5.  Unless this is the last iteration, repeat steps 1 - 4.
!
!
! Discarding Outliers
!
! Each correction term recalculation step in each Gauss-Seidel iteration 
! identifies and discards outliers as follows.  
!
!     1.  The correction term is first calculated as the average of the 
!     residual first break times after the latest terms are applied.  
!
!     2.  Calculate the standard deviation of trace residual first break times.
!
!     3.  Ignore traces with residual first break times greater than the 
!     threshold.  The threshold for the last outlier identification iteration
!     is one standard deviation.  If more than one iteration is specified, the
!     threshold is two standard deviations for the first iteration, decreasing
!     to one standard deviation for the last iteration.
!
!     4.  Recalculate the correction term after discarding the outliers.
!
!     5.  Unless this is the last iteration, repeat steps 1 - 4.
!
!
! Elevation Difference Term
!
! For each Gauss-Seidel iteration, a non-linear function is derived relating
! residual time to the elevation difference between either source or receiver
! and the average elevation.
!
!     1.  Apply the latest interpolated offset term, source term and receiver
!     term to selected first break times.
!
!     2.  Find the difference between residual times for receivers at different
!     elevations within the same shot profiles.  Ignore data points that are
!     inconsistent with a reasonable range of velocity.
!
!     3.  Derive a non-linear equation by performing a least squares fit of
!     these residual time differences as a function of receiver elevation 
!     difference.  Iterate discarding outliers and redoing the least squares
!     fit.
!
!     4.  Use the non-linear equation to calculate times for correcting sources
!     to the average datum elevation (source average datum correction term) and 
!     for correcting receivers to the average datum elevation (receiver average
!     datum correction term).   
!
! The equation relating correction time with elevation difference must be
! non-linear because times for very large elevation differences are smaller
! than would be predicted using linear extrapolation from times associated with
! small elevation differences.
!
!
! Source and Receiver Statics
!
! After the Gauss-Seidel iterations are complete, source static corrections are
! calculated as the sum of the latest source term, source average datum 
! correction term and a correction to shift from the average datum to the 
! desired final datum.
!
! Receiver static corrections are calculated as the sum of the latest receiver
! term, receiver average datum correction term and a correction to shift from 
! the average datum to the desired final datum.
!
!
! Output Files
!
! Four static files are written by MFRS: a source static file with .src 
! extension, a receiver static file with .rec extension, an offset term file
! with .off extension and an elevation difference term file with a .elev 
! extension.  
!
! At the completion of the Gauss-Seidel iterations, MFRS writes five diagnostic
! files containing statistical information about the Gauss-Seidel iteration: a 
! source diagnostic file with .diag.src extension, a receiver diagnostic file
! with .diag.rec extension, an offset diagnostic file with .diag.off extension,
! an elevation difference diagnostic file with .diag.elev extension and a
! reference file with a .diag.ref extension that contains information that
! can be used to calculate the statics files.
!
!-------------------------------------------------------------------------------
!</descript_doc>


!<advice_doc>
!-------------------------------------------------------------------------------
!                          ADVICE FOR USERS                 
!
!
! Correction for First Break Flattening
!
! First break data is frequently flattened by applying a static file containing
! time shifts as a function of offset.  Normally this static file contains 
! positive time shifts and is SUBTRACTED from the data to flatten it.  MFRS 
! will add the shift contained in the PATH_STATIC file to correct the picks 
! back to zero time reference.
!
! Usually a bulk shift is also applied to prevent data from being lost off the
! top of the trace.   Normally this bulk shift applied prior to first break 
! picking is positive.  MFRS subtracts the value of the BULK parameter to 
! correct the picks back to zero time reference.
!
!
! Mode of Operation
!
! Three modes of operation are available.
!
! If MODE=ITER, then run only the iteration portion of the program and write 
! the diagnostic files including the .diag.ref file.
!
! If MODE=STATIC, then iterations have already been run.  MFRS will read the
! .diag.ref file and create static files.
!
! If MODE=ALL, run the entire program.
!
!-------------------------------------------------------------------------------
!</advice_doc>


!<trace_in_doc>
!-------------------------------------------------------------------------------
!                        TRACE INPUT REQUIREMENTS           
!
! Process is a setup process only.
!
!-------------------------------------------------------------------------------
!</trace_in_doc>


!<trace_out_doc>
!-------------------------------------------------------------------------------
!                      TRACE OUTPUT CHARACTERISTICS         
!
! Process is a setup process only.
!
!-------------------------------------------------------------------------------
!</trace_out_doc>


!<global_doc>
!-------------------------------------------------------------------------------
!       PROJECT DATA, JOB DATA, AND GLOBAL PARAMETERS USED OR CHANGED      
!
! None used.
!
!-------------------------------------------------------------------------------
!</global_doc>


!<header_word_doc>
!-------------------------------------------------------------------------------
!                    TRACE HEADER WORDS USED OR CHANGED     
!
! Process is a setup process only.
!
!-------------------------------------------------------------------------------
!</header_word_doc>


!<history_doc>
!-------------------------------------------------------------------------------
!                             REVISION HISTORY                
!
!     Date        Author     Description
!     ----        ------     -----------
!007. 2006-10-16  D. Glover  Added NULLIFY statements for Intel compiler.
!006. 2006-01-10  B. Menger  Removed Unused Variables.
!  5. 2004-01-29  Emmons     Set program to add datum correction to shot
!                            and receiver static files.
!  4. 2002-09-23  Goodger    Use mth_module for binning.  If METH_OFF was
!                            FIND, MFRS was using the same calc as the mth_
!                            module for OFF_TOT.  However, if METH_OFF was
!                            SPECIFY, then OFF_TOT was different (less).
!  3. 2002-04-22  Goodger    Default num_iter to 100.  Make path_static always
!                            sensitive.
!  2. 2002-04-16  Emmons     First betalib version
!                 K. Goodger Conversion to cps.
!                 CIBurch    Documentation
!  1. 1998-06-01  C. Emmons  Initial F77 version.
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


!<compile_doc>
!-------------------------------------------------------------------------------
!                     SPECIAL COMPILING REQUIREMENTS        
!
! No special requirements.
!
!
!-------------------------------------------------------------------------------
!</compile_doc>


!<calling_doc>
!-------------------------------------------------------------------------------
!                    SPECIFIC CALLING CHARACTERISTICS       
! Control
! Parameter     Value
! Name          Reported   Description
! ---------     --------   -----------
! NTAPES           0       number of magnetic tapes needed.
! NEED_REQUEST   false     whether this process ever needs to request traces.
! NEED_LABEL     false     whether this process needs a label.     
! TWOSETS        false     whether this process needs two trace/header arrays.
! NSCRATCH         0       amount of temporary memory needed.       
! NSTORE           0       amount of permanent memory needed.      
! IFTD           false     whether this process frees tape drives. 
! NDISK            0       disk space needed (megabytes) if large. 
! SETUP_ONLY     true      whether this process is setup-only.    
! PARALLEL_SAFE  false     whether this process can be in a parallelized loop.
!
!-------------------------------------------------------------------------------
!</calling_doc>


!<int_calling_doc>
!-------------------------------------------------------------------------------
!                   ALTERNATE INTERNAL CALLING METHODS     
!
!  None provided.
!
!
!-------------------------------------------------------------------------------
!</int_calling_doc>


!<algorithm_doc>
!-------------------------------------------------------------------------------
!                  ALGORITHM DESCRIPTION FOR DEVELOPERS    
!
!
!
!-------------------------------------------------------------------------------
!</algorithm_doc>


!<programming_doc>
!-------------------------------------------------------------------------------
!                           PROGRAMMING NOTES              
!
!
!-------------------------------------------------------------------------------
!</programming_doc>


!-------------------------------------------------------------------------------
!<gui_def>
!<NS MFRS Process/NC=80>
!
!                   Model-free Refraction Statics Process
!
!Select PATH_PICK[PATH_PICK] =`QSSSSSSSSSSSSSSSSSSSSSSSSSSSSSSSSSSSSSSSSSSSSSSSSSSSSSSSSSSS
!                   [PATH_PICK_INFO]`XXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXX
!
!   PATH_OUT~~=`SSSSSSSSSSSSSSSSSSSSSSSSSSSSSSSSSSSSSSSSSSSSSSSSSSSSSSSSSSS
!
!   METH_OFF~~=`CCCCCCC  MODE~~~~=`CCCCCC  OPT_ITER=`CCCC
!
!   OFF_INIT~~=`FFFFFFF  OFF_LAST=`FFFFFFF  OFF_INC=`FFFFFFF  OFF_TOT=`IIIIIII
!         
!   NUM_ITER~~=`IIIIIIIII       NUM_OUTLY~~=`IIIIIIIII
!      
!   DATUM~~~~~=`FFFFFFFFF       VEL_CORR~~~=`FFFFFFFFF        BULK~~~=`IIIIIII
!   
!Select PATH_STATIC[PATH_STATIC] =`QSSSSSSSSSSSSSSSSSSSSSSSSSSSSSSSSSSSSSSSSSSSSSSSSSSSSSSSSSSS
!                   [PATH_STATIC_INFO]`XXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXX
!
!<PARMS PATH_PICK[/ML=128/XST]>
!<PARMS PATH_STATIC[/ML=128/XST]>
!</gui_def>
!-------------------------------------------------------------------------------


!-------------------------------------------------------------------------------
!<HelpSection>
!
!<HELP KEYWORD="PATH_PICK_INFO"  TYPE= "DISPLAY_ONLY">
!<Tip> Status of PATH_PICK. </Tip>
!</Help>
!
!<Help KEYWORD="SELECT_PATH_PICK">
!<Tip> Choose PATH_PICK using a file selection dialog box. </Tip>
!</Help>
!
!<Help KEYWORD="PATH_PICK">
!<Tip> Pathname for the first break pick file written by CBYT. </Tip>
! Default = None
! Allowed = char
!</Help>
!
!<Help KEYWORD="PATH_OUT">
!<Tip> Pathname and root filename for the output files. </Tip>
! Default = None
! Allowed = char
! Pathname and root filename for the output statics and diagnostic files.  Each
! file will be provided with a unique extension as shown in General Description.
!
! If MODE = STATIC, then PATH_OUT also specifies the pathname of the .diag.ref 
! file that will be used to calculate the output static files.
!</Help>
!
!<Help KEYWORD="METH_OFF">
!<Tip> Method for determining OFF_INIT and OFF_LAST. </Tip>
! Default = SPECIFY
! Allowed = SPECIFY or FIND
! If METH_OFF=SPECIFY, the user must explicitly specify OFF_INIT and OFF_LAST.
!
! If METH_OFF=FIND, the program will determine these parameters from the data.
!</Help>
!
!<Help KEYWORD="MODE">
!<Tip> Select mode of operation. </Tip>
! Default = ALL
! Allowed = ALL, ITER, STATIC
! If MODE=ITER, then run only the iteration portion of the program and write 
! the diagnostic files including the .diag.ref file.
!
! If MODE=STATIC, then iterations have already been run.  MFRS will read the
! .diag.ref file and create static files.
!
! If MODE=ALL, run the entire program.
!</Help>
!
!<Help KEYWORD="OPT_ITER">
!<Tip> Whether to determine the number of G_S iterations automatically. </Tip>
! Default = YES
! Allowed = YES/NO
! If OPT_ITER = YES, then iterations continue until no change is observed in
! the second decimal place of the standard deviation for all terms for at
! least 5 consecutive iterations.
!
! If OPT_ITER = NO, then the number of iterations is determined by the NUM_ITER
! parameter.
!</Help>
!
!<Help KEYWORD="NUM_ITER">
!<Tip> Number of Gauss-Seidel iterations to perform. </Tip>
! Default = 100
! Allowed = int > 0
! Number of Gauss-Seidel iterations to perform.  15 - 20 iterations is usually
! sufficient.
!
! Active only if OPT_ITER = NO.
!</Help>
!
!<Help KEYWORD="NUM_OUTLY">
!<Tip> Number of outlier discard iterations to perform. </Tip>
! Default = 2
! Allowed = int > 0
! Number of outlier discard iterations for each term in each Gauss-Seidel 
! iteration.   2 iterations is usually sufficient.
!</Help> 
!
!<Help KEYWORD="OFF_INIT">
!<Tip> Value of offset at the center of the first offset bin. </Tip>
! Default = 1.0
! Allowed = real
! Value of offset at the center of the first offset bin.  OFF_INIT and OFF_LAST
! define the range of offset to be used by MFRS; this is not necessarily the 
! full offset range of the data.
!</Help> 
!
!<Help KEYWORD="OFF_INC">
!<Tip> Increment of offset between centers of adjacent offset bins. </Tip>
! Default = 1
! Allowed = real > 0
! Increment of offset between centers of adjacent offset bins.  OFF_INC is 
! usually set to the receiver group interval value.
!</Help> 
!
!<Help KEYWORD="OFF_LAST">
!<Tip> Value of offset at the center of the last offset bin. </Tip>
! Default = 2
! Allowed = real > OFF_INIT
! Value of offset at the center of the last offset bin.  OFF_INIT and OFF_LAST
! define the range of offset to be used by MFRS; this is not necessarily the 
! full offset range of the data.
!</Help> 
!
!<Help KEYWORD="OFF_TOT">
!<Tip> Total number of offset bins. </Tip>
! Default = 1
! Allowed = int > 0
!</Help> 
!
!<Help KEYWORD="DATUM">
!<Tip>  Desired final datum elevation, in feet or meters. </Tip>
! Default = 1000
! Allowed = real
!</Help> 
!
!<Help KEYWORD="VEL_CORR">
!<Tip>  Correction velocity to use for correction to final datum. </Tip>
! Default = 1000
! Allowed = real > 0
! If VEL_CORR =< 4750, then MFRS assumes distance units are meters.
!
! If VEL_CORR > 4750, then MFRS assumes distance units are feet.
!</Help> 
!
!<Help KEYWORD="BULK">
!<Tip> Static file bulk shift in milliseconds. </Tip>
! Default = 0
! Allowed = real >= 0
! Bulk shift applied to the data prior to first break picking.  Normally a 
! positive bulk shift is applied prior to first break picking.  MFRS subtracts 
! this bulk shift to correct the picks back to zero time reference.
!
! Specify the PATH_STATIC value if a static file was also used to flatten the 
! data prior to first break picking.
!</Help>
!
!<HELP KEYWORD="PATH_STATIC_INFO"  TYPE= "DISPLAY_ONLY">
!<Tip> Status of PATH_STATIC. </Tip>
!</Help>
!
!<Help KEYWORD="SELECT_PATH_STATIC">
!<Tip> Choose PATH_STATIC using a file selection dialog box. </Tip>
!</Help>
!
!<Help KEYWORD="PATH_STATIC">
!<Tip> Pathname of the static file that was used to flatten the data. </Tip>
! Default = None
! Allowed = char
! A static file in offset is frequently used to flatten the first break data
! prior to first break picking.  Normally this static file contains positive 
! time shifts and is SUBTRACTED from the data to flatten it.  MFRS will add
! the shift contained in the PATH_STATIC file to correct the picks back to zero
! time reference.
!
! Specify the BULK parameter if a bulk shift was also used to flatten the data
! prior to first break picking.
!</Help>
!
!</HelpSection>
!-------------------------------------------------------------------------------
!!--------------------------- start of module ------------------------------!!
!!--------------------------- start of module ------------------------------!!
!!--------------------------- start of module ------------------------------!!




module mfrs_module
use ameq_module
use getlun_module
use mem_module
use mth_module
use named_constants_module
use pathcheck_module
use pathchoose_module
use pc_module
use timer_module
implicit none
private
public :: mfrs_create
public :: mfrs_initialize
public :: mfrs_update
public :: mfrs_delete
!<execute_only>
public :: mfrs_wrapup
!</execute_only>


character(len=100),public,save :: MFRS_IDENT = &
'$Id: mfrs.f90,v 1.7 2006/10/17 13:45:45 Glover prod sps $'

!!---------------------- parameter structure -------------------------------!!
!!---------------------- parameter structure -------------------------------!!
!!---------------------- parameter structure -------------------------------!!


      type,public :: mfrs_struct              

      private
      logical                        :: skip_wrapup
      character(len=filename_length) :: path_pick,path_out,path_static
      character(len=8)               :: meth_off,mode,opt_iter
      integer                        :: bulk,num_iter,num_iter_used,num_outly
      integer                        :: off_tot,off_init,off_inc,off_last
      integer                        :: datum,vel_corr
      type(pathchoose_struct),pointer :: pathchoose,pathchoose_static

      end type mfrs_struct

      integer :: i1,iavgelev,idstat,ipickt0,lgiloc,lgiloc1,lgishw,mine,minx,miny
      integer :: nelv,nrec=1,nsht,numx,numy

      real,pointer :: ffshft(:)
      real,pointer :: stats(:) 
      real,pointer :: statr(:) 
      real,pointer :: xmins(:), xmaxs(:) 
      real,pointer :: f3(:), f4(:) 
      real,pointer :: elevs(:), xminr(:), xmaxr(:), fhw34(:) 
      real,pointer :: fhw33(:),poff(:),apoff(:),apoff1(:),aoff(:),stdp(:),&
                      dif(:),xminp(:),xmaxp(:),xmind(:),xmaxd(:),shift(:),&
                      eshift(:),xmaxe(:),xmine(:),&
                      sumj(:),xpick(:),ap(:),xvel(:),xstd(:)
      real,pointer :: elevi(:) 
      real,pointer :: apsht(:), psht(:), stdpa(:), fhw9(:) 
      real,pointer :: aprec(:), prec(:) 

      integer,pointer :: iloc(:) 
      integer,pointer :: nloc(:) 
      integer,pointer :: ishw(:) 
      integer,pointer :: ihw35(:,:),ihw36(:,:),ipick(:,:),&
                         ioff(:,:),ielevr(:,:) 
      integer,pointer :: ivel(:),noff1(:),noff2(:) 
      integer,pointer :: nstdp(:),ndif(:),iaoff(:),nxpick(:),nxstd(:),nsht1(:),&
                         inum(:) 
      integer,pointer :: nrec1(:) 


!!---------------------------- interfaces ---------------------------------!!
!!---------------------------- interfaces ---------------------------------!!
!!---------------------------- interfaces ---------------------------------!!




!!--------------------------------- data -----------------------------------!!
!!--------------------------------- data -----------------------------------!!
!!--------------------------------- data -----------------------------------!!


type(mfrs_struct),pointer,save :: object      ! needed for traps.

integer    ,parameter :: meth_off_noptions = 2
character(len=8)      :: meth_off_options(meth_off_noptions)     &
                        = (/ 'SPECIFY ','FIND    '/)

integer    ,parameter :: mode_noptions = 3
character(len=8)      :: mode_options(mode_noptions)             &
                        = (/ 'ALL     ','ITER    ','STATIC  '/)

integer    ,parameter :: opt_iter_noptions = 2
character(len=8)      :: opt_iter_options(opt_iter_noptions)             &
                        = (/ 'YES     ','NO      '/)

contains


!!----------------------------- create -------------------------------------!!
!!----------------------------- create -------------------------------------!!
!!----------------------------- create -------------------------------------!!


subroutine mfrs_create (obj)
implicit none
type(mfrs_struct),pointer :: obj       ! arguments

allocate (obj)
nullify (obj%pathchoose) ! jpa
nullify (obj%pathchoose_static) ! jpa

call pathchoose_create (obj%pathchoose,'path_pick','cst')
call pathchoose_create (obj%pathchoose_static,'path_static','dat')
call mfrs_initialize (obj)
return
end subroutine mfrs_create


!!------------------------------- delete -----------------------------------!!
!!------------------------------- delete -----------------------------------!!
!!------------------------------- delete -----------------------------------!!


subroutine mfrs_delete (obj)
implicit none
type(mfrs_struct),pointer :: obj       ! arguments

!<execute_only>
call mfrs_wrapup (obj)
!</execute_only>

call pathchoose_delete (obj%pathchoose)
deallocate(obj)
return
end subroutine mfrs_delete


!!----------------------------- initialize ---------------------------------!!
!!----------------------------- initialize ---------------------------------!!
!!----------------------------- initialize ---------------------------------!!


subroutine mfrs_initialize (obj)
implicit none
type(mfrs_struct),intent(inout) :: obj     

obj%path_pick = PATHCHECK_EMPTY
obj%path_static = PATHCHECK_EMPTY
obj%path_out  = PATHCHECK_EMPTY
obj%meth_off  = 'SPECIFY'
obj%mode      = 'ALL'
obj%opt_iter  = 'YES'
obj%bulk      = 0
obj%num_iter  = 100
obj%num_iter_used = 100
obj%num_outly = 2
obj%off_init = 1.0
obj%off_inc  = 1.0
obj%off_last = 2.0
obj%off_tot  = 1
obj%datum    = 0.0
obj%vel_corr = 6000.0


call mfrs_update (obj)
return
end subroutine mfrs_initialize


!!------------------------- start of update --------------------------------!!
!!------------------------- start of update --------------------------------!!
!!------------------------- start of update --------------------------------!!


subroutine mfrs_update (obj)
implicit none
type(mfrs_struct),intent(inout),target :: obj             ! arguments

!-----------------------------------------------
!   L o c a l   V a r i a b l e s
!-----------------------------------------------
integer :: nloop1=0,  &
           icheck, minoff, maxoff,  maxe, maxx, maxy, &
           nsumelev, ntr, i2, i3, i4, i5, i6, i7, i8, i9, i10, i11, i12, i13, &
           i14, l, j2, j, i,iomin,iomin_osadd,iloc1,&
           k, nitr, io, ia1, ia2, ib1, ib2, k3, nct1, ip1, ip2, ictps1, ip, &
           ictd1,ictd2,nsumvel,isavgvel,iwavgvel,idavgvel,iavgvel,ipick0,&
           minj, maxj, maxvel, nj, nmaxj, nminj, ic, ii, &
           k1,iy4,nsxvel,nsxpick,ixvel2,ixvel,ixvel3, &
           nbval,naval,nstdp3,numa,ihw34,ihw33, &
           iihw36,iihw35,oflag,oldstdo,cstdo,&
           sflag,oldstds,cstds,rflag,oldstdr,cstdr,testflag,istdo,&
           istds,istdr,ovflag,oldvelo,cvelo,sflag3,oldstds3,&
           cstds3,rflag3,oldstdr3,cstdr3,istds3,istdr3,&
           evflag,oldvele,cvele,e2vflag,oldvele2,cvele2,ckbval

integer :: id1,idum1,idum2,idum3,inco,inumhor,iomax,istat,nalloc,noff,nn,nrecest
integer :: timer1,timerf,timerg  
integer :: lun_path_pick,lun_static,lun_src,lun_rec,lun_off,lun_elev,lun_outp,&
           lun_ref

real :: sumelev, f1, f2, f5, f6, f7, f8, f9, f10, f11,&
        avgelev, x1, x2, t2, xo, te1, te1a, te2, te2a, te3, epick, opick1,&
        atest, avgoff, avgpoff, ap0, ap2, ap3, ap4, stdp2, stdp1, fact, &
        sum, stdd2, stdd4, avgdif, stdd1, stdd3, sumvel, xap1, xap2, xap3,xap4,&
        savgvel, wv2, wv3, wv1, wv4, xop, dstat, xk, xx, shft, aa, bb, &
        gap, x3, x4, opick, picka, eleva, y1, y2, y3, y4, pickb, elevb, &
        deltap, deltae, sxvel, sxvel2, sxpick,gavge,&
        bval, aval, emax, spick1, avgpsht, stdp3, &
        gstd3, shotc, stdmax, xsum, xstd0, asum, rpick1, &
        avgprec, recc, tg2, tf2, ckbval2, xbval

real :: d1,d2,d3,d4,d5,d6,d7,d8,d9,d10
real :: fhdrinc,fhorinc,firsthdr,firsthor

character(len=filename_length) :: outpname,srcname,recname,offname,&
                                  elevname,refname
character(len=6) :: a1
character(len=8) :: aaaa,cdum


object => obj               ! needed for traps.
obj%skip_wrapup = .true.    ! needed for the wrapup routine.

if(pathchoose_update(obj%pathchoose,obj%path_pick))return
if(pathchoose_update(obj%pathchoose_static,obj%path_static))return


!!------------------------- read parameters --------------------------------!!
!!------------------------- read parameters --------------------------------!!
!!------------------------- read parameters --------------------------------!!




!      call pc_get('path_pick',obj%path_pick,mfrs_path_pick_trap)
call pc_get('path_pick',obj%path_pick)
! need to always call this trap because obj%path_pick might be set by 
! pathchoose instead of pc_get
call mfrs_path_pick_trap('path_pick')
call pc_get('path_static',obj%path_static)
! need to always call this trap because obj%path_static might be set by 
! pathchoose instead of pc_get
call mfrs_path_static_trap('path_static')
call pc_get('mode',obj%mode,mfrs_mode_trap)
call pc_get('path_out',obj%path_out,mfrs_path_out_trap)
call pc_get('meth_off',obj%meth_off,mfrs_meth_off_trap)
call pc_get('bulk',obj%bulk,mfrs_bulk_trap)
call pc_get('num_iter',obj%num_iter,mfrs_num_iter_trap)
call pc_get('opt_iter',obj%opt_iter,mfrs_opt_iter_trap)
call pc_get('num_outly',obj%num_outly,mfrs_num_outly_trap)
call pc_get('off_init',obj%off_init,mfrs_off_init_trap)
call pc_get('off_inc',obj%off_inc,mfrs_off_inc_trap)
call pc_get('off_last',obj%off_last,mfrs_off_last_trap)
call pc_get('off_tot',obj%off_tot,mfrs_off_tot_trap)
call pc_get('datum',obj%datum,mfrs_datum_trap)
call pc_get('vel_corr',obj%vel_corr,mfrs_vel_corr_trap)


!!-------------------------- verify parameters -----------------------------!!
!!-------------------------- verify parameters -----------------------------!!
!!-------------------------- verify parameters -----------------------------!!

call pc_call_end_trap(mfrs_end_trap)


!!----------------------- write parameters ---------------------------------!!
!!----------------------- write parameters ---------------------------------!!
!!----------------------- write parameters ---------------------------------!!


call pc_put_options_field('METH_OFF',meth_off_options,meth_off_noptions)
call pc_put_options_field('MODE',mode_options,mode_noptions)
call pc_put_options_field('OPT_ITER',opt_iter_options,opt_iter_noptions)

call pc_put('path_pick',obj%path_pick)
call pc_put('path_static',obj%path_static)
call pc_put('path_out',obj%path_out)
call pc_put('meth_off',obj%meth_off)
call pc_put('mode',obj%mode)
call pc_put('opt_iter',obj%opt_iter)
call pc_put('bulk',obj%bulk)
call pc_put('num_iter',obj%num_iter)
call pc_put('num_outly',obj%num_outly)
call pc_put('off_init',obj%off_init)
call pc_put('off_inc',obj%off_inc)
call pc_put('off_last',obj%off_last)
call pc_put('off_tot',obj%off_tot)
call pc_put('datum',obj%datum)
call pc_put('vel_corr',obj%vel_corr)

call pc_put_control ('SETUP_ONLY', .true.)


!!----------------------- prepare for execution ----------------------------!!
!!----------------------- prepare for execution ----------------------------!!
!!----------------------- prepare for execution ----------------------------!!



!<execute_only>

if (pc_do_not_process_traces()) return

obj%skip_wrapup = .false.     ! to run wrapup code after processing.




if (pc_do_not_process_traces()) return   ! in case of allocation errors.


!******************************************************************************
!******************************************************************************
!       this program takes a refraction pickfile and iterates a refraction      
!       static solution                                                         
!******************************************************************************
!cst file                                (a40)                                
!num_iter   nitnloop1                    (4i6)                                
!off_init  off_last  off_tot  off_inc  datum vel_corr    (6i6)     
!       
!num_iter  = # of times to run through offset,elevation,shot,and reciever loop  
!num_outly    = # of times to iterate solution in individual sections of loop 
!nloop1 = 0                                                                   
!off_init   = minimum offset to use.  if off_init < 0 no limits on offsets  
!off_last   = maximum offset    (not used if off_init < 0)          
!off_tot    = number of offsets (not used if off_init < 0)            
! OLD METHOD   (off_last-off_init+1)/off_inc  must = off_tot if off_init > 0  
! off_tot changed to use mth_module method with rev4
!             off_tot=nint(((last_off-off_init) / vinc + 1
!off_inc    = width of offset bins to use     
!datum      = final datum elevation    
!vel_corr   = velocity to correct from iavgelev to datum  ; 0 = use iavgvel  
!**** example *****************************************************************
!osadd.cst                                                                    
!       3     3     0     0                                                     
!  301  1  6970    24   165   700  7000                                         
!******************************************************************************
!       creates restart files after each section                                
!       refract.diagxy  x = loop (a,b,c...)   y = section # (1,2,3,4) completed 
!******************************************************************************
!       creates diagnostic files for each section   
!       .diag.off   .diag.elev  .diag.src   .diag.rec
!******************************************************************************
!       creates output file -- .diag.ref                                     
!******************************************************************************
!******************************************************************************


!          Open files
      outpname=trim(obj%path_out) // '.diag.outp'
      if(obj%mode.ne.'STATIC')then
        call getlun(lun_path_pick,istat)
        if(istat.ne.0)then
         call pc_error('Unable to get unit number for path_pick')
        endif
        open (lun_path_pick,file=obj%path_pick,status='OLD',iostat=istat)
        if(istat.ne.0)call pc_error('Unable to open path_pick file')

        if(obj%bulk.ne.0)then
           call getlun(lun_static,istat)
           if(istat.ne.0)then
             call pc_error('Unable to get unit number for static file')
           endif
           open(lun_static,file=obj%path_static,status='old',iostat=istat)
           if(istat.ne.0)call pc_error('Unable to open path_static file')
        endif

        call getlun(lun_outp,istat)
        if(istat.ne.0)call pc_error('Unable to get unit number for .diag.outp')
        open(lun_outp,file=outpname,status='replace',iostat=istat)
        if(istat.ne.0)then
          call pc_error('Unable to open .diag.outp file')
        endif

        refname=trim(obj%path_out) // '.diag.ref'
        call getlun(lun_ref,istat)
        if(istat.ne.0)call pc_error('Unable to get unit number for .diag.ref')
        open(lun_ref,file=refname,status='replace',iostat=istat)
        if(istat.ne.0)call pc_error('Unable to open .diag.ref file')

        srcname=trim(obj%path_out) // '.diag.src'
        call getlun(lun_src,istat)
        if(istat.ne.0)call pc_error('Unable to get unit number for .diag.src')
        open(lun_src,file=srcname,status='replace',iostat=istat)
        if(istat.ne.0)call pc_error('Unable to open .diag.src file')

        recname=trim(obj%path_out) // '.diag.rec'
        call getlun(lun_rec,istat)
        if(istat.ne.0)call pc_error('Unable to get unit number for .diag.rec')
        open(lun_rec,file=recname,status='replace',iostat=istat)
        if(istat.ne.0)call pc_error('Unable to open .diag.rec file')

        offname=trim(obj%path_out) // '.diag.off'
        call getlun(lun_off,istat)
        if(istat.ne.0)call pc_error('Unable to get unit number for .diag.off')
        open(lun_off,file=offname,status='replace',iostat=istat)
        if(istat.ne.0)call pc_error('Unable to open .diag.off file')

        elevname=trim(obj%path_out) // '.diag.elev'
        call getlun(lun_elev,istat)
        if(istat.ne.0)call pc_error('Unable to get unit number for .diag.elev')
        open(lun_elev,file=elevname,status='replace',iostat=istat)
        if(istat.ne.0)call pc_error('Unable to open .diag.elev file')

      endif  !  if(obj%mode.ne.'STATIC')


      if(obj%mode.eq.'STATIC')go to 5000



        call timer_alloc(timer1)
        call timer_alloc(timerf)
        call timer_clear(timer1)
        call timer_clear(timerf)
        call timer_start(timer1)
        call timer_start(timerf)
        call timer_alloc(timerg)
!******************************************************************************
        write(lun_outp,25)
   25   format(/,4x,'REFRACTION PROGRAM ',//,4x,'READING DATA')
        if(obj%off_init.ge.0)then
          icheck=mth_bin_number(real(obj%off_init),real(obj%off_inc),&
                                val=real(obj%off_last))
!rev4          icheck=(obj%off_last-obj%off_init+1)/obj%off_inc
          if(icheck.ne.obj%off_tot)then
            write(lun_outp,31)
   31       format(/,4x,'OFF_INIT,OFF_LAST,OFF_TOT, OFF_INC DO NOT CHECK',//)
            go to 803
          end if
        end if
        close(lun_outp)
!******************************************************************************
!
        if(obj%bulk.ne.0)then
!              Read in the static file used to shift the data
           read(lun_static,140)idum1,idum2,idum3
           read(lun_static, 150)firsthdr,firsthor,fhdrinc,fhorinc,noff,inumhor
  140     format(8x,3i4) 
  150     format(4f14.3,2i7)
          iomin_osadd = nint(firsthdr) 
          inco =(fhdrinc) 
          iomax =(noff - 1) * inco + iomin_osadd
          cdum=' '
          do while (cdum.ne.'+++')
            read(lun_static,'(a3)')cdum
          enddo
          nn = int((noff + 4) / 5)        
          nalloc =((nn - 1) * 5) + 5
          call mem_alloc(ffshft,nalloc)
          ffshft=0.0
          do i = 1, nn 
            j =((i - 1) * 5) + 1 
            read(lun_static,160,iostat=istat)j,ffshft(j),ffshft(j+1),&
                                             ffshft(j+2),ffshft(j+3),ffshft(j+4)
            if(istat.lt.0)exit
          enddo                                                  
  160     format(i8,5f14.2) 
        endif
!******************************************************************************
!       read scrs pick file                                         
!******************************************************************************
        
        minoff=99999
        maxoff=0
        mine=99999
        maxe=0
        minx=99999
        miny=99999
        maxx=-99999
        maxy=-99999
        sumelev=0
        nsumelev=0
        ntr=0
        read (lun_path_pick,4)i1,i2,i3,i4,i5,i6,i7,i8
    4   format(8i6)
        read (lun_path_pick,5)f1,i9,f2,a1,i10,i11,i12,i13,i14
    5   format(f6.1,i6,f6.3,a6,5i4)
        call mem_alloc(f3,i10)
        call mem_alloc(f4,i11)
        read (lun_path_pick,6)(f3(l),l=1,i10)
    6   format(5f10.3)
        read (lun_path_pick,6)(f4(l),l=1,i11)
        j2=i10*i11*i12
        do 201 j=1,j2
          read (lun_path_pick,6)f5,f6,f7
  201     continue

      call mem_alloc(ishw,max(i1,1000))
      call mem_alloc(shift,max(i1,1000))
      call mem_alloc(iaoff,max(i1,1000))
      call mem_alloc(eshift,max(i1,1000))
      call mem_alloc(elevs,max(i1,1000))
      call mem_alloc(stats,max(i1,1000))

      call mem_alloc(ihw35,max(i1,1000),i9)
      call mem_alloc(ihw36,max(i1,1000),i9)
      call mem_alloc(ipick,max(i1,1000),i9)
      call mem_alloc(ioff,max(i1,1000),i9)
      call mem_alloc(ielevr,max(i1,1000),i9)
      call mem_alloc(ivel,max(i1,1000))
      call mem_alloc(noff1,max(i1,1000))
      call mem_alloc(noff2,max(i1,1000))
      call mem_alloc(nstdp,max(i1,1000))
      call mem_alloc(ndif,max(i1,1000))
      call mem_alloc(nxpick,max(i1,1000))
      call mem_alloc(nxstd,max(i1,1000))
      call mem_alloc(nsht1,max(i1,1000))
      call mem_alloc(inum,max(i1,1000))
      call mem_alloc(xmins,max(i1,1000))
      call mem_alloc(xmaxs,max(i1,1000))
      call mem_alloc(xminr,max(i1,1000))
      call mem_alloc(xmaxr,max(i1,1000))
      call mem_alloc(fhw34,max(i1,1000))
      call mem_alloc(fhw33,max(i1,1000))
      call mem_alloc(poff,max(i1,1000))
      call mem_alloc(apoff,max(i1,1000))
      call mem_alloc(apoff1,max(i1,1000))
      call mem_alloc(aoff,max(i1,1000))
      call mem_alloc(stdp,max(i1,1000))
      call mem_alloc(dif,max(i1,1000))
      call mem_alloc(xminp,max(i1,1000))
      call mem_alloc(xmaxp,max(i1,1000))
      call mem_alloc(xmind,max(i1,1000))
      call mem_alloc(xmaxd,max(i1,1000))
      call mem_alloc(sumj,max(i1,1000))
      call mem_alloc(xpick,max(i1,1000))
      call mem_alloc(ap,max(i1,1000))
      call mem_alloc(xvel,max(i1,1000))
      call mem_alloc(xstd,max(i1,1000))
      call mem_alloc(apsht,max(i1,1000))
      call mem_alloc(psht,max(i1,1000))
      call mem_alloc(stdpa,max(i1,1000))
      call mem_alloc(fhw9,max(i1,1000))

        do 203 i=1,i1
          read (lun_path_pick,7)fhw33(i),f6,f7,elevs(i),f8
          read (lun_path_pick,7)f9,f10,fhw34(i),fhw9(i),f11
    7     format(5f10.3)
          if(fhw33(i).eq.0)go to 95
          if(fhw33(i).lt.minx)minx=fhw33(i)
          if(fhw34(i).lt.miny)miny=fhw34(i)
          if(fhw33(i).gt.maxx)maxx=fhw33(i)
          if(fhw34(i).gt.maxy)maxy=fhw34(i)
          if(elevs(i).eq.0)go to 95
          if(elevs(i).lt.mine)mine=elevs(i)
          if(elevs(i).gt.maxe)maxe=elevs(i)
          sumelev=sumelev+elevs(i)
          nsumelev=nsumelev+1
   95     continue
          do 202 j=1,i9
            read(lun_path_pick,8)ipick(i,j),ihw35(i,j),ihw36(i,j),ioff(i,j),&
                                 ielevr(i,j)
            if(obj%bulk.ne.0)then
              call mfrs_osadd(ipick(i,j),ioff(i,j),ffshft,iomin_osadd,inco,&
                              iomax,obj%bulk,noff)
            endif
    8       format(5i6)
            if(ipick(i,j).eq.0)go to 202
            if(ihw35(i,j).eq.0)go to 202
            ntr=ntr+1
            if(minoff.gt.ioff(i,j))minoff=ioff(i,j)
            if(maxoff.lt.ioff(i,j))maxoff=ioff(i,j)
            if(ihw35(i,j).lt.minx)minx=ihw35(i,j)
            if(ihw36(i,j).lt.miny)miny=ihw36(i,j)
            if(ihw35(i,j).gt.maxx)maxx=ihw35(i,j)
            if(ihw36(i,j).gt.maxy)maxy=ihw36(i,j)
            if(ielevr(i,j).eq.0)go to 202
            if(ielevr(i,j).lt.mine)mine=ielevr(i,j)
            if(ielevr(i,j).gt.maxe)maxe=ielevr(i,j)
            sumelev=sumelev+ielevr(i,j)
            nsumelev=nsumelev+1
  202       continue
  203     continue
        close (lun_path_pick)
        if(obj%bulk.ne.0)call mem_free(ffshft)
        numx=maxx-minx+1
        numy=maxy-miny+1
        if((miny.eq.0).and.(maxy.eq.0))then
          numx=maxx
          minx=1
        end if
        nelv=maxe-mine+1
        call mem_alloc(eshift,max(1000,i1,nelv))
        call mem_alloc(nxpick,max(1000,i1,nelv))
        call mem_alloc(xpick,max(1000,i1,nelv))
        call mem_alloc(ap,max(1000,i1,nelv))
        call mem_alloc(xvel,max(1000,i1,nelv))
        call mem_alloc(xstd,max(1000,i1,nelv))
        call mem_alloc(nxstd,max(1000,i1,nelv))
        call mem_alloc(sumj,max(1000,i1,nelv))
        call mem_alloc(xmine,max(1000,i1,nelv)) 
        call mem_alloc(xmaxe,max(1000,i1,nelv))
        avgelev=sumelev/nsumelev
        iavgelev=nint(avgelev)
        if(mine.lt.0)then
          do i=1,i1
            elevs(i)=elevs(i)-mine+1
            do j=1,i9
              ielevr(i,j)=ielevr(i,j)-mine+1
             end do
          end do
          iavgelev=iavgelev-mine+1
        end if
        nsht=i1
!******************************************************************************
! calculate # offset ranges
!*****************************************************************************
        if(obj%off_init.ge.0)then  !meth_off was SPECIFY
          iomin=obj%off_init
!??          obj%off_tot=obj%off_tot
          go to 204
        end if
        x1=minoff                  !meth_off was FIND
        iomin=x1+obj%off_inc/2-1
        if(iomin.lt.0)iomin=obj%off_inc/2-1
        obj%off_tot=mth_bin_number(real(iomin),real(obj%off_inc),&
                                   val=real(maxoff))
!rev4        x2=maxoff-iomin
!rev4        x2=x2/obj%off_inc
!rev4        obj%off_tot=nint(x2)+1
  204   continue
!**************************************************************************
!          calculate nrec and save largest iloc1
        nrecest=50000
        lgiloc1=-999999
        call mem_alloc(iloc,nrecest)
        iloc=0
        do 13203 i=1,i1
          do 13205 j=1,i9
            if(ipick(i,j).eq.0)go to 13205
            if(ihw35(i,j).eq.0)go to 13205
            iloc1=numx*(ihw36(i,j)-miny)+ihw35(i,j)-minx+1
            lgiloc1=max(lgiloc1,iloc1)
            if(nrec.eq.0)go to 13204
            do k=1,nrec
              if(iloc(k).eq.iloc1)go to 13205
            enddo
 13204      continue
            nrec=nrec+1
            if(nrec.gt.nrecest)then
              nrecest=nrecest+50000
              call mem_realloc(iloc,nrecest)
            endif
            iloc(nrec)=iloc1
 13205      continue
 13203    continue

          call mem_free(iloc)
          call mem_alloc(nloc,lgiloc1)
          call mem_alloc(iloc,nrec)
          call mem_alloc(nrec1,nrec)
          call mem_alloc(statr,nrec)
          call mem_alloc(elevi,nrec)
          call mem_alloc(aprec,nrec)
          call mem_alloc(prec,nrec)
          call mem_alloc(nstdp,max(1000,i1,nrec))
          call mem_alloc(stdp,max(1000,i1,nrec))
          call mem_alloc(xminr,max(1000,i1,nrec))
          call mem_alloc(xmaxr,max(1000,i1,nrec))
          nloc=0
          iloc=0
          nrec1=0
          statr=0.0
          elevi=0.0
          aprec=0.0
          prec=0.0


!******************************************************************************
! calculate receiver ordinal numbers
!*****************************************************************************
        nrec=0
        do 3203 i=1,i1
          ishw(i)=numx*(fhw34(i)-miny)+fhw33(i)-minx+1
          do 3205 j=1,i9
            if(ipick(i,j).eq.0)go to 3205
            if(ihw35(i,j).eq.0)go to 3205
            iloc1=numx*(ihw36(i,j)-miny)+ihw35(i,j)-minx+1
            if(nrec.eq.0)go to 3204
            do k=1,nrec
              if(iloc(k).eq.iloc1)go to 3205
            enddo
 3204       continue
            nrec=nrec+1
            iloc(nrec)=iloc1
            nloc(iloc1)=nrec
            elevi(nrec)=ielevr(i,j)
 3205       continue
 3203     continue
        call timer_stop(timer1)
        call timer_fetch(timer1,id1,d1,d2,d3,d4,d5,d6,d7,d8,d9,d10,t2)
        call timer_clear(timer1)
!******************************************************************************
        open(lun_outp,file=outpname,status='old',position='append')
        write(lun_outp,900)t2
  900   format(5x,'ELAPSED TIME =',f8.2,' SECONDS',/)
        close(lun_outp)
!
!set autostop variable values to zero
!
        oflag=0
        oldstdo=0
        cstdo=0
        ovflag=0
        oldvelo=0
        cvelo=0
        evflag=0
        oldvele=0
        cvele=0
        e2vflag=0
        oldvele2=0
        cvele2=0
        sflag=0
        oldstds=0
        cstds=0
        rflag=0
        oldstdr=0
        cstdr=0
        sflag3=0
        oldstds3=0
        cstds3=0
        rflag3=0
        oldstdr3=0
        cstdr3=0
!*****************************************************************************
!       begin iteration loop                                  
!*****************************************************************************
  999   continue
        call timer_clear(timerg)
        call timer_start(timerg)
        nloop1=nloop1+1
!******************************************************************************
        open(lun_outp,file=outpname,status='old',position='append')
        write(lun_outp,30)nloop1
   30   format(/,4x,'REFRACTION PROGRAM LOOP#',i5)
        close(lun_outp)
!******************************************************************************
!******************************************************************************
!******************************************************************************
!******************************************************************************
!******************************************************************************
!******************************************************************************
!******************************************************************************
!
!       offset calculations
!
!*****************************************************************************
        call timer_start(timer1)
        nitr=0
        do i=1,obj%off_tot+2
          noff2(i)=0
          apoff(i)=0
          apoff1(i)=0
        end do
!
!       begin iteration loop                                  
 1000   continue
        nitr=nitr+1
!
!reset array values to zero
!
        do i=1,obj%off_tot+2
!                           avg calculation
          aoff(i)=0
          noff1(i)=0
          poff(i)=0
!                           std calculation
          stdp(i)=0
          nstdp(i)=0
!                           shift calculation
          shift(i)=0
          iaoff(i)=0
        end do
!
!start loop over shots
        do i=1,i1
!
!  start loop over recievers
          do j=1,i9
!
!    start trace selection if
!       select trace if:       
!       elevations > 0 
!       pick > 0 
!       (if offset range is specified)pick is within offset range 
! 
            if((elevs(i).gt.0).and.&
               (ielevr(i,j).gt.0).and.&
               (ipick(i,j).gt.0).and.&
               ((obj%off_init.lt.0).or.((obj%off_init.ge.0)&
                      .and.(ioff(i,j).ge.obj%off_init).and.&
                      (ioff(i,j).le.obj%off_last))))&
            then
! 
!calculate offset ordinal
!            
              xo=ioff(i,j)-iomin
              xo=xo/obj%off_inc+.5
              io=nint(xo)
!
!    calculate elevation static
!
              te1=elevs(i)-iavgelev
              te1a=abs(te1)
              ia1=nint(te1a)
              if(ia1.ne.0)ia2=nint(te1/te1a)
              if(ia1.eq.0)ia2=0
              te2=ielevr(i,j)-iavgelev
              te2a=abs(te2)
              ib1=nint(te2a)
              if(ib1.ne.0)ib2=nint(te2/te2a)
              if(ib1.eq.0)ib2=0
              te3=ia2*eshift(ia1)+ib2*eshift(ib1)
              epick=te3
!
!    calculate reciever ordinal
!
              iloc1=numx*(ihw36(i,j)-miny)+ihw35(i,j)-minx+1
              k3=nloc(iloc1)
!
!    calculate adjusted pick time
!
              opick1=ipick(i,j)-epick-stats(i)-statr(k3)
              if(nloop1.eq.1)opick1=ipick(i,j)
!
!    atest is difference between current pick
!           and the average pick of the next smaller offset bin
!
              atest=(opick1-apoff(io-1))/(xo+1-io)
              if(io.eq.1)atest=((apoff(2)-opick1))/(2-xo)
!
!    min/max tests
!
              if((nitr.eq.1).or.&
                 ((nitr.gt.1).and.&
                  (opick1.ge.xminp(io)).and.&
                  (opick1.le.xmaxp(io)).and.&
                  (atest.ge.xmind(io)).and.&
                  (atest.le.xmaxd(io))))&
              then
!
                aoff(io)=aoff(io)+ioff(i,j)
                noff1(io)=noff1(io)+1
                poff(io)=poff(io)+opick1
              end if
            end if
!    end trace selection if
!
          end do
!  end loop over recievers
!
        end do
!end loop over shots
!
        nct1=0
        avgoff=0
        avgpoff=0
!
!start average loop
!
        do i=1,obj%off_tot
          if(noff1(i).gt.0)then
            avgoff=avgoff+aoff(i)
            avgpoff=avgpoff+poff(i)
            apoff(i)=poff(i)/noff1(i)
            aoff(i)=aoff(i)/noff1(i)
            nct1=nct1+noff1(i)
            if(nitr.eq.1)then
              apoff1(i)=apoff(i)
              noff2(i)=noff1(i)
            end if
          end if
        end do
!end average and std loop
!
!global average 
        avgoff=avgoff/nct1
        avgpoff=avgpoff/nct1
!*****************************************************************************
! interpolate or extrapolate pick if pick = 0
!
        ip1=0
        ip2=0
        do i=1,obj%off_tot
!
!         extrapolate picks at near offsets if needed
!
          if(noff1(1).gt.0)ip1=999
          if((noff1(i).gt.0).and.(ip1.gt.0).and.(ip1.ne.999))then
            ap0=(apoff(i+2)-apoff(i))/2
            ap2=(apoff(i+2)+apoff(i))/2
            do j=1,i-1
              apoff(j)=ap2-(i-j+1)*ap0
              aoff(j)=mth_bin_center(real(iomin),real(obj%off_inc),j)
!rev4              aoff(j)=iomin+(j-1)*obj%off_inc
            end do
            ip1=999
          end if
!
!         interpolate picks for offsets with pick = 0
!
          if((noff1(i).gt.0).and.(ip2.gt.0))then
            ap0=apoff(i+1)+apoff(i)-apoff(i-ip2-2)-apoff(i-ip2-1)
            ap0=ap0/(2*(ip2+2))
            ap2=apoff(i+1)+apoff(i)+apoff(i-ip2-2)+apoff(i-ip2-1)
            ap2=ap2/4
            ap3=i-(ip2+1)/2
            do j=i-ip2,i-1
              ap4=ap3-j
              aoff(j)=mth_bin_center(real(iomin),real(obj%off_inc),j)
!rev4              aoff(j)=iomin+(j-1)*obj%off_inc
              apoff(j)=ap2-ap4*ap0
            end do
            ip2=0
          end if
!
!         extrapolate picks at far offsets if needed
!
          if((noff1(i).eq.0).and.(i.eq.obj%off_tot))then
            k=obj%off_tot-ip2
            ap0=(apoff(k-1)-apoff(k-3))/2
            ap2=(apoff(k-1)+apoff(k-3))/2
            do j=k,obj%off_tot
              apoff(j)=ap2+(j-k+2)*ap0
              aoff(j)=mth_bin_center(real(iomin),real(obj%off_inc),j)
!rev4              aoff(j)=iomin+(j-1)*obj%off_inc
            end do
            ip2=0
          end if
!
!         count number of near offsets with pick = 0
!
          if((noff1(i).eq.0).and.(ip1.ne.999))then
            ip1=ip1+1
          end if
!
!         set flag if pick = 0 for this offset 
!                after first non-zero pick is processed
!
          if((noff1(i).eq.0).and.(ip1.eq.999))then
            ip2=ip2+1
          end if
        end do
!
!end loop to set zero picks
!*****************************************************************************
!       calculate pick standard deviation                     
!*****************************************************************************
!reset array values to zero
!
        do i=1,obj%off_tot
          stdp(i)=0
          nstdp(i)=0
        end do
        stdp2=0
        ictps1=0
!
!start loop over shots
        do i=1,i1
!
!  start loop over recievers
          do j=1,i9
!
!    start trace selection if
!       select trace if:       
!       elevations > 0 
!       pick > 0 
!       (if offset range is specified)pick is within offset range 
! 
            if((elevs(i).gt.0).and.&
               (ielevr(i,j).gt.0).and.&
               (ipick(i,j).gt.0).and.&
               ((obj%off_init.lt.0).or.((obj%off_init.ge.0)&
                      .and.(ioff(i,j).ge.obj%off_init).and.&
                      (ioff(i,j).le.obj%off_last))))&
            then
! 
!calculate offset ordinal
! 
!           adjust pick value
              xo=ioff(i,j)-iomin
              xo=xo/obj%off_inc+.5
              io=nint(xo)
!
!    calculate elevation static
!
              te1=elevs(i)-iavgelev
              te1a=abs(te1)
              ia1=nint(te1a)
              if(ia1.ne.0)ia2=nint(te1/te1a)
              if(ia1.eq.0)ia2=0
              te2=ielevr(i,j)-iavgelev
              te2a=abs(te2)
              ib1=nint(te2a)
              if(ib1.ne.0)ib2=nint(te2/te2a)
              if(ib1.eq.0)ib2=0
              te3=ia2*eshift(ia1)+ib2*eshift(ib1)
              epick=te3
!
!    calculate reciever ordinal
!
              iloc1=numx*(ihw36(i,j)-miny)+ihw35(i,j)-minx+1
              k3=nloc(iloc1)
!
!    calculate adjusted pick time
!
              opick1=ipick(i,j)-epick-stats(i)-statr(k3)
              if(nloop1.eq.1)opick1=ipick(i,j)
              atest=(opick1-apoff(io-1))/(xo+1-io)
              if(io.eq.1)atest=((apoff(2)-opick1))/(2-xo)
!
!    min/max tests
!
              if((nitr.eq.1).or.&
                 ((nitr.gt.1).and.&
                  (opick1.ge.xminp(io)).and.&
                  (opick1.le.xmaxp(io)).and.&
                  (atest.ge.xmind(io)).and.&
                  (atest.le.xmaxd(io))))&
              then
!
                ip=xo
                ap0=apoff(ip+1)-apoff(ip)
                if(ip.eq.0)ap0=apoff(2)-apoff(1)
                if(ip+1.gt.obj%off_tot)ap0=apoff(obj%off_tot)-&
                                       apoff(obj%off_tot-1)
                ap0=ap0*(io-xo)
                stdp1=opick1+ap0-apoff(io)
                stdp1=stdp1*stdp1
                stdp2=stdp2+stdp1
                ictps1=ictps1+1
                stdp(io)=stdp(io)+stdp1
                nstdp(io)=nstdp(io)+1
              end if
            end if
!    end trace selection if
!
          end do
!  end loop over recievers
!
        end do
!end loop over shots
!
!
!
!set factor for scaling the std for this iteration
!
        fact=1
        if(obj%num_outly.gt.1)then
          fact=obj%num_outly-nitr-1
          fact=fact/(obj%num_outly-1)
          fact=1+fact
          if(fact.lt.1)fact=1
        end if
!
!calculate standard deviations
!
        nstdp3=0
        stdp3=0
        gstd3=0
        stdp2=stdp2/ictps1
        stdp2=sqrt(stdp2)
        do i=1,obj%off_tot
          if(nstdp(i).lt.2)then
             stdp(i)=0
            else
             stdp(i)=stdp(i)/nstdp(i)
             stdp(i)=sqrt(stdp(i))
             stdp3=stdp3+stdp(i)
             gstd3=gstd3+stdp(i)**2
             nstdp3=nstdp3+1
          end if
!
!  min/max pick using standard deviation
!
          xminp(i)=apoff(i)-stdp(i)*fact
          xmaxp(i)=apoff(i)+stdp(i)*fact
        end do
        stdp3=stdp3/nstdp3
        gstd3=gstd3/nstdp3
        gstd3=gstd3-stdp3**2
        gstd3=sqrt(gstd3)
!*****************************************************************************
!       calculate difference stats                                
!*****************************************************************************
!       reset array values to zero
!
        do i=1,obj%off_tot
          dif(i)=0
          ndif(i)=0
        end do
        sum=0
        ictd1=0
!
!start loop over shots
        do i=1,i1
!
!  start loop over recievers
          do j=1,i9
!
!    start trace selection if
!       select trace if:       
!       elevations > 0 
!       pick > 0 
!       (if offset range is specified)pick is within offset range 
! 
            if((elevs(i).gt.0).and.&
               (ielevr(i,j).gt.0).and.&
               (ipick(i,j).gt.0).and.&
               ((obj%off_init.lt.0).or.((obj%off_init.ge.0)&
                      .and.(ioff(i,j).ge.obj%off_init).and.&
                      (ioff(i,j).le.obj%off_last))))&
            then
! 
!calculate offset ordinal
! 
!           adjust pick value
              xo=ioff(i,j)-iomin
              xo=xo/obj%off_inc+.5
              io=nint(xo)
!
!    calculate elevation static
!
              te1=elevs(i)-iavgelev
              te1a=abs(te1)
              ia1=nint(te1a)
              if(ia1.ne.0)ia2=nint(te1/te1a)
              if(ia1.eq.0)ia2=0
              te2=ielevr(i,j)-iavgelev
              te2a=abs(te2)
              ib1=nint(te2a)
              if(ib1.ne.0)ib2=nint(te2/te2a)
              if(ib1.eq.0)ib2=0
              te3=ia2*eshift(ia1)+ib2*eshift(ib1)
              epick=te3
!
!    calculate reciever ordinal
!
              iloc1=numx*(ihw36(i,j)-miny)+ihw35(i,j)-minx+1
              k3=nloc(iloc1)
!
!           calculate adjusted pick time
!
              opick1=ipick(i,j)-epick-stats(i)-statr(k3)
              if(nloop1.eq.1)opick1=ipick(i,j)
              atest=(opick1-apoff(io-1))/(xo+1-io)
              if(io.eq.1)atest=((apoff(2)-opick1))/(2-xo)
!
!    min/max tests
!
              if((nitr.eq.1).or.&
                 ((nitr.gt.1).and.&
                  (opick1.ge.xminp(io)).and.&
                  (opick1.le.xmaxp(io)).and.&
                  (atest.ge.xmind(io)).and.&
                  (atest.le.xmaxd(io))))&
              then
!
        end if
               dif(io)=dif(io)+atest
                ndif(io)=ndif(io)+1
                sum=sum+atest
                ictd1=ictd1+1
              end if
!    end trace selection if
!
          end do
!  end loop over recievers
!
        end do
!end loop over shots
!
        do i=1,obj%off_tot
          if(ndif(i).ge.1)then
            dif(i)=dif(i)/ndif(i)
            if(dif(i).lt.0)dif(i)=0
          end if
        end do
        avgdif=sum/ictd1
!*****************************************************************************
!       calculate difference standard deviation                     
!*****************************************************************************
!reset constants to zero
        stdd2=0
        stdd4=0
        ictd1=0
        ictd2=0
!
!
!start loop over shots
        do i=1,i1
!
!  start loop over recievers
          do j=1,i9
!
!    start trace selection if
!       select trace if:       
!       elevations > 0 
!       pick > 0 
!       (if offset range is specified)pick is within offset range 
! 
            if((elevs(i).gt.0).and.&
               (ielevr(i,j).gt.0).and.&
               (ipick(i,j).gt.0).and.&
               ((obj%off_init.lt.0).or.((obj%off_init.ge.0)&
                      .and.(ioff(i,j).ge.obj%off_init).and.&
                      (ioff(i,j).le.obj%off_last))))&
            then
! 
!calculate offset ordinal
! 
!           adjust pick value
              xo=ioff(i,j)-iomin
              xo=xo/obj%off_inc+.5
              io=nint(xo)
!
!    calculate elevation static
!
              te1=elevs(i)-iavgelev
              te1a=abs(te1)
              ia1=nint(te1a)
              if(ia1.ne.0)ia2=nint(te1/te1a)
              if(ia1.eq.0)ia2=0
              te2=ielevr(i,j)-iavgelev
              te2a=abs(te2)
              ib1=nint(te2a)
              if(ib1.ne.0)ib2=nint(te2/te2a)
              if(ib1.eq.0)ib2=0
              te3=ia2*eshift(ia1)+ib2*eshift(ib1)
              epick=te3
!
!    calculate reciever ordinal
!
              iloc1=numx*(ihw36(i,j)-miny)+ihw35(i,j)-minx+1
              k3=nloc(iloc1)
!
!           calculate adjusted pick time
!
              opick1=ipick(i,j)-epick-stats(i)-statr(k3)
              if(nloop1.eq.1)opick1=ipick(i,j)
              atest=(opick1-apoff(io-1))/(xo+1-io)
              if(io.eq.1)atest=((apoff(2)-opick1))/(2-xo)
!
!    min/max tests
!
              if((nitr.eq.1).or.&
                 ((nitr.gt.1).and.&
                  (opick1.ge.xminp(io)).and.&
                  (opick1.le.xmaxp(io)).and.&
                  (atest.ge.xmind(io)).and.&
                  (atest.le.xmaxd(io))))&
              then
!
!    stdd2 uses dif by offset  stdd4 uses global dif
!
                stdd1=atest-dif(io)
                stdd1=stdd1*stdd1
                stdd2=stdd2+stdd1
                stdd3=atest-avgdif
                stdd3=stdd3*stdd3
                stdd4=stdd4+stdd3
                ictd2=ictd2+1
              end if
            end if
!    end trace selection if
!
          end do
!  end loop over recievers
!
        end do
!end loop over shots
!
!stdev calculations
        stdd2=stdd2/ictd2
        stdd2=sqrt(stdd2)
        stdd4=stdd4/ictd2
        stdd4=sqrt(stdd4)
!
!       using dif by offset stdev to set min/max difference
!
        do i=1,obj%off_tot
          xmind(i)=avgdif-stdd2*fact
          xmaxd(i)=avgdif+stdd2*fact
        end do
!
!check to see if finished with iterations 
!
        if(nitr.ne.obj%num_outly)go to 1000
!
!****************************************************************************
!       calculate refractor velocity                         
!****************************************************************************
        sumvel=0
        nsumvel=0
        if((apoff(1).gt.1).and.(aoff(1).gt.0))then
          ivel(1)=nint(aoff(1)*1000/apoff(1))
          sumvel=sumvel+ivel(1)
          nsumvel=nsumvel+1
        end if
        do i=2,obj%off_tot
          if((apoff(i).eq.0).or.&
             (apoff(i-1).eq.0).or.&
             (apoff(i).le.apoff(i-1)))&
          then
             ivel(i)=0
           else
            xap1=aoff(i)-aoff(i-1)
            xap2=apoff(i)-apoff(i-1)
            xap3=xap1/xap2
            xap4=xap3*1000
            ivel(i)=xap4
            xk=20000
            if(obj%vel_corr.lt.4750)xk=6100.0
            if((ivel(i).gt.xk).or.(ivel(i).lt.0))then
               ivel(i)=0
             else
               sumvel=sumvel+ivel(i)
               nsumvel=nsumvel+1
            end if
          end if
        end do
        savgvel=sumvel/nsumvel
        isavgvel=nint(savgvel)
!
!calculate weighted average velocity        
!
        wv2=0
        wv3=0
        do i=1,obj%off_tot
          if(noff1(i).eq.0)cycle
          wv1=noff1(i)
          wv2=wv2+wv1
          wv3=wv3+ivel(i)*wv1
        enddo
        wv4=wv3/wv2
        iwavgvel=nint(wv4)
!
!calculate average difference velocity        
!
        idavgvel=obj%off_inc*1000/avgdif
!
!average velocity = difference velocity + weighted average velocity /2
!
        iavgvel=(iwavgvel+idavgvel+isavgvel)/3
!
!calculate pick extrapolated to zero offset        
!
        xop=avgoff-iomin
        xop=xop*1000/iavgvel
        xop=avgpoff-xop
        ipick0=nint(xop)
!
!calculate datum correction        
!
        x1=obj%datum-iavgelev
        if(mine.lt.0)x1=obj%datum-(iavgelev+mine-1)
        x2=x1*1000
        dstat=x2/iavgvel
        if(obj%vel_corr.gt.0.0)dstat=x2/obj%vel_corr
        idstat=nint(dstat)
!****************************************************************************
!       smooth and edit refractor velocity        
!****************************************************************************
        minj=99999
        maxj=0
        maxvel=iavgvel*1.5
!
!smooth velocities over 1/2 mile         
!
        do i=1,obj%off_tot
          nj=0
          sumj(i)=0
          xk=2640.0
          if(obj%vel_corr.lt.4750)xk=800.0
          xk=xk/obj%off_inc
          xk=xk/2
          k=nint(xk)
          do j=-k,k
            if((i+j.ge.1).and.(ivel(i+j).gt.0)&
                  .and.(i+j.le.obj%off_tot))then
              nj=nj+1
              sumj(i)=sumj(i)+ivel(i+j)
            end if
          end do
          sumj(i)=sumj(i)/nj
          if(nj.eq.0)sumj(i)=0
        end do
!
!find minimum and maximum velocity         
!
        do i=1,obj%off_tot
          if(sumj(i).gt.maxj)then
            maxj=sumj(i)
            nmaxj=i
          end if
        end do
        if(maxj.gt.maxvel)then
          do i=1,obj%off_tot
            maxj=maxvel
            if(sumj(i).gt.maxvel)then
              nmaxj=i
              sumj(i)=maxj
              go to 1100
            end if
          end do
        end if
 1100   continue
        do i=1,nmaxj
           if((sumj(i).lt.minj).and.(sumj(i).gt.0))then
            minj=sumj(i)
            nminj=i
          end if
        end do
        ic=0
!
!remove velocity inversions        
!
        do i=1,obj%off_tot
          if(i.le.nminj)then
             sumj(i)=minj
           else if(ic.gt.0)then
             if(i.ge.nmaxj)sumj(i)=maxj
             if(sumj(i).lt.sumj(i-ic-1))then
                ic=ic+1
              else
                k=0
                xx=(sumj(i)-sumj(i-ic-1))/(ic+1)
                do j=i-ic,i-1
                  k=k+1
                  sumj(j)=sumj(i-ic-1)+k*xx
                end do
                ic=0
             end if
           else if(i.ge.nmaxj)then
             sumj(i)=maxj
           else
            if(sumj(i).lt.sumj(i-1))ic=ic+1
          end if
        end do
!
!calculate static (shft) at zero offset from edited refractor velocities        
!
        shft=0
        do k=1,obj%off_tot
          if(aoff(k).gt.avgoff)then
            aa=aoff(k)-avgoff
            bb=avgoff-aoff(k-1)
            i=k
            if(aa.lt.bb)i=k-1
            do j=i,2,-1
              shft=shft+(aoff(j)-aoff(j-1))*1000/sumj(j)
            end do
            shft=shft+aoff(1)*1000/sumj(1)
            go to 1200
          end if
        end do
 1200   continue
        shft=apoff(i)-shft
!
!calculate remaining statics from edited refractor velocities        
!
        shift(1)=shft
        iaoff(1)=0
        shift(2)=shft+aoff(1)*1000/sumj(1)
        iaoff(2)=nint(aoff(1))
        do i=2,obj%off_tot
          if(aoff(i).eq.0)aoff(i)=(i-2)*obj%off_inc &
                + iomin + obj%off_inc/2
          iaoff(i+1)=nint(aoff(i))
          shift(i+1)=(aoff(i)-aoff(i-1))*1000/sumj(i)+shift(i)
        end do
        shift(obj%off_tot+2)=obj%off_inc*1000/sumj(obj%off_tot)+ &
                             shift(obj%off_tot+1)
        iaoff(obj%off_tot+2)=iaoff(obj%off_tot+1)+obj%off_inc
        ipickt0=shift(1)
!*****************************************************************************
!       write .diag.off file                                
!*****************************************************************************
!
        write(lun_off,1300)nloop1
 1300   format(/,4x,'REFRACTION PROGRAM LOOP#',i4,4x,&
              'OFFSET VELOCITY ESTIMATION',/)
        write(lun_off,1310)
 1310   format('OFFSET AVERAGE    LAST ITERATION     FIRST ITERATION  ',&
           '   SHIFT     STD',/,'NUMBER  OFFSET  PICK  NUM TR   VEL',&
           '  PICK  NUM TR   VEL')
        do i=2,obj%off_tot+1
          write(lun_off,1320)i,iaoff(i),nint(apoff(i-1)),noff1(i-1),&
                          nint(sumj(i-1)),nint(apoff1(i-1)),&
                          noff2(i-1),ivel(i-1),shift(i),stdp(i-1)
 1320     format(i6,i8,2(i6,i8,i6),2f8.2)
        end do
        write(lun_off,1330)
 1330   format(/,2x,'PICKSTD  AVGDIF  DIFSTD   NSTDD     ',&
               '#TR    NSTD')
        write(lun_off,1340)stdp2,avgdif,stdd4,ictd2,ntr,ictps1
 1340   format(1x,3f8.2,3i8)
        write(lun_off,1345)stdp2,stdp3,gstd3
 1345   format(2x,'OFFSET STD    =',f7.2,3x,'AVG STD         =',&
             f6.2,' STDSTD =',f6.2,/)
        write(lun_off,*)' SIMPLE AVERAGE VELOCITY    = ',isavgvel
        write(lun_off,*)' WEIGHTED AVERAGE VELOCITY  = ',iwavgvel
        write(lun_off,*)' AVGDIF AVERAGE VELOCITY    = ',idavgvel
        write(lun_off,*)' COMBINED AVERAGE VELOCITY  = ',iavgvel
        write(lun_off,*)' AVERAGE OFFSET             = ',avgoff
        write(lun_off,*)' AVERAGE PICK               = ',avgpoff
        write(lun_off,*)' PICK AT IOMIN              = ',ipick0
        write(lun_off,*)' PICK AT T=0                = ',ipickt0
        write(lun_off,*)' DATUM CORRECTION           = ',idstat
!        write(lun_off,*)'            '
!******************************************************************************
        call timer_stop(timer1)
        call timer_fetch(timer1,id1,d1,d2,d3,d4,d5,d6,d7,d8,d9,d10,t2)
        call timer_clear(timer1)
        write(lun_off,1350)t2
 1350   format(2x,'ELAPSED TIME  = ',f8.2,' SECONDS',/)
        close(lun_off) 
        if(oflag.eq.0)then
          istdo=nint(stdp2*100)
          if(oldstdo.eq.istdo)then
            cstdo=cstdo+1
           else
            cstdo=1
            oldstdo=istdo
          end if
          if(cstdo.ge.5)oflag=1
        end if
        if(ovflag.eq.0)then
          if(oldvelo.eq.iavgvel)then
            cvelo=cvelo+1
           else
            cvelo=1
            oldvelo=iavgvel
          end if
          if(cvelo.ge.5)ovflag=1
        end if
!******************************************************************************
        open(lun_outp,file=outpname,status='old',position='append')
        write(lun_outp,1400)stdp2,iavgvel,gstd3
 1400   format(8x,'OFFSET STD    =',f7.2,3x,'AVG VELOCITY    =',&
             i6,' STDSTD =',f6.2)
        close(lun_outp)
!******************************************************************************
!******************************************************************************
!******************************************************************************
!******************************************************************************
!******************************************************************************
!******************************************************************************
!******************************************************************************
!
!       elevation calculations
!
!*****************************************************************************
        call timer_start(timer1)
        nitr=0
!
!       begin iteration loop                                  
 2000   continue
        nitr=nitr+1
!
!reset array values to zero
        do ii=1,nelv
          ap(ii)=0
          xpick(ii)=0
          xvel(ii)=0
          nxpick(ii)=0
          stdp(ii)=0
        end do
!
!start loop over shots
        do i=1,i1
!
!  start loop over recievers
          do j=1,i9-1,10
!
!    start trace selection if
!       select trace if:       
!       elevations > 0 
!       pick > 0 
!       (if offset range is specified)pick is within offset range 
! 
            if((elevs(i).gt.0).and.&
               (ielevr(i,j).gt.0).and.&
               (ipick(i,j).gt.0).and.&
               ((obj%off_init.lt.0).or.((obj%off_init.ge.0)&
                      .and.(ioff(i,j).ge.obj%off_init).and.&
                      (ioff(i,j).le.obj%off_last))))&
            then
!
!    calculate offset static
!
              k1=0
 2100         continue
              k1=k1+1
              if (ioff(i,j).ge.iaoff(k1))go to 2100
              x1=ioff(i,j)-iaoff(k1-1)
              x2=iaoff(k1)-iaoff(k1-1)
              x3=shift(k1)-shift(k1-1)
              x4=x1*x3/x2
              opick=shift(k1-1)+x4
!
!    calculate reciever ordinal
!
              iloc1=numx*(ihw36(i,j)-miny)+ihw35(i,j)-minx+1
              k3=nloc(iloc1)
!
!    get pick value and elevation for trace a
!
              picka=ipick(i,j)-opick-stats(i)-statr(k3)
              if(nloop1.eq.1)picka=ipick(i,j)-opick
              eleva=ielevr(i,j)
!    get new reciever b from shot i
              do k=j+1,i9
!
              y1=ihw36(i,k)-ihw36(i,j)
              y2=ihw35(i,k)-ihw35(i,j)
              y3=y1**2+y2**2
              y4=sqrt(y3)
              iy4=nint(y4)
!        start trace selection if
!           select trace if:       
!           elevations > 0 
!           pick > 0 
!           distance between traces >4 and <51
!           (if offset range is specified)pick is within offset range 
!     
                if((elevs(i).gt.0)&
                   .and.(ielevr(i,k).gt.0)&
                   .and.(ipick(i,k).gt.0)&
                   .and.(iy4.ge.5)&
                   .and.(y4.le.50)&
                   .and.((obj%off_init.lt.0)&
                         .or.((obj%off_init.ge.0).and.&
                           (ioff(i,k).ge.obj%off_init).and.&
                           (ioff(i,k).le.obj%off_last))))&
                then
!
!        calculate offset static
!
                  k1=0
 2110             continue
                  k1=k1+1
                  if (ioff(i,k).ge.iaoff(k1))go to 2110
                  x1=ioff(i,k)-iaoff(k1-1)
                  x2=iaoff(k1)-iaoff(k1-1)
                  x3=shift(k1)-shift(k1-1)
                  x4=x1*x3/x2
                  opick=shift(k1-1)+x4
!
!        calculate reciever ordinal
!
                  iloc1=numx*(ihw36(i,k)-miny)+ihw35(i,k)-minx+1
                  k3=nloc(iloc1)
!
!        get pick value and elevation for trace b
!
                  pickb=ipick(i,k)-opick-stats(i)-statr(k3)
                  if(nloop1.eq.1)pickb=ipick(i,k)-opick
                  elevb=ielevr(i,k)
!        calculate delta values trace b to a
                  deltap=pickb-picka
                  deltae=elevb-eleva
                  if((deltae.ne.0).and.(deltap.ne.0))then
!        if delta elevation is negative change sign of delta values
                    if(deltae.lt.0)then
                      deltap=deltap*(-1.0)
                      deltae=deltae*(-1.0)
                    end if
!        set group ii = nint(deltae)
                    ii=nint(deltae)
!        use this trace pair if
!           ii > 1 and ii < nelev       
!           if vel > 0 or vel < refvel (vel = deltae*1000/deltap)
!           delatp meets min/max test  
!     
                    if((ii.ge.1).and.(ii.le.nelv))then
                      if((deltap.gt.0).and.((ii*1000/deltap).le.iavgvel)&
                         .and.((nitr.eq.1).or.((nitr.gt.1).and.&
                                    (deltap.ge.xmine(ii)).and.&
                                      (deltap.le.xmaxe(ii)))))&
                      then
                        nxpick(ii)=nxpick(ii)+1
                        xpick(ii)=xpick(ii)+deltap
                        stdp(ii)=stdp(ii)+deltap**2
                      end if
                    end if
                  end if
                end if
!    end trace b selection if
!
              end do
!    end trace b loop over recievers
!
            end if
!    end trace a selection if
!
          end do
!  end loop trace a over recievers
!
        end do
!end loop over shots
!
!set sum variables to zero
!
        if(nitr.eq.2)ictps1=nsxpick
        sxvel=0
        sxvel2=0
        nsxvel=0
        nsxpick=0
        sxpick=0
        gavge=0
        stdp3=0
        gstd3=0
!
!start average and std loop
!
        do ii=1,nelv
          if((nxpick(ii).gt.2).and.(xpick(ii).gt.0))then
            xstd(ii)=stdp(ii)
            ap(ii)=xpick(ii)/nxpick(ii)
            stdp(ii)=stdp(ii)/nxpick(ii)
            stdp(ii)=stdp(ii)-ap(ii)**2
            stdp(ii)=sqrt(stdp(ii))
            xmine(ii)=ap(ii)-stdp(ii)
            xmaxe(ii)=ap(ii)+stdp(ii)
            stdp3=stdp3+stdp(ii)
            gstd3=gstd3+stdp(ii)**2
            xvel(ii)=ii*1000/ap(ii)
            sxvel=sxvel+xvel(ii)
            sxvel2=sxvel2+xvel(ii)*nxpick(ii)
            nsxvel=nsxvel+1
            nsxpick=nsxpick+nxpick(ii)
            sxpick=sxpick+xpick(ii)
            gavge=gavge+(ii*nxpick(ii))
           else
            stdp(ii)=0
            ap(ii)=0
            xmine(ii)=xmine(ii-1)+obj%off_inc/xvel(i-1)
            xmaxe(ii)=xmaxe(ii-1)+obj%off_inc/xvel(i-1)
          end if
        end do
        if(nsxpick.ne.0)then
          gavge=gavge/nsxpick
          gap=sxpick/nsxpick
          ixvel=nint(sxvel/nsxvel)
          ixvel2=nint(sxvel2/nsxpick)
          ixvel3=nint(gavge*1000/gap)
          stdp3=stdp3/nsxvel
          gstd3=gstd3/nsxvel
          gstd3=gstd3-stdp3**2
          gstd3=sqrt(gstd3)
        end if
!
!check to see if finished with iterations 
!
        if(nitr.ne.obj%num_outly)go to 2000
!
!find A & B to fit STATIC = A*e**(B*ELEV)
!
        aa=1
        ckbval=nelv-1
        ckbval2=0
 2115   continue
        bval=0
        nbval=0
        if(ckbval2.gt.6)then
          open(lun_outp,file=outpname,status='old',position='append')
          write(lun_outp,*)'**   UNABLE TO CALCULATE ELEV CORRECTION'
          close(lun_outp)
          do j=i,nelv
            sumj(j)=0
            eshift(j)=0
          end do
        end if          
        do i=1,ckbval
          do j=1+i,nelv
            if((ap(j).gt.0).and.(ap(i).gt.0))then
              bval=bval+(log(ap(j))-log(ap(i)))/(log(exp(aa))*(j-i))
              nbval=nbval+1
            end if
          end do
        end do
        bval=bval/nbval
        if(bval.lt.0)then
          ckbval2=ckbval2+1
          xbval=nelv
          xbval=xbval/(ckbval2*.5+1)-1
          ckbval=nint(xbval)
          go to 2115
        end if
          do j=i,nelv
            sumj(j)=0
            eshift(j)=0
          end do
        aval=0
        naval=0
        do i=1,nelv
          if(ap(i).gt.0)then
            aval=aval+ap(i)/(exp(aa)**(i*bval))
            naval=naval+1
          end if
        end do
        aval=aval/naval
        do i=1,nelv
          eshift(i)=aval*(exp(aa)**(bval*i))-aval
          sumj(i)=i*1000/(eshift(i)+aval)
          if(((sumj(i)-sumj(i-1)).le.0)&
            .or.(sumj(i).gt.obj%vel_corr))&
          then
            do j=i,nelv
              sumj(j)=sumj(i-1)
              eshift(j)=eshift(j-1)+1000/sumj(j)
            end do
            go to 2120
          end if            
        end do
 2120   continue
        if(nbval.lt.200)then
          do i=1,nelv
            sumj(i)=min(obj%vel_corr,iavgvel)
            eshift(i)=i/sumj(i)
          end do
        end if
!*****************************************************************************
!       write .diag.elev file                                
!*****************************************************************************
        write(lun_elev,2200)nloop1
 2200   format(/,4x,'REFRACTION PROGRAM LOOP#',i4,4x,&
                'ELEVATION DIFERENTIAL CALCULATIONS',/)
        write(lun_elev,2210)
 2210   format(/,6x,'DELTA_E',4x,'SHIFT',5x,'VEL',5x,'DELTA_P',&
               5x,'VEL',3x,'NUMPICK',5x,'STD E')
        do ii=1,nelv
          write(lun_elev,2220)ii,eshift(ii),nint(sumj(ii)),ap(ii),&
                   nint(xvel(ii)),nxpick(ii),stdp(ii)
 2220     format(5x,i5,5x,f7.3,3x,i5,5x,f7.3,3x,i5,2x,i8,5x,f5.2)
        end do
        emax=aval*(exp(aa)**(bval*nelv))-aval
        write(lun_elev,2230)ixvel,ixvel2,ixvel3
 2230   format(/,4x,'AVG VEL =',i6,'   WEIGHTED AVG VEL =',&
               I6,'    ELEV DIF VEL =',i6)
        write(lun_elev,2240)stdp3,gstd3,nint(sumj(nelv))
 2240   format(4x,'AVG STD =',f7.3,2x,'STDSTD =',f7.2,13x,&
               'MAXIMUM VEL  =',i6)
        write(lun_elev,2250)gavge,gap
 2250   format(4x,'AVG ELEVEVATION DIFFERENCE =',f9.3,&
                9x,'AVG PICK    =',f7.2)
        write(lun_elev,2260)ictps1,nsxpick
 2260   format(4x,'1ST ITERATION TRACES =',i10,&
               ' LAST ITERATION TRACES =',i10)
        write(lun_elev,2270)aval,bval,nint(emax)
 2270   format(4x,'STATIC=A*e**(B*ELEV)',2x,'A=',f6.2,1x,' B=',e12.5,&
               1x,' EMAX       =',i6)
        xbval=(1/(ckbval2*.5+1))*100
        write(lun_elev,2275)nint(xbval)
 2275   format(4x,'USED ',I3,'% OF ELEVATIONS TO CALCULATE B VALUE')
        call timer_stop(timer1)
        call timer_fetch(timer1,id1,d1,d2,d3,d4,d5,d6,d7,d8,d9,d10,t2)
        call timer_clear(timer1)
        write(lun_elev,2280)t2
 2280   format(4x,'ELEV TOTAL ELAPSED TIME IS ',f10.2,' SECONDS',/)
        close(lun_elev)
!
!if program is checking for convergence
!
!check if weighted average velocity has been the same for 5 iterations
!
        if(evflag.eq.0)then
          if(oldvele.eq.ixvel2)then
            cvele=cvele+1
           else
            cvele=1
            oldvele=ixvel2
          end if
          if(cvele.ge.5)evflag=1
        end if
!
!check if elev diff velocity has been the same for 5 iterations
!
        if(e2vflag.eq.0)then
          if(oldvele2.eq.ixvel3)then
            cvele2=cvele2+1
           else
            cvele2=1
            oldvele2=ixvel3
          end if
          if(cvele2.ge.5)e2vflag=1
        end if
!******************************************************************************
        open(lun_outp,file=outpname,status='old',position='append')
        write(lun_outp,2300)stdp3,ixvel3,gstd3
 2300   format(8x,'ELEVATION STD =',f7.2,3x,'ELEV DIF VEL    =',&
            i6,' STDSTD =',f6.2)
        if(ckbval2.ne.0)then
          write(lun_outp,2310)nint(xbval)
 2310     format(8x,'* USED ',I3,'% OF ELEVATIONS TO CALCULATE B VALUE')
        end if
        close(lun_outp)
!******************************************************************************
!******************************************************************************
!******************************************************************************
!******************************************************************************
!******************************************************************************
!
!       shot statics
!
!******************************************************************************
        call timer_start(timer1)
        nitr=0
!
!       begin iteration loop                                  
 3000   continue
        nitr=nitr+1
!
!reset array values to zero
        do i=1,i1
          apsht(i)=0
          psht(i)=0
          nsht1(i)=0
          stdp(i)=0
        end do
!
!start loop over shots
        do i=1,i1
!
!  start loop over recievers
          do j=1,i9
!
!    start trace selection if
!       select trace if:       
!       elevations > 0 
!       pick > 0 
!       (if offset range is specified)pick is within offset range 
! 
            if((elevs(i).gt.0).and.&
               (ielevr(i,j).gt.0).and.&
               (ipick(i,j).gt.0).and.&
               ((obj%off_init.lt.0).or.((obj%off_init.ge.0)&
                      .and.(ioff(i,j).ge.obj%off_init).and.&
                      (ioff(i,j).le.obj%off_last))))&
            then
!
!    calculate offset static
!
              k1=0
 3100         continue
              k1=k1+1
              if (ioff(i,j).ge.iaoff(k1))go to 3100
              x1=ioff(i,j)-iaoff(k1-1)
              x2=iaoff(k1)-iaoff(k1-1)
              x3=shift(k1)-shift(k1-1)
              x4=x1*x3/x2
              opick=shift(k1-1)+x4
!
!    calculate elevation static
!
              te1=elevs(i)-iavgelev
              te1a=abs(te1)
              ia1=nint(te1a)
              if(ia1.ne.0)ia2=nint(te1/te1a)
              if(ia1.eq.0)ia2=0
              te2=ielevr(i,j)-iavgelev
              te2a=abs(te2)
              ib1=nint(te2a)
              if(ib1.ne.0)ib2=nint(te2/te2a)
              if(ib1.eq.0)ib2=0
              te3=ia2*eshift(ia1)+ib2*eshift(ib1)
              epick=te3
!
!    calculate reciever ordinal
!
              iloc1=numx*(ihw36(i,j)-miny)+ihw35(i,j)-minx+1
              k3=nloc(iloc1)
!
!    adjust shot pick
!
            spick1=ipick(i,j)-opick-epick-statr(k3)
            if(nloop1.eq.1)spick1=ipick(i,j)-opick-epick
!
!    min/max tests
!
              if((nitr.eq.1).or.&
                ((nitr.gt.1).and.&
                  (spick1.ge.xmins(i)).and.&
                  (spick1.le.xmaxs(i))))&
              then
!
                nsht1(i)=nsht1(i)+1
                psht(i)=psht(i)+spick1
                stdp(i)=stdp(i)+spick1**2
              end if
            end if
!    end trace selection if
!
          end do
!  end loop over recievers
!
        end do
!end loop over shots
!
        ictps1=nct1
        nct1=0
        avgpsht=0
        nstdp3=0
        stdp3=0
        gstd3=0
!
!set factor for scaling the std for this iteration
        fact=1
        if(obj%num_outly.gt.1)then
          fact=obj%num_outly-nitr-1
          fact=fact/(obj%num_outly-1)
          fact=1+fact
        end if
!
!start average and std loop
!
        do i=1,i1
          if(nsht1(i).gt.0)then
            avgpsht=avgpsht+psht(i)
            nct1=nct1+nsht1(i)
            apsht(i)=psht(i)/nsht1(i)
            stdp(i)=stdp(i)/nsht1(i)
            stdp(i)=stdp(i)-apsht(i)**2
            stdp(i)=sqrt(stdp(i))
            if(nsht1(i).lt.2)stdp(i)=0
            if(nint(stdp(i)).gt.0)then
               xmins(i)=apsht(i)-stdp(i)*fact
               xmaxs(i)=apsht(i)+stdp(i)*fact
               stdp3=stdp3+stdp(i)
               gstd3=gstd3+stdp(i)**2
               nstdp3=nstdp3+1
             else 
               xmins(i)=apsht(i)-stdp3/nstdp3*fact
               xmaxs(i)=apsht(i)+stdp3/nstdp3*fact
            end if
          end if
        end do
!end average and std loop
!
!global average and std
        avgpsht=avgpsht/nct1
        stdp3=stdp3/nstdp3
        gstd3=gstd3/nstdp3
        gstd3=gstd3-stdp3**2
        gstd3=sqrt(gstd3)
!
!check to see if finished with iterations 
        if(nitr.ne.obj%num_outly)go to 3000
!*****************************************************************************
!       write .diag.src file                                
!*****************************************************************************
!
        write(lun_src,3200)nloop1
 3200   format(/,4x,'REFRACTION PROGRAM LOOP#',i4,4x,'SHOT STATIC CALCULATION'&
               ,/)
        write(lun_src,3210)avgpsht,stdp3,gstd3
 3210   format(/,4x,'AVGPICK  =',f10.2,4x,'AVGSTD =',f10.2,4x,&
              'STDSTD    =',f10.2)
        write(lun_src,3220)ntr,ictps1,nct1
 3220   format(4x,'INPUT TR =',i10,4x,'#TRSTD =',i10,4x,&
              '#TRFNLAVG =',i10,/)
        write(lun_src,3230)
 3230   format(3x,'TOTAL     SEQ   HDR   HDR   HDR   NUM    AVG    STD',&
           '   SHOT   ELEV   SHOT    STD',/,2X,'STATIC  NUMBER     X     Y',&
           '     9    TR   PICK          ELEV   STAT   STAT   FLAG')
        numa=0
        do i=1,i1
          ihw34=ishw(i)/numx
          ihw33=ishw(i)-(ihw34*numx)
          ihw34=ihw34+miny
          ihw33=ihw33+minx-1
          shotc=apsht(i)-avgpsht
          stdmax=stdp3+gstd3*2.0
          aaaa=' '
          if(stdp(i).gt.stdmax)aaaa='  *****'
          if(stdp(i).gt.stdmax)then
            numa=numa+1
            stdpa(numa)=stdp(i)
            inum(numa)=i
          end if
          if(nsht1(i).eq.0)shotc=0
          stats(i)=shotc
          te1=iavgelev-elevs(i)
          te1a=abs(te1)
          ia1=nint(te1a)
          if(ia1.ne.0)ia2=nint(te1/te1a)
          if(ia1.eq.0)ia2=0
          x1=ia2*eshift(ia1)
          x2=x1-stats(i)
          write(lun_src,3240)x2,i,ihw33,ihw34,nint(fhw9(i)),nsht1(i),&
                        apsht(i),stdp(i),nint(elevs(i)),x1,shotc,aaaa
 3240     format(f8.2,i8,4i6,2f7.2,i7,2f7.2,a7)
        end do
!
!list shots with large std
!
        xsum=0
        xstd0=0
        do i=1,numa
          xsum=xsum+stdpa(i)
        end do
        asum=xsum/numa
        do i=1,numa
          xstd0=xstd0+(stdpa(i)-asum)**2
        end do
        xstd0=xstd0/(numa-1)
        xstd0=sqrt(xstd0)
        write(lun_src,3250)numa
 3250   format(/,4x,'NUMBER OF QUESTIONABLE REC = ',i5)
        write(lun_src,3260)asum,xstd0
 3260   format(4x,'AVG OF POOR STD= ',f7.2,'  STDSTD= ',f7.2)
        write(lun_src,3270)
 3270   format(/,5x,'SHOT NUM   SHOTX   SHOTY       STD')
        do i=1,numa
          ihw34=ishw(inum(i))/numx
          ihw33=ishw(inum(i))-(ihw34*numx)
          ihw34=ihw34+miny
          ihw33=ihw33+minx-1
          write(lun_src,3280)inum(i),ihw33,ihw34,stdpa(i)
 3280     format(5x,3i8,f10.2)
        end do
        call timer_stop(timer1)
        call timer_fetch(timer1,id1,d1,d2,d3,d4,d5,d6,d7,d8,d9,d10,t2)
        call timer_clear(timer1)
        write(lun_src,3290)t2
 3290   format(/,4x,'ELAPSED TIME =',f8.2,' SECONDS',/)
        close(lun_src)
!
!if program is checking for convergence
!
!check if avgstd has been the same for 5 iterations
!
        if(sflag.eq.0)then
          istds=nint(stdp3*100)
          if(oldstds.eq.istds)then
            cstds=cstds+1
           else
            cstds=1
            oldstds=istds
          end if
          if(cstds.ge.5)sflag=1
        end if
!
!check if stdstd has been the same for 5 iterations
!
        if(sflag3.eq.0)then
          istds3=nint(gstd3*100)
          if(oldstds3.eq.istds3)then
            cstds3=cstds3+1
           else
            cstds3=1
            oldstds3=istds3
          end if
          if(cstds3.ge.5)sflag3=1
        end if
!
!write shot information to .outp file
!
        open(lun_outp,file=outpname,status='old',position='append')
        write(lun_outp,3300)stdp3,numa,gstd3
 3300   format(8x,'SHOT STD      =',f7.2,3x,'# FLAGED SHOTS  =',i6,&
            ' STDSTD =',f6.2)
        close(lun_outp)
!******************************************************************************
!******************************************************************************
!******************************************************************************
!******************************************************************************
!******************************************************************************
!
!       receiver static
!
!******************************************************************************
!*****************************************************************************
        call timer_start(timer1)
        nitr=0
!
!       begin iteration loop                                  
 4000   continue
        nitr=nitr+1
!
!reset array values to zero
        do i=1,nrec
          aprec(i)=0
          prec(i)=0
          nrec1(i)=0
          stdp(i)=0
        end do
!
!start loop over shots
        do i=1,i1
!
!  start loop over recievers
          do j=1,i9
!
!    start trace selection if
!       select trace if:       
!       elevations > 0 
!       pick > 0 
!       (if offset range is specified)pick is within offset range 
! 
            if((elevs(i).gt.0).and.&
               (ielevr(i,j).gt.0).and.&
               (ipick(i,j).gt.0).and.&
               ((obj%off_init.lt.0).or.((obj%off_init.ge.0)&
                      .and.(ioff(i,j).ge.obj%off_init).and.&
                      (ioff(i,j).le.obj%off_last))))&
            then
!
!    calculate offset static
!
              k1=0
 4100         continue
              k1=k1+1
              if (ioff(i,j).ge.iaoff(k1))go to 4100
              x1=ioff(i,j)-iaoff(k1-1)
              x2=iaoff(k1)-iaoff(k1-1)
              x3=shift(k1)-shift(k1-1)
              x4=x1*x3/x2
              opick=shift(k1-1)+x4
!
!    calculate elevation static
!
              te1=elevs(i)-iavgelev
              te1a=abs(te1)
              ia1=nint(te1a)
              if(ia1.ne.0)ia2=nint(te1/te1a)
              if(ia1.eq.0)ia2=0
              te2=ielevr(i,j)-iavgelev
              te2a=abs(te2)
              ib1=nint(te2a)
              if(ib1.ne.0)ib2=nint(te2/te2a)
              if(ib1.eq.0)ib2=0
              te3=ia2*eshift(ia1)+ib2*eshift(ib1)
              epick=te3
!
!    calculate reciever ordinal
!
              iloc1=numx*(ihw36(i,j)-miny)+ihw35(i,j)-minx+1
              k3=nloc(iloc1)
!
!    adjust reciever pick
!
              rpick1=ipick(i,j)-opick-epick-stats(i)
!
!    min/max tests
!
              if((nitr.eq.1).or.&
                ((nitr.gt.1).and.&
                  (rpick1.ge.xminr(k3)).and.&
                  (rpick1.le.xmaxr(k3))))&
              then
!
                nrec1(k3)=nrec1(k3)+1
                prec(k3)=prec(k3)+rpick1
                stdp(k3)=stdp(k3)+rpick1**2
              end if
            end if
!    end trace selection if
!
          end do
!  end loop over recievers
!
        end do
!end loop over shots
!
        ictps1=nct1
        nct1=0
        avgprec=0
        nstdp3=0
        stdp3=0
        gstd3=0
!
!set factor for scaling the std for this iteration
        fact=1
        if(obj%num_outly.gt.1)then
          fact=obj%num_outly-nitr-1
          fact=fact/(obj%num_outly-1)
          fact=1+fact
        end if
!
!start average and std loop
        do i=1,nrec
          if(nrec1(i).gt.0)then
            avgprec=avgprec+prec(i)
            nct1=nct1+nrec1(i)
            aprec(i)=prec(i)/nrec1(i)
            stdp(i)=stdp(i)/nrec1(i)
            stdp(i)=stdp(i)-aprec(i)**2
            stdp(i)=sqrt(stdp(i))
            if(nrec1(i).lt.2)stdp(i)=0
            if(nint(stdp(i)).gt.0)then
               xminr(i)=aprec(i)-stdp(i)*fact
               xmaxr(i)=aprec(i)+stdp(i)*fact
               stdp3=stdp3+stdp(i)
               gstd3=gstd3+stdp(i)**2
               nstdp3=nstdp3+1
             else 
               xminr(i)=aprec(i)-stdp3/nstdp3*fact
               xmaxr(i)=aprec(i)+stdp3/nstdp3*fact
            end if
          end if
        end do
!end average and std loop
!
!global average and std
        avgprec=avgprec/nct1
        stdp3=stdp3/nstdp3
        gstd3=gstd3/nstdp3
        gstd3=gstd3-stdp3**2
        gstd3=sqrt(gstd3)
!
!check to see if finished with iterations 
        if(nitr.ne.obj%num_outly)go to 4000
!
!****************************************************************************
!       write .diag.rec file                                
!
        write(lun_rec,4200)nloop1
 4200   format(/,4x,'REFRACTION PROGRAM LOOP#',i4,4x,&
               'RECIEVER STATIC CALCULATION',/)
        write(lun_rec,4210)avgprec,stdp3,gstd3
 4210   format(/,4x,'AVGPICK=',f7.2,2x,'AVGSTD=',f7.2,2x,'STDSTD=',f7.2)
        write(lun_rec,4220)ntr,ictps1,nct1
 4220   format(4x,'#TR=',i8,2x,'#TRSTD=',i8,2x,'#TRFNLAVG=',i8,/)
        write(lun_rec,4230)
 4230   format(3x,'TOTAL     SEQ   HDR   HDR   LOC   NUM    AVG    STD',&
           '    REC   ELEV    REC    STD',/,2X,'STATIC  NUMBER     X     Y',&
           '     #    TR   PICK          ELEV   STAT   STAT   FLAG')
         numa=0
        do i=1,nrec
          iihw36=(iloc(i)-1)/numx
          iihw35=iloc(i)-iihw36*numx
          iihw36=iihw36+miny
          iihw35=iihw35+minx-1
          recc=aprec(i)-avgprec
          stdmax=stdp3+gstd3*2
          aaaa=' ' 
          if(stdp(i).gt.stdmax)then
            aaaa='  *****'
            numa=numa+1
            stdpa(numa)=stdp(i)
            inum(numa)=i
          end if
          if(nrec1(i).eq.0)recc=0
          statr(i)=recc
          te1=iavgelev-elevi(i)
          te1a=abs(te1)
          ia1=nint(te1a)
          if(ia1.ne.0)ia2=nint(te1/te1a)
          if(ia1.eq.0)ia2=0
          x1=ia2*eshift(ia1)
          x2=x1-statr(i)
          write(lun_rec,4240)x2,i,iihw35,iihw36,iloc(i),nrec1(i),&
                        aprec(i),stdp(i),nint(elevi(i)),x1,recc,aaaa
 4240     format(f8.2,i8,4i6,2f7.2,i7,2f7.2,a7)
        end do
!
!list recivers with large std
!
        xsum=0
        xstd0=0
        do i=1,numa
          xsum=xsum+stdpa(i)
        end do
        asum=xsum/numa
        do i=1,numa
          xstd0=xstd0+(stdpa(i)-asum)**2
        end do
        xstd0=xstd0/(numa-1)
        xstd0=sqrt(xstd0)
        write(lun_rec,4250)numa
 4250   format(/,4x,'NUMBER OF QUESTIONABLE REC = ',i5)
        write(lun_rec,4260)asum,xstd0
 4260   format(4x,'AVG OF POOR STD= ',f7.2,'  STDSTD= ',f7.2)
        write(lun_rec,4270)
 4270   format(/,5x,' REC NUM    RECX    RECY       STD')
        do i=1,numa
          iihw36=(iloc(inum(i))-1)/numx
          iihw35=iloc(inum(i))-iihw36*numx
          iihw36=iihw36+miny
          iihw35=iihw35+minx-1
          write(lun_rec,4280)inum(i),iihw35,iihw36,stdpa(i)
 4280     format(5x,3i8,f10.2)
        end do
!
!write elapsed time
!
        call timer_stop(timer1)
        call timer_fetch(timer1,id1,d1,d2,d3,d4,d5,d6,d7,d8,d9,d10,t2)
        call timer_clear(timer1)
        write(lun_rec,4290)t2
 4290   format(/,4x,'ELAPSED TIME =',f8.2,' SECONDS',/)
        close(lun_rec)
!
!if program is checking for convergence
!
!check if avgstd has been the same for 5 iterations
!
        if(rflag.eq.0)then
          istdr=nint(stdp3*100)
          if(oldstdr.eq.istdr)then
            cstdr=cstdr+1
           else
            cstdr=1
            oldstdr=istdr
          end if
          if(cstdr.ge.5)rflag=1
        end if
!
!check if stdstd has been the same for 5 iterations
!
        if(rflag3.eq.0)then
          istdr3=nint(gstd3*100)
          if(oldstdr3.eq.istdr3)then
            cstdr3=cstdr3+1
           else
            cstdr3=1
            oldstdr3=istdr3
          end if
          if(cstdr3.ge.5)rflag3=1
        end if
!
!write reciever information to .outp file
!
        open(lun_outp,file=outpname,status='old',position='append')
        write(lun_outp,4300)stdp3,numa,gstd3
 4300   format(8x,'RECEIVER STD  =',f7.2,3x,'# FLAGED REC    =',&
             i6,' STDSTD =',f6.2)
        close(lun_outp)
!****************************************************************************
!       write .diag.ref file                                     
!****************************************************************************
        write(lun_ref,1)obj%path_pick
    1   format(a80)
!           There are all from iteration 1  
        write(lun_ref,2)obj%num_iter_used,obj%num_outly,nloop1
        write(lun_ref,2)obj%off_init,obj%off_last,obj%off_tot,obj%off_inc,&
                  obj%datum,obj%vel_corr
        write(lun_ref,2)iomin,iavgvel,ipick0,ipickt0,idstat
        write(lun_ref,2)minx,miny,numx,numy
    2   format(10i6)
        if(mine.lt.0)iavgelev=iavgelev+mine-1
        write(lun_ref,13)nelv,iavgelev,mine,nrec,nsht,ntr
   13   format(5i6,f7.3,i10)
        if(mine.lt.0)iavgelev=iavgelev-mine+1
!       write offset statics
        write(lun_ref,3)(iaoff(i),shift(i),i=1,obj%off_tot+2)
    3   format(i8,f8.3,i8,f8.3,i8,f8.3,i8,f8.3)
!       write elev statics
        write(lun_ref,3)(i,eshift(i),i=1,nelv)
!       write shot statics
        write(lun_ref,3)(ishw(i),stats(i),i=1,nsht)
!       write rec statics
        write(lun_ref,3)(iloc(i),statr(i),i=1,nrec)
        close(lun_ref)
        call timer_stop(timerg)
        call timer_fetch(timerg,id1,d1,d2,d3,d4,d5,d6,d7,d8,d9,d10,tg2)
!******************************************************************************
        testflag=oflag+ovflag+evflag+e2vflag+sflag+sflag3+rflag+rflag3
        if((testflag.eq.8).and.(obj%opt_iter.eq.'YES'))&
            obj%num_iter_used=nloop1
        open(lun_outp,file=outpname,status='old',position='append')
        write(lun_outp,36)tg2,testflag
   36   format(8x,'LOOP ELAPSED TIME =',f8.2,' SECONDS',&
            12x,'TESTFLAG =',i6)
        close(lun_outp)
!******************************************************************************
!******************************************************************************
!******************************************************************************
!******************************************************************************
!******************************************************************************
        if(nloop1.lt.obj%num_iter_used)then
          open(lun_src,file=srcname,status='old',position='append')
          open(lun_rec,file=recname,status='old',position='append')
          open(lun_off,file=offname,status='old',position='append')
          open(lun_elev,file=elevname,status='old',position='append')
!            Do not append refraction file - save only last iteration
          open(lun_ref,file=refname,status='old')
          go to 999
        endif
   39   continue
        call timer_stop(timerf)
        call timer_fetch(timerf,id1,d1,d2,d3,d4,d5,d6,d7,d8,d9,d10,tf2)
!******************************************************************************
        open(lun_outp,file=outpname,status='old',position='append')
        write(lun_outp,35)tf2
   35   format(//,4x,'TOTAL ELAPSED TIME =',f8.2,' SECONDS',/,4X,'WRITING &
                     &STATIC FILES')
        close(lun_outp)
!******************************************************************************
  803   continue
!             Needed for array allocation in refstat
        lgishw=-999999
        lgiloc=-999999
        do i=1,nsht
          lgishw=max(lgishw,ishw(i))
        enddo
          lgishw=max(lgishw,nsht,nrec,numx)
        do i=1,nrec
          lgiloc=max(lgiloc,iloc(i))
        enddo
          lgiloc=max(lgiloc,nsht,nrec,numx)

      call mem_free(xmins)
      call mem_free(xmaxs)
      call mem_free(xminr)
      call mem_free(xmaxr)
      call mem_free(fhw34)
      call mem_free(fhw33)
      call mem_free(poff)
      call mem_free(apoff)
      call mem_free(apoff1)
      call mem_free(aoff)
      call mem_free(stdp)
      call mem_free(dif)
      call mem_free(xminp)
      call mem_free(xmaxp)
      call mem_free(xmind)
      call mem_free(xmaxd)
      call mem_free(sumj)
      call mem_free(xpick)
      call mem_free(ap)
      call mem_free(xmaxe)
      call mem_free(xvel)
      call mem_free(xstd)
      call mem_free(xmine) 
      call mem_free(apsht)
      call mem_free(psht)
      call mem_free(stdpa)
      call mem_free(fhw9)
      call mem_free(aprec)
      call mem_free(prec)

 5000 continue
      if(obj%mode.ne.'ITER')call mfrs_refstat(obj,lun_outp,outpname)

      call mem_free(f3)
      call mem_free(f4)
      call mem_free(ishw)
      call mem_free(shift)
      call mem_free(iaoff)
      call mem_free(eshift)
      call mem_free(elevs)
      call mem_free(stats)
      call mem_free(elevi)
      call mem_free(iloc)
      call mem_free(statr)
      call mem_free(ihw35)
      call mem_free(ihw36)
      call mem_free(ioff)
      call mem_free(ielevr)
      call mem_free(ipick)


!</execute_only>


!!------------------------- finish update ----------------------------------!!
!!------------------------- finish update ----------------------------------!!
!!------------------------- finish update ----------------------------------!!


      return
      end subroutine mfrs_update
      subroutine mfrs_osadd(ipick,ioff,ffshft,iomin,inco,&
                            iomax,bulk,noff)
 
      integer,intent(inout) :: ipick
      integer,intent(in)    :: iomin,inco,ioff,iomax,bulk,noff
      real,pointer          :: ffshft(:)  

!         local variables
      real    :: dpick,xo,x1,x2,x3,x4,x5
      integer :: io

      if(ioff.le.0)then
        ipick=0
       elseif(ipick.ne.0)then
        xo = ioff 
        xo = xo - iomin 
        xo =(xo / inco) + 1 
        io = xo 
        x1 = inco 
        x2 = ioff -(io - 1) * inco - iomin 
        x3 = ffshft(io + 1) - ffshft(io) 
        x4 = x2 * x3 
        x5 = x4 / x1 
        dpick = x5 + ffshft(io) 
        if(ioff.lt.iomin) dpick = ffshft(1) 
        if(ioff.gt.iomax) dpick = ffshft(noff) 
!**           shift pick file to original times
        ipick = nint(dpick) - bulk + ipick 
      endif

      end subroutine mfrs_osadd
      subroutine mfrs_refstat(obj,lun_outp,outpname)
!***********************************************************************
!***********************************************************************
!     This routine reads the .diag.ref file and outputs static files
!                                                                       
!***********************************************************************
!     creates static files:    
!     path_out.off                                                      
!     path_out.elev                                                      
!     path_out.src   use for refraction static aer run                  
!     path_out.rec   use for refraction static aer run                  
!***********************************************************************
!***********************************************************************
!
      type(mfrs_struct),intent(inout) :: obj       
      integer, intent(in) :: lun_outp
      character(len=*) :: outpname
!-----------------------------------------------
!   L o c a l   V a r i a b l e s
!-----------------------------------------------
      integer :: nloop, nit, nloop1,mino, maxo, idat, &
                 idvel, iomin, iavgvel, ipick0, &
                 ntrdum,i,j,i2,i3,i4,i5,i6, &
                 i7,i8,i9,i10,i11,i12,i13,i14,l,j2,iloc1,k,noff1,k1,iioff,&
                 ihdr,ifnum,isnum,ifinc,nn,ihdrval,ia1,ia2,nsht2 
      integer :: istat,lun_elev,lghdrval,lun_off,lun_pick
      integer :: lun_rec,lun_ref,lun_src,nalloc,nstato
      real,pointer :: state(:),stato(:),statr1(:),stats1(:)
      real :: xavg4,f1,f2,a1,f5,f6,f7,fhw33dum,f8,f9,f10,fhw34dum, &
              fhw9,f11,xoff,x1,x2,x3,x4,fhdr,shdr,finc,sinc,te1,te1a
      character(len=4) :: junk
      character(len=9) :: junk4
      character(len=filename_length) :: tmpname,pickfile
      logical :: almost_equal
!***********************************************************************
!                 read the .diag.ref file                                       
!***********************************************************************
      if(obj%mode.eq.'STATIC')then
        call getlun(lun_ref,istat)
        if(istat.ne.0)then
          call pc_error('Unable to get unit for .diag.ref file')
        endif
        tmpname=trim(obj%path_out) // '.diag.ref'
        open(lun_ref, file=tmpname,status = 'old',iostat=istat) 
        if(istat.ne.0)call pc_error('Unable to open .diag.ref file')
        read(lun_ref, 1) pickfile 
    1   format(a40) 
        read(lun_ref, 2) nloop, nit, nloop1
        read(lun_ref, 2) mino, maxo, obj%off_tot, obj%off_inc, idat, idvel 
    2   format(10i6) 
        read(lun_ref, 2) iomin, iavgvel, ipick0, ipickt0, idstat 
        read(lun_ref, 2) minx, miny, numx, numy 
        read(lun_ref, 13) nelv, iavgelev, mine, nrec, nsht, xavg4, ntrdum 
   13   format(5i6,f7.3,i10) 

        call mem_alloc(elevi,nrec)
        elevi=0.0
        call mem_alloc(iaoff,obj%off_tot+2)
        call mem_alloc(shift,obj%off_tot+2)
!             read offset statics
        read(lun_ref, 3)(iaoff(i), shift(i), i = 1, obj%off_tot + 2) 
    3   format(i8,f8.3,i8,f8.3,i8,f8.3,i8,f8.3) 
        call mem_alloc(eshift,nelv)
!             read elev statics
        read(lun_ref, 3)(j, eshift(i), i = 1, nelv) 
        call mem_alloc(ishw,nsht)
        call mem_alloc(stats,nsht)
!             read shot statics
        read(lun_ref, 3)(ishw(i), stats(i), i = 1, nsht) 
        lgishw=-999999
        do i=1,nsht
          lgishw=max(lgishw,ishw(i))
        enddo
        lgishw=max(lgishw,nsht,nrec,numx)
        call mem_alloc(iloc,nrec)
        call mem_alloc(statr,nrec)
!             read rec sttatics
        read(lun_ref, 3)(iloc(i), statr(i), i = 1, nrec) 
        lgiloc=-999999
        do i=1,nrec
          lgiloc=max(lgiloc,iloc(i))
        enddo
        lgiloc=max(lgiloc,nsht,nrec,numx)
  103   continue 
        close(unit = lun_ref) 
!***********************************************************************
!                   read scrs pick file                                   
!***********************************************************************
        call getlun(lun_pick,istat)
        if(istat.ne.0)call pc_error('Unable to get unit number for pick file')
        open(lun_pick, file = obj%path_pick, status = 'old',iostat=istat)
        if(istat.ne.0)call pc_error('Unable to open path_pick file') 
        read(lun_pick, 4) i1, i2, i3, i4, i5, i6, i7, i8 
    4   format(8i6) 
        lgishw=max(lgishw,i3)
        read(lun_pick, 5) f1, i9, f2, a1, i10, i11, i12, i13, i14 
    5   format(f6.1,i6,f6.3,a6,5i4) 
        call mem_alloc(f3,i10)
        read(lun_pick, 6)(f3(l), l = 1, i10) 
    6   format(5f10.3) 
        call mem_alloc(f4,i11)
        read(lun_pick, 6)(f4(l), l = 1, i11) 
        j2 = i10 * i11 * i12 
        do j = 1, j2 
           read(lun_pick, 6) f5, f6, f7 
        end do 
        call mem_alloc(elevs,i1)
        do     i = 1, i1 
           read(lun_pick, 7) fhw33dum, f6, f7, elevs(i), f8 
           read(lun_pick, 7) f9, f10, fhw34dum, fhw9, f11 
    7   format(5f10.3) 
           call mem_alloc(ihw35,max(i1,1000),i9)
           call mem_alloc(ihw36,max(i1,1000),i9)
           call mem_alloc(ioff,max(i1,1000),i9)
           call mem_alloc(ielevr,max(i1,1000),i9)
           call mem_alloc(ipick,max(i1,1000),i9)
           do j = 1, i9 
              read(lun_pick, 8) ipick(i, j), ihw35(i, j), ihw36(i, j),       &
              ioff(i, j), ielevr(i, j)                                  
    8         format(5i6) 
              if(ipick(i, j) .eq.0) cycle
              iloc1 = numx *(ihw36(i, j) - miny) + ihw35(i, j) - minx + 1
              do  k = 1, nrec 
                 if(iloc(k) .eq.iloc1) then 
                    elevi(k) = ielevr(i, j) 
                    cycle
                 endif 
              end do 
           end do 
        end do 
        close(lun_pick) 
      endif  !  if(mode.eq.'STATIC')
!***********************************************************************
!     write offset static file                                          
!***********************************************************************
      xoff = iaoff(obj%off_tot + 2) 
      xoff = xoff / obj%off_inc 
      noff1 = nint(xoff) + 1 
      nalloc=noff1*5+5
      nstato=max(noff1,max(i1,1000),nalloc)
      call mem_alloc(stato,nstato)
      stato=0.0
      do 200 i = 1, noff1 
         do k1 = 2, obj%off_tot + 2 
         iioff =(i - 1) * obj%off_inc 
         if(iioff.gt.iaoff(obj%off_tot + 2) ) then 
            stato(i) = shift(obj%off_tot + 2) 
            goto 200 
         endif 
         if(iioff.lt.iaoff(k1) ) then 
            x1 = iioff - iaoff(k1 - 1) 
            x2 = iaoff(k1) - iaoff(k1 - 1) 
            x3 = shift(k1) - shift(k1 - 1) 
            x4 = x1 * x3 / x2 
            stato(i) = shift(k1 - 1) + x4 
            goto 200 
         endif 
         enddo 
  200 end do 
      tmpname=trim(obj%path_out) // '.off'
      call getlun(lun_off,istat)
      if(istat.ne.0)call pc_error('Unable to get unit number for offset file')
      open(lun_off, file = tmpname, status = 'replace',iostat=istat) 
      if(istat.ne.0)call pc_error('Unable to open offset file')
      ihdr = 6 
      i2 = 0 
      junk = 'MISC' 
      write(lun_off, 203) junk, ihdr, i2, i2, i2 
  203 format(a4,4x,5i4) 
      fhdr = 0 
      shdr = 1 
      finc = obj%off_inc 
      sinc = 1 
      ifnum = noff1 
      isnum = 1 
      write(lun_off, 204) fhdr, shdr, finc, sinc, ifnum, isnum 
  204 format(4f14.3,2i7) 
      write(lun_off, 205) 
  205 format('ISEP - STATICS FROM REFRACT.DIAG DATA') 
      junk4 = '+++END+++' 
      write(lun_off, 206) junk4 
  206 format(a9) 
      ifinc = nint(finc) 
      nn = int((ifnum + 4.1) / 5) 
      do 208, i = 1, nn 
         ihdrval =((i - 1) * 5) + 1 
         write(lun_off, 207) ihdrval, stato(ihdrval), stato(ihdrval + 1),  &
         stato(ihdrval + 2), stato(ihdrval + 3), stato(ihdrval + 4)  
  207 format(i8,e10.4,4(4x,e10.4)) 
  208 end do 
      close(lun_off) 
!***********************************************************************
!     write elevation static file                                       
!***********************************************************************
      nalloc=nelv*5+5
      call mem_alloc(state,nalloc)
      state=0.0
      do 300 i = 1, nelv 
         te1 = mine+i - 1 
         te1 = iavgelev - te1 
         te1a = abs(te1) 
         ia1 = nint(te1a) 
         if(ia1.ne.0) ia2 = nint(te1 / te1a) 
         if(ia1.eq.0) ia2 = 0 
         state(i) = ia2 * eshift(ia1) 
  300 end do 
      call getlun(lun_elev,istat)
      if(istat.ne.0)call pc_error('Unable to get unit number for elevation &
                                  &file')
      tmpname=trim(obj%path_out) // '.elev'
      open(lun_elev, file =tmpname, status ='replace', iostat=istat)
      if(istat.ne.0)call pc_error('Unable to open elevation file') 
      ihdr = 13 
      i2 = 0 
      i3 = 16 
      junk = 'MISC' 
      write(lun_elev, 203) junk, ihdr, i2, i3, i2 
      fhdr = mine 
      shdr = 1 
      finc = 1 
      sinc = 1 
      ifnum = nelv 
      isnum = 1 
      write(lun_elev, 204) fhdr, shdr, finc, sinc, ifnum, isnum 
      write(lun_elev, 205) 
      junk4 = '+++END+++' 
      write(lun_elev, 206) junk4 
      ifinc = nint(finc) 
      nn = int((ifnum + 4.1) / 5) 
      do i = 1, nn 
         ihdrval =((i - 1) * 5) + 1 
         write(lun_elev, 207) ihdrval, state(ihdrval), state(ihdrval + 1),  &
         state(ihdrval + 2), state(ihdrval + 3), state(ihdrval + 4)  
      end do 
      close(lun_elev) 
!***********************************************************************
!     write shot static file                                            
!***********************************************************************
      call mem_alloc(stats1,lgishw)
      stats1=0.0
      do i = 1, nsht 
         j = ishw(i) 
         te1 = iavgelev - elevs(i) 
         te1a = abs(te1) 
         ia1 = nint(te1a) 
         if(ia1.ne.0) ia2 = nint(te1 / te1a) 
         if(ia1.eq.0) ia2 = 0 
         x1 = ia2 * eshift(ia1) 
         stats1(j) = x1 - stats(i)+idstat 
      end do 
      call getlun(lun_src,istat)
      if(istat.ne.0)call pc_error('Unable to get unit for source static file')
      tmpname=trim(obj%path_out) // '.src'
      open(lun_src, file = tmpname, status = 'replace', iostat=istat)
      if(istat.ne.0)call pc_error('Unable to open source static file')
      ihdr = 33 
      i2 = 34 
      i3 = 0 
      junk = 'MISC' 
      if(miny.eq.0)then
        ihdr = 46
        i2 = 0
      end if
      write(lun_src, 203) junk, ihdr, i2, i3, i3 
      fhdr = minx 
      shdr = miny 
      finc = 1 
      sinc = 1 
      ifnum = numx 
      isnum = numy 
      write(lun_src, 204) fhdr, shdr, finc, sinc, ifnum, isnum 
      write(lun_src, 205) 
      junk4 = '+++END+++' 
      write(lun_src, 206) junk4 
      nsht2 = numx * numy 
      nn = int((nsht2 + 4.1) / 5) 
      lghdrval=-999999
      do i = 1, nn 
         ihdrval =((i - 1) * 5) + 1 
         lghdrval=max(lghdrval,ihdrval)
         do j = 1, 5 
!!!        if(stats1(ihdrval + j - 1) .eq.0.0) stats1(ihdrval + j - 1)= - .1e-29
            almost_equal=ameq(stats1(ihdrval + j - 1),0.0,0.00000001)
            if(almost_equal)stats1(ihdrval + j - 1)= - .1e-29
         end do 
         write(lun_src, 207) ihdrval, stats1(ihdrval), stats1(ihdrval + 1),&
         stats1(ihdrval + 2), stats1(ihdrval + 3), stats1(ihdrval + 4)
      end do 
      lghdrval=lghdrval+5
      close(lun_src) 
!***********************************************************************
!     write reciever static file                                        
!***********************************************************************
!!      call mem_alloc(statr1,nrec)
      nalloc=max(lgiloc,lghdrval)
      call mem_alloc(statr1,nalloc)
      statr1=0.0
      do i = 1, nrec 
         j = iloc(i) 
         te1 = iavgelev - elevi(i) 
         te1a = abs(te1) 
         ia1 = nint(te1a) 
         if(ia1.ne.0) ia2 = nint(te1 / te1a) 
         if(ia1.eq.0) ia2 = 0 
         x1 = ia2 * eshift(ia1) 
         statr1(j) = x1 - statr(i)+idstat
      end do 
      call getlun(lun_rec,istat)
      if(istat.ne.0)call pc_error('Unable to get unit number for receiver file')
      tmpname=trim(obj%path_out) // '.rec'
      open(lun_rec, file = tmpname, status = 'replace', iostat=istat)
      if(istat.ne.0)call pc_error('Unable to open receiver file') 
      ihdr = 35 
      i2 = 36 
      i3 = 0 
      junk = 'MISC' 
      if(miny.eq.0)then
        ihdr = 47
        i2 = 0
      end if
      write(lun_rec, 203) junk, ihdr, i2, i3, i3 
      fhdr = minx 
      shdr = miny 
      finc = 1 
      sinc = 1 
      ifnum = numx 
      isnum = numy 
      write(lun_rec, 204) fhdr, shdr, finc, sinc, ifnum, isnum 
      write(lun_rec, 205) 
      junk4 = '+++END+++' 
      write(lun_rec, 206) junk4 
      nsht2 = numx * numy 
      nn = int((nsht2 + 4.1) / 5) 
      do i = 1, nn 
         ihdrval =((i - 1) * 5) + 1 
         do j = 1, 5 
            if(statr1(ihdrval + j - 1) .eq.0) statr1(ihdrval + j - 1)= - .1e-29
         end do 
         write(lun_rec, 207) ihdrval, statr1(ihdrval), statr1(ihdrval + 1),&
         statr1(ihdrval + 2), statr1(ihdrval + 3), statr1(ihdrval +  4)
      end do 
      close(lun_rec) 
      call mem_free(stato)
      call mem_free(state)
      call mem_free(stats1)
      call mem_free(statr1)

      end subroutine mfrs_refstat                                          


!!------------------------------- traps ------------------------------------!!
!!------------------------------- traps ------------------------------------!!
!!------------------------------- traps ------------------------------------!!


      subroutine mfrs_path_pick_trap (keyword)
      implicit none
      character(len=*),intent(in) :: keyword
      if(object%path_pick.eq.PATHCHECK_EMPTY)then
        call pc_error('PATH_PICK cannot be blank')
      endif
      call pathcheck('path_pick',object%path_pick,required=.true., &
                      show=PATHCHECK_INFO_INPUT)
      return
      end subroutine mfrs_path_pick_trap

      subroutine mfrs_path_static_trap (keyword)
      implicit none
      character(len=*),intent(in) :: keyword
      if(object%bulk.eq.0)return
        
      if(object%path_static.eq.PATHCHECK_EMPTY.and.object%bulk.ne.0)then
        call pc_error('PATH_STATIC cannot be blank if BULK is not zero')
      endif
      call pathcheck('path_static',object%path_static,required=.true., &
                      show=PATHCHECK_INFO_INPUT)
      return
      end subroutine mfrs_path_static_trap

      subroutine mfrs_meth_off_trap (keyword)
      implicit none
      character(len=*),intent(in) :: keyword
      if(object%meth_off.eq.'SPECIFY')then
        call pc_put_sensitive_field_flag('OFF_INIT',.true.)
        call pc_put_sensitive_field_flag('OFF_LAST',.true.)
        call pc_put_sensitive_field_flag('OFF_TOT',.true.)
      else if(object%meth_off.eq.'FIND')then
        call pc_put_sensitive_field_flag('OFF_INIT',.false.)
        call pc_put_sensitive_field_flag('OFF_LAST',.false.)
        call pc_put_sensitive_field_flag('OFF_TOT',.false.)
        object%off_init=-1
        object%off_last=0
        object%off_tot=0
      else
        call pc_error('METH_OFF must be SPECIFY or FIND')
      endif
      return
      end subroutine mfrs_meth_off_trap

      subroutine mfrs_mode_trap (keyword)
      implicit none
      character(len=*),intent(in) :: keyword
      if(object%mode.eq.'ALL'.or.object%mode.eq.'ITER'.or.object%mode.eq.&
         'STATIC')return
      call pc_error('MODE must be ALL, ITER, or STATIC')
      return
      end subroutine mfrs_mode_trap

      subroutine mfrs_opt_iter_trap (keyword)
      implicit none
      character(len=*),intent(in) :: keyword
      if(object%opt_iter.eq.'YES')then
        call pc_put_sensitive_field_flag('NUM_ITER',.false.)
        object%num_iter_used=1000
      else
        call pc_put_sensitive_field_flag('NUM_ITER',.true.)
        object%num_iter_used=object%num_iter
      endif
      return
      end subroutine mfrs_opt_iter_trap

      subroutine mfrs_path_out_trap (keyword)
      implicit none
      character(len=*),intent(in) :: keyword
      character(len=filename_length) :: tmpname
      logical :: there
      if(object%path_out.eq.PATHCHECK_EMPTY)then
        call pc_error('PATH_OUT cannot be blank')
      endif
      call pathcheck('path_out',object%path_out)

      tmpname=trim(object%path_out) // '.diag.outp'
      inquire(file=tmpname,exist=there)
      if(there)then
        call pc_warning('.diag.outp file already exists.  It will be replaced')
      endif

      if(object%mode.ne.'STATIC')then
        tmpname=trim(object%path_out) // '.diag.src'
        inquire(file=tmpname,exist=there)
        if(there)then
          call pc_warning('.diag.src file already exists.  It will be replaced')
        endif
        tmpname=trim(object%path_out) // '.diag.rec'
        inquire(file=tmpname,exist=there)
        if(there)then
          call pc_warning('.diag.rec file already exists.  It will be replaced')
        endif
        tmpname=trim(object%path_out) // '.diag.off'
        inquire(file=tmpname,exist=there)
        if(there)then
          call pc_warning('.diag.off file already exists.  It will be replaced')
        endif

        tmpname=trim(object%path_out) // '.diag.elev'
        inquire(file=tmpname,exist=there)
        if(there)then
          call pc_warning('.diag.elev file already exists.  It will be &
                          &replaced')
        endif

        tmpname=trim(object%path_out) // '.diag.ref'
        inquire(file=tmpname,exist=there)
        if(there)then
          call pc_warning('.diag.ref file already exists.  It will be replaced')
        endif
      endif

      if(object%mode.ne.'ITER')then
        tmpname=trim(object%path_out) // '.off'
        inquire(file=tmpname,exist=there)
        if(there)then
          call pc_warning('.off file already exists.  It will be replaced')
        endif
        tmpname=trim(object%path_out) // '.elev'
        inquire(file=tmpname,exist=there)
        if(there)then
          call pc_warning('.elev file already exists.  It will be replaced')
        endif
        tmpname=trim(object%path_out) // '.src'
        inquire(file=tmpname,exist=there)
        if(there)then
          call pc_warning('.src file already exists.  It will be replaced')
        endif
        tmpname=trim(object%path_out) // '.rec'
        inquire(file=tmpname,exist=there)
        if(there)then
          call pc_warning('.rec file already exists.  It will be replaced')
        endif

      endif

      return
      end subroutine mfrs_path_out_trap

      subroutine mfrs_num_iter_trap (keyword)
      implicit none
      character(len=*),intent(in) :: keyword
      if(object%num_iter.le.0)then
        call pc_error('NUM_ITER must be greater than zero')
      endif
      object%num_iter_used=object%num_iter
      return
      end subroutine mfrs_num_iter_trap

      subroutine mfrs_bulk_trap (keyword)
      implicit none
      character(len=*),intent(in) :: keyword
      if(object%bulk.lt.0)then
        call pc_error('BULK must be greater than zero.')
        call pc_error('Negative is assumed')
      endif

      return
      end subroutine mfrs_bulk_trap

      subroutine mfrs_num_outly_trap (keyword)
      implicit none
      character(len=*),intent(in) :: keyword
      if(object%num_outly.le.0)then
        call pc_error('NUM_OUTLY must be greater than zero')
      endif
      return
      end subroutine mfrs_num_outly_trap

      subroutine mfrs_off_init_trap (keyword)
      implicit none
      character(len=*),intent(in) :: keyword
      return
      end subroutine mfrs_off_init_trap

      subroutine mfrs_off_inc_trap (keyword)
      implicit none
      character(len=*),intent(in) :: keyword
      if(object%off_inc.lt.0.00000001)then
        call pc_error('OFF_INC must be greater than zero')
      endif
      if(object%meth_off.eq.'SPECIFY')then
        object%off_tot=mth_bin_number(real(object%off_init),&
                                      real(object%off_inc),&
                                      val=real(object%off_last))
!rev4        object%off_tot=(object%off_last-object%off_init+1.0)/object%off_inc
      endif
      return
      end subroutine mfrs_off_inc_trap

      subroutine mfrs_off_last_trap (keyword)
      implicit none
      character(len=*),intent(in) :: keyword
      if(object%off_last.le.object%off_init)then
        call pc_error('OFF_LAST must be greater than OFF_INIT')
      endif
      return
      end subroutine mfrs_off_last_trap

      subroutine mfrs_off_tot_trap (keyword)
      implicit none
      character(len=*),intent(in) :: keyword
      if(object%meth_off.eq.'SPECIFY')then
        if(object%off_tot.le.0.)then
          call pc_error('OFF_TOT must be greater than zero')
        endif
        object%off_inc=mth_bin_increment(real(object%off_init),&
                                         real(object%off_last),&
                                         object%off_tot)
!rev4        object%off_inc=(object%off_last-object%off_init+1)/object%off_tot
      endif
      return
      end subroutine mfrs_off_tot_trap

      subroutine mfrs_datum_trap (keyword)
      implicit none
      character(len=*),intent(in) :: keyword
      return
      end subroutine mfrs_datum_trap

      subroutine mfrs_vel_corr_trap (keyword)
      implicit none
      character(len=*),intent(in) :: keyword
      if(object%vel_corr.le.0)then
        call pc_error('VEL_CORR must be greater than zero')
      endif
      return
      end subroutine mfrs_vel_corr_trap

      subroutine mfrs_end_trap

      integer :: icheck

      if(object%meth_off.ne.'FIND')then
        icheck=mth_bin_number(real(object%off_init),real(object%off_inc),&
                              val=real(object%off_last))
!rev4        icheck=(object%off_last-object%off_init+1.0)/object%off_inc
        if(icheck.ne.object%off_tot)then
          call pc_error('OFF_TOT must equal nint ((OFF_LAST-OFF_INIT) / &
                        &OFF_INC + 1')
!rev4          call pc_error('OFF_TOT must equal (OFF_LAST-OFF_INIT+1)/OFF_INC')
        endif
      endif
      end subroutine mfrs_end_trap





!!! Place code for parameter traps here.
!!! You should call PC_ERROR   to register an error message.
!!! You should call PC_WARNING to register a warning message.
!!! You should call PC_INFO    to register an informational message.
!!! You can call a PC_JUMP routine to force a GUI to jump to a different
!!!   field or remain in the current field.
!!!
!!! The argument lists are simpler than in the old CPS system because
!!! subroutine calls are provided to put information into the parameter cache.
!!! For example, subroutine calls to the parameter cache are available
!!! to set values which force the GUI to a specific field or another
!!! screen (see below).
!!!
!!! Note that what we previously called an array trap is now called an
!!! arrayset trap if it corresponds to a set of linked arrays instead of
!!! a single array.
!!!
!!!                           +++++++++++++++++++
!!!
!!!   subroutine mfrs_mode_trap (keyword)              ! scalar trap.
!!!   implicit none
!!!   character(len=*),intent(in) :: keyword           ! arguments
!!!
!!!   if (object%mode /= 'GENERATE' .and. object%mode /= 'MODIFY') then
!!!        call pc_error      ('MODE must be GENERATE or MODIFY.')
!!!        call pc_jump_field (keyword)
!!!   end if
!!!   return
!!!   end subroutine mfrs_mode_trap
!!!
!!!
!!!   subroutine mfrs_array1_trap (keyword)            ! array trap.
!!!   implicit none
!!!   character(len=*),intent(in) :: keyword           ! arguments
!!!   integer                     :: j                 ! local
!!!
!!!   if (object%mode.eq.'MODIFY'.and.object%nfold.eq.0) then
!!!        call pc_error('There must be at least one ARRAY1 value.')
!!!   end if
!!!   do j = 1,object%nfold
!!!        if (object%array1(j).le.0.0) then
!!!            call pc_error ('All ARRAY1 elements must be > zero.')
!!!        end if
!!!   end do
!!!   return
!!!   end subroutine mfrs_array1_trap
!!!
!!!
!!!   subroutine mfrs_array1_element_trap (keyword,indx,action)
!!!                                                    ! array element trap.
!!!   implicit none
!!!   character(len=*),intent(in) :: keyword           ! arguments
!!!   integer         ,intent(in) :: indx              ! arguments
!!!   integer         ,intent(in) :: action            ! arguments
!!!    ! INDX is a Fortran-style index (=1 for the first array element).
!!!    ! ACTION is PC_INSERT or PC_REMOVE or PC_MODIFY.
!!!    ! INDX refers to the array element inserted or removed or modified.
!!!
!!!   if (object%mode.eq.'MODIFY'.and.object%array1(indx).le.0.0) then
!!!        call pc_error ('All ARRAY1 elements must be > zero.')
!!!   end if
!!!   return
!!!   end subroutine mfrs_array1_element_trap
!!!
!!!
!!!   subroutine mfrs_abc_arrayset_trap (keyword)      ! arrayset trap.
!!!   implicit none
!!!   character(len=*),intent(in) :: keyword           ! arguments
!!!
!!!   !!! for illustration only - no code here.
!!!   return
!!!   end subroutine mfrs_abc_arrayset_trap
!!!
!!!
!!!   subroutine mfrs_screen1_trap (keyword)           ! screen trap.
!!!   implicit none
!!!   character(len=*),intent(in) :: keyword           ! arguments
!!!
!!!   !!! for illustration only - no code here.
!!!   return
!!!   end subroutine mfrs_screen1_trap
!!!
!!!
!!!   subroutine mfrs_ngathers_trap (keyword)          ! scalar trap.
!!!   implicit none
!!!   character(len=*),intent(in) :: keyword           ! arguments
!!!
!!!   if (object%mode.eq.'MODIFY') then
!!!   if (object%ngathers.eq.2*(object%ngathers/2).or.    &
!!!       object%ngathers.le.0) then
!!!        call pc_error ('NGATHERS must be > zero and odd.')
!!!   end if
!!!   end if
!!!   return
!!!   end subroutine mfrs_ngathers_trap
!!!
!!!
!!!   subroutine mfrs_end_trap                         ! end trap.
!!!   implicit none
!!!
!!!   !!! for illustration only - no code here.
!!!   return
!!!   end subroutine mfrs_end_trap
!!!
!!!                           +++++++++++++++++++
!!!
!!! Traps are called at these times:
!!!
!!!   Pushbutton traps:
!!!     (1) Whenever the pushbutton is pressed in a GUI.
!!!     (2) Never called at any other time.
!!!
!!!   Scalar traps:
!!!     (1) Whenever the scalar value is successfully modified in a GUI.
!!!     (2) Always called when this is not a GUI update.
!!!
!!!   Array element traps:
!!!     (1) Whenever the array is successfully modified in a GUI
!!!          by inserting, removing, or changing a single array element.
!!!     (2) Never called at any other time.
!!!     (3) At a minimum, the specified array element should be verified in
!!!          this trap.
!!!
!!!   Array traps:
!!!     (1) When leaving the array field in the GUI, as long as the array is
!!!          not part of a set of linked arrays.
!!!     (2) An arrayset trap should be used instead of an array trap for an 
!!!          array which is part of a linked array set.
!!!     (3) Always called when this is not a GUI update.
!!!     (4) It is guaranteed that an array trap will always be eventually
!!!          called after one or more calls to the corresponding array
!!!          element trap.
!!!     (5) The entire array should be verified in this trap.
!!!
!!!   Arrayset traps:
!!!     (1) When leaving the arrayset field (set of linked arrays) in the GUI.
!!!     (2) An array trap should be used instead of an arrayset trap for a 
!!!          single array which is not part of a linked array set. 
!!!     (3) Always called when this is not a GUI update.
!!!     (4) It is guaranteed that an arrayset trap will always be eventually
!!!          called after one or more calls to the corresponding array
!!!          element traps.
!!!     (5) The entire set of linked arrays should be verified in this trap.
!!!     (6) The keyword for a set of linked arrays is the keyword for the first
!!!          array in the set (leftmost in the GUI), with the suffix "_arrayset"
!!!          appended.
!!!
!!!   Screen traps:
!!!     (1) When leaving the screen in the GUI.
!!!     (2) Always called when this is not a GUI update.
!!!     (3) Any variables on this screen, or interactions between such
!!!          variables, which have not been verified in the preceding traps
!!!          should be verified here.
!!!     (4) The keyword for a screen is the word "screen1" for the first
!!!          screen in the GUI for this process, with the digit incremented
!!!          by 1 for each additional screen.
!!!
!!!   End traps:
!!!     (1) Never called when this is a GUI update.
!!!     (2) Always called when this is not a GUI update.
!!!     (3) An end trap can be used for general or time-consuming verifications
!!!          which cannot be performed in other traps but need to be performed
!!!          before process parameters are saved (e.g. by a front-end) or used
!!!          to process traces (e.g. on the back-end).
!!!
!!! GUI updates can generate a call to any trap except an end trap.
!!!
!!! Frontend and backend updates always generate a call to all types of traps
!!! except never to array element traps and pushbutton traps.
!!!
!!! In the original CPS system, traps (except the end trap) were only called
!!! when the user changed a value, or the user left an array field or a screen.
!!! This meant that the trap code often had to be duplicated in the end trap
!!! routine on the frontend.  And also of course, in the original CPS system,
!!! this code also had to be duplicated (again) on the backend.  The current
!!! method of determining when traps are called is designed to avoid all such
!!! duplication requirements in the new system.
!!!
!!!                           +++++++++++++++++++
!!!
!!! Description of update states:
!!!
!!! GUI updates:
!!!   GUI updates occur whenever a single scalar parameter or a single array
!!!   element is modified interactively in a GUI, or when some other GUI action
!!!   occurs such as pressing a button or leaving a screen.  This can happen
!!!   repeatedly in the frontend of a processing system, or from any other
!!!   program which does multiple updates.  A sequence of GUI updates must
!!!   always be eventually followed by a frontend (or possibly a backend)
!!!   update.
!!!
!!! Frontend updates:
!!!   Frontend updates are all non-GUI updates in the interactive frontend of
!!!   a processing system, or in any other program which does multiple updates
!!!   and does not immediately proceed to process traces.  Frontend updates
!!!   may occur with or without any changes to one or more (or all) parameters.
!!!   For example, a frontend update might occur whenever a workfile is created
!!!   or updated, or when a process module is inserted into the job flow, or
!!!   when a dialog box for a process module is popped down.
!!!
!!! Backend updates:
!!!   A backend update must be the last (or only) update which occurs just
!!!   before trace processing starts.  Therefore a single backend update is
!!!   normally the only update in a batch processing job or in a program outside
!!!   of a processing system, but may follow several GUI and/or frontend updates
!!!   in an interactive processing system or any other interactive program.
!!!
!!!                           +++++++++++++++++++
!!!
!!! If you are converting this process module from the old CPS system,
!!! you might simply be able to copy the corresponding traps from your old
!!! front-end code, and then make the following adjustments:
!!!
!!!    Make each trap a separate subroutine if entry statements were used.
!!!    Change to the simpler argument lists as shown above.
!!!    Replace CRT_MESS calls by calls to PC_ERROR or PC_WARNING.
!!!    Replace CRT_LINE calls by calls to PC_INFO.
!!!    Replace TURN_ON_PROMPT calls by calls to PC_PUT_SENSITIVE_FLAG.
!!!    Replace TURN_OFF_PROMPT calls by calls to PC_PUT_SENSITIVE_FLAG.
!!!    Replace setting of array switch variables by PC_PUT_SENSITIVE_FLAG.
!!!    Call one of these subroutines to jump to the specified location:
!!!       PC_JUMP_SCREEN        (keyword)        ! specified screen.
!!!       PC_JUMP_FIELD         (keyword)        ! specified scalar field.
!!!       PC_JUMP_ARRAYSET      (keyword)        ! specified linked array set.
!!!       PC_JUMP_ARRAYSET_ROW  (keyword,indx)   ! specified row (index).
!!!       PC_JUMP_ARRAY_ELEMENT (keyword,indx)   ! specified array element.
!!!       PC_JUMP_ARRAY         (keyword)        ! specified array.
!!!    INDX is a Fortran-style index (=1 for the first array element).
!!!
!!! Note:
!!!
!!!    The field jumped to by PC_JUMP... may be on same or different screen.
!!!    PC_JUMP... can specify a current location to stay in the current field
!!!      or array or linked array set or screen (like previously setting
!!!      IEXSW to 1 or 3, or setting other old trap arguments, depending on
!!!      the type of trap or type of field to stay in).
!!!    If PC_JUMP... causes an exit from an array or a linked array set or
!!!      a screen, the appropriate array or arrayset or screen trap(s)
!!!      will subsequently be called.
!!!    It is no longer necessary to tell the GUI to repaint the screen.
!!!
!!! You must change any trap code which makes any assumption about the order
!!! in which parameters are changed by the person entering values into the
!!! GUI.  You must assume that any trap might be called at any time.


!!--------------------------- main execution -------------------------------!!
!!--------------------------- main execution -------------------------------!!
!!--------------------------- main execution -------------------------------!!


!!! Upon input, NTR will have one of these values:
!!!   NTR >= 1              means to process the input traces.
!!!   NTR == NO_MORE_TRACES means there are no more imput traces.
!!!   NTR == NEED_TRACES    means someone from below needs more traces.
!!!   NTR == NEED_TRACES    might mean this is a trace-supplying process.
!!!   NTR == NEED_TRACES    will not occur unless this process has a label.
!!!
!!! Upon output, NTR must have one of these values:
!!!   NTR >= 1              if you are outputting traces.
!!!   NTR == NO_MORE_TRACES if there are no more traces to output.
!!!   NTR == FATAL_ERROR    if you have a fatal error.
!!!   NTR == NEED_TRACES    if you need another trace before passing any out.
!!!   NTR == NEED_TRACES    must not occur unless you specified that you
!!!                           might need to request more traces.

!!------------------------------- wrapup -----------------------------------!!
!!------------------------------- wrapup -----------------------------------!!
!!------------------------------- wrapup -----------------------------------!!


!<execute_only>

      subroutine mfrs_wrapup (obj)
      implicit none
      type(mfrs_struct),intent(inout) :: obj       ! arguments

      if (obj%skip_wrapup) return
      obj%skip_wrapup = .true.

!!!
!!!  --> Put any required wrapup code here, including wrapups of
!!!  --> internally-called processes.
!!!
!!!   if (associated(obj%test1)) call test1_wrapup (obj%test1)
!!!

      return
      end subroutine mfrs_wrapup

!</execute_only>


!!! All processes must wrap up appropriately after processing traces.
!!! Some processes might not have anything to do.  Other processes might
!!! want to save files or print summary information.  Appropriate wrapup
!!! procedures should be taken when trace processing has terminated
!!! successfully or unsuccessfully.
!!!
!!! The code in this subroutine must function correctly even if trace
!!! processing never began.  In spite of the <execute> flags in this
!!! template, there is no guarantee that this wrapup routine will not
!!! get called from the front end.
!!!
!!! Even if this routine is empty, this routine should be called from the
!!! main trace processing routine before returning NTR == NO_MORE_TRACES
!!! or NTR == FATAL_ERROR, and from the DELETE routine.
!!!
!!! This routine is called from the back-end after some process (including
!!! possibly this one) has reported an error before all traces have been
!!! processed.
!!!
!!! This routine should be called from any other calling program (e.g.
!!! another process module or an interactive program) if processing is
!!! prematurely terminated for any reason.
!!!
!!! This routine is also called from the DELETE routine to make sure the
!!! wrapup is completed.  Therefore, if this object is to be deleted right
!!! after the wrapup is completed, the call to the WRAPUP routine is not
!!! necessary.
!!!
!!! By testing the SKIP_WRAPUP structure variable, this wrapup routine is
!!! able to handle the following situations:
!!!
!!!  (1) This routine does not repeat any wrapup previously done in case
!!!      it is called more than once.
!!!
!!!  (2) This routine does not do any wrapup if this process never prepared
!!!      to process traces in the first place.
!!!
!!!
!!----------------------------- end of module ------------------------------!!
!!----------------------------- end of module ------------------------------!!
!!----------------------------- end of module ------------------------------!!


      end module mfrs_module


!!--------------------------------- end ------------------------------------!!
!!--------------------------------- end ------------------------------------!!
!!--------------------------------- end ------------------------------------!!
