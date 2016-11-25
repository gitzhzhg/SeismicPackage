!<CPS_v1 type="PRIMITIVE"/>
!!------------------------------ conjgrad.f90 ------------------------------!!
!!------------------------------ conjgrad.f90 ------------------------------!!
!!------------------------------ conjgrad.f90 ------------------------------!!

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
! Name       : CONJGRAD   (Conjugate Gradients)
! Category   : math
! Written    : 2000-01-26  by: Bob Baumel (based on code by Bill Harlan)
! Revised    : 2000-06-02  by: Bob Baumel
! Maturity   : production  2000-06-14
! Purpose    : Solve linear equations by conjugate gradient method.
! Portability: No known limitations - but note: current code is single
!              precision only.
!
!-------------------------------------------------------------------------------
!</brief_doc>

!<descript_doc>
!-------------------------------------------------------------------------------
!                         GENERAL DESCRIPTION
!
! The term "conjugate gradients" refers to a class of iterative methods for
! solving various linear and nonlinear sets of equations. The conjugate
! gradient method implemented in this module is for solving linear systems
! of the form  Y = A X  where A is a known matrix, Y is a known vector, and
! we wish to solve for the vector X. The matrix A need not be square, and
! the algorithm attempts to find a least squares solution.
!
! In the case where A is a square N x N matrix, the method theoretically
! converges to the exact answer after N iterations (given exact arithmetic).
! But that would not be efficient (probably slower than other general purpose
! linear equation solvers), and the required numerical precision normally
! doesn't exist. The power of this method is that an adequate approximation
! is often obtained after only a few iterations (for example, 3 to 5).
!
! This module consists of overloaded subroutines to cover the 4 cases where
! X is real or complex, and Y is real or complex. However, it is currently
! limited to single precision arithmetic.
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
!                                                opt   opt    opt
!                         i  o   i  i  i    i     i     i      i
!        CALL  CONJGRAD (NX, X, NY, Y, A, NTER, PRWH, CONV, XSTART)
!
! integer            NX         = Number of values in X vector.
! real or complex**  X(NX)      = Vector to solve for in equation Y = A*X.
! integer            NY         = Number of values in Y vector.
! real or complex**  Y(NY)      = Known vector in equation Y = A*X.
! real or complex**  A(NY,NX)   = Known matrix in equation Y = A*X.
! integer            NTER       = Number of conjugate gradient iterations.
! real > 0           PRWH       = pre-whitening (diagonal load) factor
!                                 (If omitted, default is PRWH = 0.0001).
! real > 0           CONV       = Convergence test value to stop iterations
!                                 (If omitted, default is CONV = 0.000001).
! real or complex**  XSTART(NX) = Starting guess for solution vector X
!                                 (If omitted, inital guess is X = 0.0).
!
! **This module consists of overloaded subroutines to cover the four cases
!   where X is real or complex, and Y is real or complex. Matrix A must be
!   real when X and Y are both real, and must be complex when either X or
!   Y is complex. The starting guess XSTART, if specified, must be of the
!   same type as X.
!
!-------------------------------------------------------------------------------
!</calling_doc>

!<advice_doc>
!-------------------------------------------------------------------------------
!                            ADVICE FOR USERS
!
! Since X and Y can each be either real or complex, there are 4 cases:
! 1) X real and Y real. A must also be real. You are finding a least squares
!    solution of a totally real system  Y = A X.
! 2) X complex and Y complex. A must also be complex. You are finding a
!    least squares solution of a totally complex system  Y = A X.
! 3) X real and Y complex. A must be complex. You are finding the real
!    vector X such that (A X) is as close as possible to complex vector Y.
! 4) X complex and Y real. A must be complex. This is really just a special
!    instance of case (2) but is provided for completeness so that all 4
!    possibilities of X and Y real and complex are supported. Here, you are
!    solving the fully complex equations for the special case where complex
!    vector Y happens to be real. Note: this is NOT equivalent to solving the
!    equation Y = Re(A X) which would have a different least squares solution.
!
! Notes on PRWH and CONV arguments: Usually, you can omit these arguments, as
! the defaults should be adequate for most cases. For more info on PRWH, see
! algorithm description below.  CONV specifies a convergence criterion for
! exiting the loop before completing the specified number (NTER) of iterations.
! The routine assumes convergence when the magnitude of the current gradient
! falls below its initial value (from first iteration) by a factor of CONV.
!
!-------------------------------------------------------------------------------
!</advice_doc>

!<history_doc>
!-------------------------------------------------------------------------------
!                           REVISION HISTORY
!
!     Date        Author       Description
!     ----        ------       -----------
!  2. 2000-06-14  Bob Baumel   Added optional XSTART argument.
!  1. 2000-01-26  Bob Baumel   Initial version - based on Harlan's conjugate
!                              gradient code in RMUL but many modifications.
!
!-------------------------------------------------------------------------------
!</history_doc>

!<portability_doc>
!-------------------------------------------------------------------------------
!                         PORTABILITY LIMITATIONS
!
! No known limitations; however, current code is single precision only. Thus,
! it is possible that some applications would work on a 64-bit machine and not
! on a 32-bit machine. If it is desired to do this calculation in double
! precision, it would be straightforward to overload another routine for the
! PURELY REAL case. However, it is less obvious how to extend the options
! involving complex arithmetic to double precision in a platform-independent
! manner.
!
!-------------------------------------------------------------------------------
!</portability_doc>

!<compile_doc>
!-------------------------------------------------------------------------------
!                      SPECIAL COMPILING REQUIREMENTS
!
! No special requirements.
!
!-------------------------------------------------------------------------------
!</compile_doc>

!<algorithm_doc>
!-------------------------------------------------------------------------------
!                   ALGORITHM DESCRIPTION FOR DEVELOPERS
!
! The module attempts to minimize the functional
!                   ||Y - (A X)||**2  +  EPS ||X||**2
! which is equivalent to solving the equation
!                    (A* A  +  EPS) X   =   A* Y
! where A* denotes the hermitian adjoint of A; and thus, (A* A) denotes the
! square, non-negative matrix obtained by pre-multiplying A by its adjoint.
! Since A may have a null space, the diagonal load term EPS is, in general,
! necessary to ensure a unique solution. The value of EPS is derived from the
! user-specified PRWH by scaling relative to the A matrix; in particular:
!               EPS = PRWH * Average_diagonal_term(A* A).
!
! The iterative algorithm determines the gradient at a possible solution X:
!                     GRAD  =  (A* RESID)  -  (EPS X)
! where RESID is the current residual:  RESID = Y - (A X)  [Actually, the
! GRAD given by this equation points in the steepest descent direction, so
! is negative of the vector usually called the gradient].  A current step
! vector (STEP) is determined as a linear combination of the gradient (GRAD)
! and the previous step vector (STEP_PREV):
!                 STEP  =  GRAD  +  (BETA STEP_PREV) ;
! then, the solution vector X is updated as
!                      X  =  X  +  (ALPHA STEP) .
! Coefficients ALPHA and BETA are determined as the optimal values to minimize
! the desired functional. In the cases where vector X is real [cases (1) and
! (3) in above Advice for Users section], the update is optimal for all REAL
! values of ALPHA and BETA. In cases (2) and (4), all COMPLEX values are
! considered; however, the optimal ALPHA and BETA turn out to be real and
! positive, given by formulas which are easily determined from the code below.
!
! The first iteration is always simple gradient descent; i.e., the first STEP
! vector is equal to the initial GRAD.
!
!-------------------------------------------------------------------------------
!</algorithm_doc>

!<programming_doc>
!-------------------------------------------------------------------------------
!                           PROGRAMMING NOTES
!
! This module consists of 4 overloaded subroutines; however, the subroutine
! for case (4) [see above Advice for Users] is trivial, as it simply calls
! the subroutine for case (2); this approach requires slightly more temporary
! memory but avoids duplicating all the code for case (2).
!
! This implementation of conjugate gradients is somewhat specialized, as it
! requires X and Y to be rank 1 vectors and requires A to be passed explicitly
! as a matrix in the argument list. In a more object-oriented setting, it
! is possible to write conjugate gradient routines at higher levels of
! abstraction, as in the "Hilbert Class Library" from Rice University which
! is written in C++ (see http://www.trip.caam.rice.edu/txt/hcldoc/html/ ),
! where the "vectors" may be more general objects (which needn't even reside
! in memory) and, instead of specifying A explicitly as a matrix, one need
! only have routines for calculating the action of A and its adjoint on a
! vector. Actually, Harlan's original conjugate gradient code was slightly
! more "object-oriented" in that it didn't require A to be passed explicitly
! as a matrix; however, it was very awkward and difficult to use. Hopefully,
! this current implementation will be adequate for applications of conjugate
! gradients in CPS. If necessary, more general versions can be provided.
!
! This algorithm ought to be reasonably robust numerically, as long as the
! number of iterations NTER is reasonably small (e.g., no greater than 10).
! Greater numerical stability could be achieved at the cost of adding an
! extra MATMUL call to each iteration (which would increase execution time
! around 50%), specifically, by recalculating the residual explicitly as
! Y - (A X)  every time through the loop before calculating the gradient.
! The current code updates RESID iteratively by addition, which may cause
! increasing discrepancy between RESID and the current X vector after many
! iterations.
!
! This module calls only Fortran intrinsics and uses no other CPS modules.
!
!-------------------------------------------------------------------------------
!</programming_doc>

!!-------------------------- start of module ------------------------------!!
!!-------------------------- start of module ------------------------------!!
!!-------------------------- start of module ------------------------------!!

      module conjgrad_module
      implicit none

      private
      public :: conjgrad

      character(len=100),public,save :: CONJGRAD_IDENT = &
       '$Id: conjgrad.f90,v 1.2 2000/06/13 13:35:40 sps prod sps $'

!!---------------------------- interfaces ---------------------------------!!
!!---------------------------- interfaces ---------------------------------!!
!!---------------------------- interfaces ---------------------------------!!

      interface conjgrad
        module procedure conjgrad_real
        module procedure conjgrad_complex
        module procedure conjgrad_real2complex
        module procedure conjgrad_complex2real
      end interface

!!------------------------------ data -------------------------------------!!
!!------------------------------ data -------------------------------------!!
!!------------------------------ data -------------------------------------!!

      real, parameter  :: prwh_default = 0.0001
      real, parameter  :: conv_default = 0.000001

      contains

!!----------------------------- subroutines -------------------------------!!
!!----------------------------- subroutines -------------------------------!!
!!----------------------------- subroutines -------------------------------!!

!!---------------------------- conjgrad_real ------------------------------!!
!!---------------------------- conjgrad_real ------------------------------!!
!!---------------------------- conjgrad_real ------------------------------!!

   subroutine conjgrad_real (nx, x, ny, y, a, nter, prwh, conv, xstart)
!
     implicit none
     integer,intent(in)          :: nx
     real   ,intent(out)         :: x (nx)
     integer,intent(in)          :: ny
     real   ,intent(in)          :: y (ny)
     real   ,intent(in)          :: a (ny,nx)
     integer,intent(in)          :: nter
     real   ,intent(in),optional :: prwh, conv
     real   ,intent(in),optional :: xstart (nx)
!
     real    :: resid (ny)
     real    :: grad (nx)
     real    :: step (nx)
     real    :: a_step (ny)
     integer :: iter, ix, iy
     real    :: eps, ggdot, ggtest, ggprev, ssdot, asasdot, alpha, beta
!
     if (present(xstart)) then
       x     = xstart
       resid = y - matmul(a,xstart)
     else
       x     = 0.0
       resid = y
     end if
!
     eps = 0.
     do ix = 1, nx
       eps = eps + dot_product (a(:,ix), a(:,ix))
     end do
     if (eps <= 0.) return
     if (present(prwh)) then
       eps = (eps * prwh) / nx
     else
       eps = (eps * prwh_default) / nx
     end if
!
     do iter = 1, nter
!
       grad = (-eps) * x
       DO IY = 1, NY              ! heavy computation--matrix multiply.
         GRAD = A(IY,:) * RESID(IY)  +  GRAD
       END DO
       ggdot = dot_product (grad, grad)
!
       if (iter == 1) then
         if (ggdot <= 0.) exit
         if (present(conv)) then
           ggtest = ggdot * conv**2
         else
           ggtest = ggdot * conv_default**2
         end if
         step = grad
         ssdot = ggdot
       else
         if (ggdot <= ggtest) exit
         beta = ggdot / ggprev
         step = grad + beta*step
         ssdot = dot_product (step, step)
         if (ssdot <= 0.) exit
       end if
!
       A_STEP = MATMUL (A, STEP)  ! heavy computation--matrix multiply.
!
       asasdot  = dot_product (a_step, a_step)
       alpha = ggdot / (asasdot + eps*ssdot)
       x     = x     + alpha*step
       resid = resid - alpha*a_step
!
       ggprev = ggdot
!
     end do
!
   end subroutine conjgrad_real

!!--------------------------- conjgrad_complex ----------------------------!!
!!--------------------------- conjgrad_complex ----------------------------!!
!!--------------------------- conjgrad_complex ----------------------------!!

   subroutine conjgrad_complex (nx, x, ny, y, a, nter, prwh, conv, xstart)
!
     implicit none
     integer,intent(in)          :: nx
     complex,intent(out)         :: x (nx)
     integer,intent(in)          :: ny
     complex,intent(in)          :: y (ny)
     complex,intent(in)          :: a (ny,nx)
     integer,intent(in)          :: nter
     real   ,intent(in),optional :: prwh, conv
     complex,intent(in),optional :: xstart (nx)
!
     complex :: resid (ny)
     complex :: grad (nx)
     complex :: step (nx)
     complex :: a_step (ny)
     integer :: iter, ix, iy
     real    :: eps, ggdot, ggtest, ggprev, ssdot, asasdot, alpha, beta
!
     if (present(xstart)) then
       x     = xstart
       resid = y - matmul(a,xstart)
     else
       x     = (0.0, 0.0)
       resid = y
     end if
!
     eps = 0.
     do ix = 1, nx
       eps = eps + real(dot_product (a(:,ix), a(:,ix)))
     end do
     if (eps <= 0.) return
     if (present(prwh)) then
       eps = (eps * prwh) / nx
     else
       eps = (eps * prwh_default) / nx
     end if
!
     do iter = 1, nter
!
       grad = (-eps) * x
       DO IY = 1, NY              ! heavy computation--matrix multiply.
         GRAD = CONJG(A(IY,:)) * RESID(IY)  +  GRAD
       END DO
       ggdot = dot_product (grad, grad)
!
       if (iter == 1) then
         if (ggdot <= 0.) exit
         if (present(conv)) then
           ggtest = ggdot * conv**2
         else
           ggtest = ggdot * conv_default**2
         end if
         step = grad
         ssdot = ggdot
       else
         if (ggdot <= ggtest) exit
         beta = ggdot / ggprev
         step = grad + beta*step
         ssdot = dot_product (step, step)
         if (ssdot <= 0.) exit
       end if
!
       A_STEP = MATMUL (A, STEP)  ! heavy computation--matrix multiply.
!
       asasdot  = dot_product (a_step, a_step)
       alpha = ggdot / (asasdot + eps*ssdot)
       x     = x     + alpha*step
       resid = resid - alpha*a_step
!
       ggprev = ggdot
!
     end do
!
   end subroutine conjgrad_complex

!!------------------------- conjgrad_real2complex -------------------------!!
!!------------------------- conjgrad_real2complex -------------------------!!
!!------------------------- conjgrad_real2complex -------------------------!!

   subroutine conjgrad_real2complex (nx, x, ny, y, a, nter, prwh, conv, &
                                     xstart)
!
     implicit none
     integer,intent(in)          :: nx
     real   ,intent(out)         :: x (nx)
     integer,intent(in)          :: ny
     complex,intent(in)          :: y (ny)
     complex,intent(in)          :: a (ny,nx)
     integer,intent(in)          :: nter
     real   ,intent(in),optional :: prwh, conv
     real   ,intent(in),optional :: xstart (nx)
!
     complex :: resid (ny)
     real    :: grad (nx)
     real    :: step (nx)
     complex :: a_step (ny)
     integer :: iter, ix, iy
     real    :: eps, ggdot, ggtest, ggprev, ssdot, asasdot, alpha, beta
!
     if (present(xstart)) then
       x     = xstart
       resid = y - matmul(a,xstart)
     else
       x     = 0.0
       resid = y
     end if
!
     eps = 0.
     do ix = 1, nx
       eps = eps + real(dot_product (a(:,ix), a(:,ix)))
     end do
     if (eps <= 0.) return
     if (present(prwh)) then
       eps = (eps * prwh) / nx
     else
       eps = (eps * prwh_default) / nx
     end if
!
     do iter = 1, nter
!
       grad = (-eps) * x
       DO IY = 1, NY              ! heavy computation--matrix multiply.
         GRAD = REAL (CONJG(A(IY,:)) * RESID(IY))  +  GRAD
       END DO
       ggdot = dot_product (grad, grad)
!
       if (iter == 1) then
         if (ggdot <= 0.) exit
         if (present(conv)) then
           ggtest = ggdot * conv**2
         else
           ggtest = ggdot * conv_default**2
         end if
         step = grad
         ssdot = ggdot
       else
         if (ggdot <= ggtest) exit
         beta = ggdot / ggprev
         step = grad + beta*step
         ssdot = dot_product (step, step)
         if (ssdot <= 0.) exit
       end if
!
       A_STEP = MATMUL (A, STEP)  ! heavy computation--matrix multiply.
!
       asasdot  = dot_product (a_step, a_step)
       alpha = ggdot / (asasdot + eps*ssdot)
       x     = x     + alpha*step
       resid = resid - alpha*a_step
!
       ggprev = ggdot
!
     end do
!
   end subroutine conjgrad_real2complex

!!------------------------- conjgrad_complex2real -------------------------!!
!!------------------------- conjgrad_complex2real -------------------------!!
!!------------------------- conjgrad_complex2real -------------------------!!

   subroutine conjgrad_complex2real (nx, x, ny, y, a, nter, prwh, conv, &
                                     xstart)
!
     implicit none
     integer,intent(in)          :: nx
     complex,intent(out)         :: x (nx)
     integer,intent(in)          :: ny
     real   ,intent(in)          :: y (ny)
     complex,intent(in)          :: a (ny,nx)
     integer,intent(in)          :: nter
     real   ,intent(in),optional :: prwh, conv
     complex,intent(in),optional :: xstart (nx)
!
     complex :: yy (ny)
!
     yy = y
     call conjgrad_complex (nx, x, ny, yy, a, nter, prwh, conv, xstart)
     return
   end subroutine conjgrad_complex2real

!!----------------------------- end of module -----------------------------!!
!!----------------------------- end of module -----------------------------!!
!!----------------------------- end of module -----------------------------!!

      end module conjgrad_module

!!--------------------------------- end -----------------------------------!!
!!--------------------------------- end -----------------------------------!!
!!--------------------------------- end -----------------------------------!!
