!<CPS_v1 type="PRIMITIVE"/>
!!------------------------------- rsvd.f90 --------------------------------!!
!!------------------------------- rsvd.f90 --------------------------------!!
!!------------------------------- rsvd.f90 --------------------------------!!


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
! Name       : RSVD     (Real Single Value Decomposition)
! Category   : math
! Written    : 2001-08-01   by: Tom Stoeckley
! Revised    : 2001-10-29   by: Tom Stoeckley
! Maturity   : production   2001-12-11
! Purpose    : Single value decomposition of a real matrix.
! Portability: No known limitations.
!
!-------------------------------------------------------------------------------
!</brief_doc>


!<descript_doc>
!-------------------------------------------------------------------------------
!                         GENERAL DESCRIPTION            
!
! Singular value decomposition of an M by N real matrix, where M > N.
!
!     P.A. Businger and G.H. Golub, "Singular Value Decomposition
!     of a Complex Matrix," Communications of the ACM, vol. 12,
!     pp. 564-565, October 1969.
!
! This algorithm is reprinted by permission, Association for Computing
! Machinery; copyright 1969.
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
!                                                          opt opt
!                      b   i     i    i  i  i   i   i   o   o   o
!           call rsvd (a, mmax, nmax, m, n, ip, nu, nv, s,  u,  v)
!
! real     A(MMAX,NMAX) = M by N matrix to decompose.
! integer  MMAX         = first or second dimension of matrix or vector.
! integer  NMAX         = first or second dimension of matrix or vector.
! integer  M            = number of rows or columns.
! integer  N            = number of rows or columns.
! integer  IP           = must be set to zero.
! integer  NU           = number of columns of U(M,M) to compute.
! integer  NV           = number of columns of V(N,N) to compute.
! real     S(NMAX)      = vector containing N singular values (returned).
! real     U(MMAX,MMAX) = M by M unitary matrix (optionally returned).
! real     V(NMAX,NMAX) = N by N unitary matrix (optionally returned).
!
!   Singular value decomposition of an M by N real matrix A, where M > N.
!   The singular values are stored in the vector S.
!   The first NU columns of the M by M unitary matrix U and the
!   first NV columns of the N by N unitary matrix V that minimize
!   Det(A-USV*) are also computed.
!
! M must be greater than N.
! IP must be set to zero.
! NU should be set to M if you want U(M,M) to be calculated and returned.
! NV should be set to N if you want V(N,N) to be calculated and returned.
! NU should be set to 0 if you do not need U(M,M) to be returned.
! NV should be set to 0 if you do not need V(N,N) to be returned.
! U must be present if NU > 0.
! V must be present if NV > 0.
! The code runs several times faster if U(M,M) and V(N,N) are not calculated.
!
! If RSVD encounters an attempt to divide by zero, it sets all output arrays
! as follows:
!                S(1)   = 1.0
!                S(2:)  = 0.0
!                U(:,:) = 0.0
!                V(:,:) = 0.0
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
!     Date        Author     Description
!     ----        ------     -----------
!  2. 2001-12-11  Stoeckley  Add checks for divide by zero.
!  1. 2001-08-08  Stoeckley  Initial version, moved from EDA; make arguments
!                             U and V optional; change arguments S, U, and V
!                             to intent(out) instead of intent(inout).
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


      module rsvd_module
      implicit none
      public

      character(len=100),public,save :: RSVD_IDENT = &
'$Id: rsvd.f90,v 1.2 2001/12/10 19:55:16 Stoeckley prod sps $'

      contains


!!----------------------------- rsvd --------------------------------------!!
!!----------------------------- rsvd --------------------------------------!!
!!----------------------------- rsvd --------------------------------------!!

      subroutine rsvd (a,mmax,nmax,m,n,ip,nu,nv,s,u,v)
      implicit none
!-----------------------------------------------
!   D u m m y   A r g u m e n t s
!-----------------------------------------------
      INTEGER , INTENT(IN)           :: MMAX
      INTEGER , INTENT(IN)           :: NMAX
      INTEGER , INTENT(IN)           :: M
      INTEGER , INTENT(IN)           :: N
      INTEGER , INTENT(IN)           :: IP
      INTEGER , INTENT(IN)           :: NU
      INTEGER , INTENT(IN)           :: NV
      REAL    , INTENT(INOUT)        :: A(MMAX,NMAX)
      REAL    , INTENT(OUT)          :: S(NMAX)
      REAL    , INTENT(OUT),optional :: U(MMAX,MMAX)
      REAL    , INTENT(OUT),optional :: V(NMAX,NMAX)
!-----------------------------------------------
!   L o c a l   V a r i a b l e s
!-----------------------------------------------
      INTEGER :: NP, N1, K, K1, I, J, KK, LL, L, L1
      REAL :: Q, R
      REAL , DIMENSION(100) :: B, C, T
      REAL :: ETA, TOL, Z, W, EPS, CS, SN, F, H, X, Y, G
      INTEGER J1S(1)
!-----------------------------------------------
      DATA ETA, TOL/ 1.2E-7, 2.4E-32/
!-----------------------------------------------
      NP = N + IP
      N1 = N + 1
!   Householder reduction
      C(1) = 0.
      K = 1
   10 CONTINUE
      K1 = K + 1
!   Elimination of A(I,K) , I=K+1,...,M
      Z = 0.
      DO I=K,M
        Z=Z+A(I,K)**2
      ENDDO
      B(K) = 0.
      IF (Z > TOL) THEN
        Z = SQRT(Z)
        B(K) = Z
        W = ABS(A(K,K))
        Q = (1.,0.)
        IF (W /= 0.) Q = A(K,K)/W
        A(K,K) = Q*(Z + W)
        IF (K /= NP) THEN
          DO J=K1,NP
            Q=(0.,0.)
            DO I=K,M
              Q=Q+A(I,K)*A(I,J)
            ENDDO
            Q=Q/(Z*(Z+W))
            DO I=K,M
              A(I,J)=A(I,J)-Q*A(I,K)
            ENDDO
          ENDDO
!   Phase transformation
          if (a(k,k) == 0.0) go to 9999         ! added 2001-10-29
          Q = -A(K,K)/ABS(A(K,K))
          A(K,K1:NP) = Q*A(K,K1:NP)
!   Elimination of A(K,J) , J=K+2,...,N
        ENDIF
      ENDIF
      IF (K == N) GO TO 140
      Z = 0.
      Z = SUM(A(K,K1:N)**2)
      C(K1) = 0.
      IF (Z > TOL) THEN
        Z = SQRT(Z)
        C(K1) = Z
        W = ABS(A(K,K1))
        Q = (1.,0.)
        IF (W /= 0.) Q = A(K,K1)/W
        A(K,K1) = Q*(Z + W)
        DO I=K1,M
        Q=(0.,0.)
          DO J=K1,N
            Q=Q+A(K,J)*A(I,J)
          ENDDO
          if (z*(z+w) == 0.0) go to 9999        ! added 2001-10-29
          Q=Q/(Z*(Z+W))
          DO J=K1,N
            A(I,J)=A(I,J)-Q*A(K,J)
          ENDDO
        ENDDO
!   Phase transformation
        if (a(k,k1) == 0.0) go to 9999          ! added 2001-10-29
        Q = -A(K,K1)/ABS(A(K,K1))
        A(K1:M,K1) = A(K1:M,K1)*Q
      ENDIF
      K = K1
      GO TO 10
!   Tolerance for negligible elements
  140 CONTINUE
      EPS = 0.
      DO K = 1, N
        S(K) = B(K)
        T(K) = C(K)
        EPS = AMAX1(EPS,S(K)+T(K))
      END DO
      EPS = EPS*ETA
!   Initialization of U and V
      IF (NU /= 0) THEN
        DO J = 1, NU
          U(:M,J) = (0.,0.)
          U(J,J) = (1.,0.)
        END DO
      ENDIF
      IF (NV /= 0) THEN
        DO J = 1, NV
          V(:N,J) = (0.,0.)
          V(J,J) = (1.,0.)
        END DO
      ENDIF
!   QR diagonalization
      DO KK = 1, N
        K = N1 - KK
!   Test for split
  220   CONTINUE
        DO LL = 1, K
          L = K + 1 - LL
          IF (ABS(T(L)) <= EPS) GO TO 290
          IF (ABS(S(L-1)) > EPS) CYCLE
          EXIT
        END DO
!   Cancellation of B(L)
        CS = 0.
        SN = 1.
        L1 = L - 1
        DO I = L, K
          F = SN*T(I)
          T(I) = CS*T(I)
          IF (ABS(F) <= EPS) EXIT
          H = S(I)
          W = SQRT(F*F + H*H)
          S(I) = W
          if (w == 0.0) go to 9999        ! added 2001-10-29
          CS = H/W
          SN = -F/W
          IF (NU /= 0) THEN
            DO J = 1, N
              X = REAL(U(J,L1))
              Y = REAL(U(J,I))
              U(J,L1) = X*CS + Y*SN
              U(J,I) = Y*CS - X*SN
            END DO
          ENDIF
          IF (NP == N) CYCLE
          DO J=N1,NP
            Q=A(L1,J)
            R=A(I,J)
            A(L1,J)=Q*CS+R*SN
            A(I,J)=R*CS-Q*SN
          ENDDO
        END DO
!   Test for convergence
  290   CONTINUE
        W = S(K)
        IF (L == K) GO TO 360
!   Origin shift
        X = S(L)
        Y = S(K-1)
        G = T(K-1)
        H = T(K)
        if (h*y == 0.0) go to 9999        ! added 2001-10-29
        F = ((Y - W)*(Y + W) + (G - H)*(G + H))/(2.*H*Y)
        G = SQRT(F*F + 1.)
        IF (F < 0.) G = -G
        if (x == 0.0) go to 9999          ! added 2001-10-29
        F = ((X - W)*(X + W) + (Y/(F + G) - H)*H)/X
!   QR step
        CS = 1.
        SN = 1.
        L1 = L + 1
        DO I = L1, K
          G = T(I)
          Y = S(I)
          H = SN*G
          G = CS*G
          W = SQRT(H*H + F*F)
          T(I-1) = W
          if (w == 0.0) go to 9999        ! added 2001-10-29
          CS = F/W
          SN = H/W
          F = X*CS + G*SN
          G = G*CS - X*SN
          H = Y*SN
          Y = Y*CS
          IF (NV /= 0) THEN
            DO J = 1, N
              X = REAL(V(J,I-1))
              W = REAL(V(J,I))
              V(J,I-1) = X*CS + W*SN
              V(J,I) = W*CS - X*SN
            END DO
          ENDIF
          W = SQRT(H*H + F*F)
          S(I-1) = W
          if (w == 0.0) go to 9999        ! added 2001-10-29
          CS = F/W
          SN = H/W
          F = CS*G + SN*Y
          X = CS*Y - SN*G
          IF (NU /= 0) THEN
            DO J=1,N
              Y=U(J,I-1)
              W=U(J,I)
              U(J,I-1)=Y*CS+W*SN
              U(J,I)=W*CS-Y*SN
            ENDDO
          ENDIF
          IF (N == NP) CYCLE
          DO J=N1,NP
            Q=A(I-1,J)
            R=A(I,J)
            A(I-1,J)=Q*CS+R*SN
            A(I,J)=R*CS-Q*SN
          ENDDO
        END DO
        T(L) = 0.
        T(K) = F
        S(K) = X
        GO TO 220
!   Convergence
  360   CONTINUE
        IF (W >= 0.) CYCLE
        S(K) = -W
        IF (NV == 0) CYCLE
        V(:N,K) = -V(:N,K)
      END DO
!   Sort singular values
      DO K = 1, N
        G = -1.
        J = K
        IF (N - K + 1 > 0) THEN
          J1S = MAXLOC(S(K:N)) - 1 + K
          IF (S(J1S(1)) > G) THEN
            G = S(J1S(1))
            J = J1S(1)
          ENDIF
        ENDIF
        IF (J == K) CYCLE
        S(J) = S(K)
        S(K) = G
        IF (NV /= 0) THEN
          DO I=1,N
            Q=V(I,J)
            V(I,J)=V(I,K)
            V(I,K)=Q
          ENDDO
        ENDIF
        IF (NU /= 0) THEN
          DO I=1,N
            Q=U(I,J)
            U(I,J)=U(I,K)
            U(I,K)=Q
          ENDDO
        ENDIF
        IF (N == NP) CYCLE
        DO I=N1,NP
          Q=A(J,I)
          A(J,I)=A(K,I)
          A(K,I)=Q
        ENDDO
      END DO
!   Back transformation
      IF (NU /= 0) THEN
        DO KK = 1, N
          K = N1 - KK
          IF (B(K) == 0.) CYCLE
          if (a(k,k) == 0.0) go to 9999        ! added 2001-10-29
          Q = -A(K,K)/ABS(A(K,K))
          U(K,:NU) = Q*U(K,:NU)
          DO J = 1, NU
            Q = SUM(A(K:M,K)*U(K:M,J))
            Q = Q/(ABS(A(K,K))*B(K))
            U(K:M,J) = U(K:M,J) - Q*A(K:M,K)
          END DO
        END DO
      ENDIF
      IF (NV /= 0) THEN
        IF (N >= 2) THEN
          DO KK = 2, N
            K = N1 - KK
            K1 = K + 1
            IF (C(K1) == 0.) CYCLE
            if (a(k,k1) == 0.0) go to 9999        ! added 2001-10-29
            Q = -A(K,K1)/ABS(A(K,K1))
            V(K1,:NV) = Q*V(K1,:NV)
            DO J = 1, NV
              Q = SUM(A(K,K1:N)*V(K1:N,J))
              Q = Q/(ABS(A(K,K1))*C(K1))
              V(K1:N,J) = V(K1:N,J) - Q*A(K,K1:N)
            END DO
          END DO
        ENDIF
      ENDIF
      return
9999  s(:) = 0.0                         ! added 2001-10-29
      s(1) = 1.0                         ! added 2001-10-29
      if (present(u)) u(:,:) = 0.0       ! added 2001-10-29
      if (present(v)) v(:,:) = 0.0       ! added 2001-10-29
      return                             ! added 2001-10-29
      end subroutine rsvd


!!----------------------------- end of module -----------------------------!!
!!----------------------------- end of module -----------------------------!!
!!----------------------------- end of module -----------------------------!!


      end module rsvd_module


!!--------------------------------- end -----------------------------------!!
!!--------------------------------- end -----------------------------------!!
!!--------------------------------- end -----------------------------------!!

