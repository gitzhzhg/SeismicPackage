!<CPS_v1 type="PRIMITIVE"/>
!!------------------------------- ap1.f90 --------------------------------!!
!!------------------------------- ap1.f90 --------------------------------!!
!!------------------------------- ap1.f90 --------------------------------!!
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
! Name       : ap1 
! Category   : velocity
! Written    : 2003-04-22   by: Michael Ried
! Revised    : 2005-01-31   by: Michael Ried
! Maturity   : production
! Purpose    : Routines to be used by ALAMO, ABRA, and other routines
! Portability: No known limitations.
!-------------------------------------------------------------------------------
!</brief_doc>


!<descript_doc>
!-------------------------------------------------------------------------------
!                         GENERAL DESCRIPTION            
!
! Purpose    : Routines to be used by ALAMO, ABRA, and other routines
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
!!!                                 i   i   o  o   o  o   o
!!!              call ap1_cofgen1 (lr, bw, rw, g, ff, a, coef)
!!!
!!!
!!! integer         lr      =    --> Number of coefficients in the filters
!!! real            bw      =    --> Maximum frequency as a fraction of nyquist
!!! real            rw(:)   =    --> Array for autocorrelation
!!! real            g(:)    =    --> Array for cross correlation
!!! real            ff(:)   =    --> Filter array used by EUREKA
!!! real            coef(:) =    --> Coefficient array 99 FILTERS OF LENGTH LR.
!!!
!!!                              i  i  i  i  i  o  io
!!!              call ap1_trint (a, n, f, l, m, b, c)
!!!
!!!
!!! real            a(:)    =    --> Trace to interpolate
!!! integer         n       =    --> Number of samples
!!! real            f(:)    =    --> Interpolation filters (LENGTH = 101*L)
!!! integer         l       =    --> Length of the filters
!!! integer         m       =    --> Number of output samples/input sample
!!!                                  (Allowed values: 5,10,20,25,50,100)
!!! real            b(:)    =    --> Interpolated trace
!!! real            c(:)    =    --> Temporary Work area (N + L)
!!!
!-------------------------------------------------------------------------------
!</calling_doc>

!<history_doc>
!-------------------------------------------------------------------------------
!                             REVISION HISTORY              
!
!     Date        Author        Description
!     ----        ------        -----------
!  3. 2005-01-31  Michael Ried  Changed the end value of B in ap1_trint
!  2. 2003-04-22  Michael Ried  Moved into CPS
!  1. 1993-10-18  D. Corigan    Initial version.
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
!
      MODULE AP1_MODULE
      use eureka_module
      use named_constants_module
!
      IMPLICIT NONE
!
      PUBLIC
!
      character(len=100),public,save :: AP1_IDENT = &
'$Id: ap1.f90,v 1.3 2005/01/31 14:05:04 Ried prod sps $'
!
      contains
!!------------------------------- ap1_cofgen1------------------------------!!
!!------------------------------- ap1_cofgen1------------------------------!!
!!------------------------------- ap1_cofgen1------------------------------!!

      SUBROUTINE AP1_COFGEN1(LR,BW,RW,G,FF,A,COEF)
!
      IMPLICIT NONE
      INTEGER          ,INTENT(IN)    :: LR        ! ARGUMENTS
      REAL             ,INTENT(IN)    :: BW        ! ARGUMENTS
      REAL             ,INTENT(OUT)   :: RW(:)     ! ARGUMENTS (LEN=LR)
      REAL             ,INTENT(OUT)   :: G(:)      ! ARGUMENTS (LEN=LR)
      REAL             ,INTENT(OUT)   :: FF(:)     ! ARGUMENTS (LEN=LR+2)
      REAL             ,INTENT(OUT)   :: A(:)      ! ARGUMENTS (LEN=LR+2)
      REAL             ,INTENT(OUT)   :: COEF(:)   ! ARGUMENTS (LEN=LR*101)
!
!**********************************************************************         
!                                                                               
!   AUTHOR   C.J. SICKING                                                       
!                                                                               
!   Revised:        10/18/93      D. Corigan
!                                                                               
!                                 Output 101 filters - include the
!                                 endpoints where the filters are
!                                 unit spikes at LR/2 or LR/2 +1
!
!  ----------------------------------------------------------------------
!
!   AP1_COFGEN COMPUTES 99 INTERPOLATION FILTERS ON EACH CALL.  EACH 
!   FILTER IS LR COEFFICIENTS IN LENGTH.  THESE 99 FILTERS CAN BE               
!   USED TO COMPUTE THE VALUE OF A SIGNAL TO THE NEAREST 0.01 SAMPLE            
!   INTERVAL.  COMPUTING THE VALUE TO THE NEAREST 0.01 SAMPLE                   
!   INTERVAL IS ADEQUATE TO MAINTAIN 30 TO 40 DB FIDELITY IN THE                
!   INTERPOLATION PROCESS                                                       
!                                                                               
!        X1          X2       P  X3          X4                                 
!                                                                               
!   IF WE HAVE A SIGNAL WITH SAMPLES AT LOCATIONS X1,X2,X3 AND X4 AND           
!   WE WANT THE VALUE OF THE SIGNAL AT LOCATION P, WE WOULD FIRST               
!   COMPUTE THE FRACTIONAL DISTANCE BY DIVIDING THE DISTANCE P-X2 BY            
!   THE DISTANCE X3-X2 AND THEN ROUND THE RESULT TO THE NEAREST 0.01            
!   AND MULTIPLY BY 100.  THE RESULT IS A NUMBER BETWEEN 0 AND 100.             
!   THIS IS THE INDEX  TO THE FILTER ARRAY AND IS USED TO DETERMINE             
!   WHICH OF THE 99 FILTERS TO USE IN THE INTERPOLATION.  IF THE RESULT         
!   IS 0 OR 100, P IS WITHIN PLUS OR MINUS 0.005*(SAMPLES INTERVAL)             
!   OF X2 OR X3 AND IS CLOSE ENOUGH TO USE THE                                  
!   KNOWN VALUE WITHOUT GOING THROUGH THE INTERPOLATION STEP.                   
!                                                                               
!                                                                               
!   THE FILTERS COMPUTED ARE WIENER FILTERS.  A CORRELATION FUNCTION            
!   IS REQUIRED IN ORDER TO COMPUTE THE FILTERS.  THE FILTERS ARE               
!   COMPUTED USING THE MATRIX EQUATION:                                         
!                                                                               
!               A F = C                                                         
!                                                                               
!   WHERE A IS THE AUTOCORRELATION MATRIX AND C IS THE CROSS CORRELATION        
!   MATRIX.  THE EQUATION IS SOLVED USING A MODIFIED VERSION OF                 
!   SUBROUTINE EUREKA FROM ROBINSON'S BOOK.                                     
!   THE CORRELATION FUNCTION USED IS A BANDLIMITED SINC FUNCTION.               
!   FOR DETAILS OF THE INTERPOLATION PROCEDURE AND HOW TO THE PARAMETERS        
!   ARE RELATED SEE SICKING' PH.D. DISSERTATION.                                
!   LR AND BW ARE INTERRELATED AND THEIR VALUES SHOULD BE CHOSEN BASED          
!   ON KNOWGLEDGE OF THE MAXIMUM FREQUENCY PRESENT IN THE SIGNAL AND            
!   THE CURVES PRESENTED IN SICKING'S DISSERTATION CHAPTER 4.                   
!                                                                               
!   PARAMETERS                                                                  
!                                                                               
!   LR    -  NUMBER OF COEFFICIENTS IN THE FILTERS                              
!   BW    -  MAXIMUM FREQUENCY AS A FRACTION OF NYQUIST                         
!   RW    -  ARRAY FOR AUTOCORRELATION                                          
!   G     -  ARRAY FOR CROSS CORRELATION                                        
!   FF    -  FILTER ARRAY USED BY EUREKA                                        
!   A     -  WORK ARRAY USED BY EUREKA                                          
!   COEF  -  COEFFICIENT ARRAY (OUTPUT).  99 FILTERS OF LENGTH LR.              
!                                                                               
!   ARRAYS RW AND G NEED TO BE DIMENSIONED TO LENGTH LR IN THE                  
!   CALLING PROGRAM.                                                            
!   ARRAYS F AND G NEED TO BE DIMENSIONED TO LENGTH LR+2 IN THE                 
!   CALLING PROGRAM.                                                            
!   THE PARAMETER CORD IS THE LAG IN NUMBER OF SAMPLE INTERVALS.                
!                                                                               
      REAL CORD, CORDIS, FRAC, TX, VAL
      INTEGER I,INO,INO1,INO2,J,K,L,LCN,M
!
      CORD=0.0
      DO I=1,LR
        TX=CORD*PI*BW
        IF (TX.NE.0.0) THEN
          VAL=SIN(TX)/TX
        ELSE
          VAL=1.0
        END IF
        RW(I)=VAL
        CORD=CORD+1.0
      END DO
!
!   First filter
!
      DO I = 1,LR
        COEF(I) = 0.
      ENDDO
      COEF(LR/2)  = 1.
!                                                                               
!   COMPUTE EACH OF NEXT 50 FILTERS                                            
!                                                                               
!   LCN IS INCREMENTED FROM 2 TO 51 WHILE FRAC IS INCREMENTED FROM              
!   O.01 TO 0.50.  THE CROSS CORRELATION ARRAY IS FILLED ,THE FILTER            
!   IS COMPUTED, AND THE FILTER IS STORED IN THE OUTPUT COEF ARRAY.             
!                                                                               
!   THE PARAMETER CORDIS IS THE CORRELATION LAG IN SAMPLE SPACINGS              
!   BETWEEN A GIVEN SAMPLE AND THE DESIRED SAMPLE LOCATION.                     
!                                                                               
      LCN=2
      FRAC=0.01
  190 CORDIS=-((LR/2)-1)-FRAC
!                                                                               
!   COMPUTE CROSS CORRELATION VALUES
!                                                                               
      DO I=1,LR
        TX=CORDIS*PI*BW
        IF (TX.NE.0.0) THEN
          VAL=SIN(TX)/TX
        ELSE
          VAL=1.0
        END IF
        G(I)=VAL
        CORDIS=CORDIS+1.0
      END DO
!
!   COMPUTE FILTER AND STORE IN COEF ARRAY
!
      CALL EUREKA(LR,RW,G,FF,A)
!
      DO I=1,LR
        INO=I+(LCN-1)*LR
        COEF(INO)=FF(I)
      END DO
!
!   INCREMENT FRAC AND LCN AND RETURN TO COMPUTE THE NEXT FILTER.               
!
      FRAC=FRAC+0.01
      LCN=LCN+1
      IF (LCN.LE.51) GO TO 190
!                                                                               
!   THE FIRST 51 FILTERS HAVE BEEN COMPUTED.  FILTER 50 IS THE
!   FILTER TO BE USED FOR MIDPOINT INTERPOLATION.
!                                                                               
!   THERE IS SYMETRY ABOUT MIDPOINT. REVERSE FIRST 50 FILTERS AND
!   STORE IN LAST 50.
!                                                                               
      DO I=1,50
        J=51-I
        K=51+I
        DO L=1,LR
          M=LR-L+1
          INO1=L+(J-1)*LR
          INO2=M+(K-1)*LR
          COEF(INO2)=COEF(INO1)
        ENDDO
      ENDDO
!
      RETURN
      END SUBROUTINE AP1_COFGEN1
!!------------------------------- ap1_trint---------------------------------!!
!!------------------------------- ap1_trint---------------------------------!!
!!------------------------------- ap1_trint---------------------------------!!
!  >>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>  <<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<          
!                                                                               
!               SUBROUTINE AP1_TRINT
!                                                                               
!  >>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>  <<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<         
!                                                                               
      SUBROUTINE AP1_TRINT( A,N,F,L,M,B,C )
!
      IMPLICIT NONE
      REAL             ,INTENT(IN)    :: A(:)     ! ARGUMENTS
      INTEGER          ,INTENT(IN)    :: N        ! ARGUMENTS
      REAL             ,INTENT(IN)    :: F(:)     ! ARGUMENTS
      INTEGER          ,INTENT(IN)    :: L        ! ARGUMENTS
      INTEGER          ,INTENT(IN)    :: M        ! ARGUMENTS
      REAL             ,INTENT(OUT)   :: B(:)     ! ARGUMENTS
      REAL             ,INTENT(INOUT) :: C(:)     ! ARGUMENTS
!                                                                               
!         STORE INTERPOLATED TRACE                                              
!                                                                               
!                 ARGUMENTS (INPUT)                                             
!                                                                               
!             -     A       -  TRACE TO INTERPOLATE                             
!             -     N       -  NUMBER OF SAMPLES                                
!             -     F       -  INTERPOLATION FILTERS  (LENGTH = 101*L)          
!             -     L       -  LENGTH OF THE FILTERS                            
!             -     M       -  NUMBER OF OUTPUT SAMPLES/INPUT SAMPLE            
!                              (ALLOWED VALUES: 5,10,20,25,50,100)              
!                                                                               
!                 ARGUMENTS (OUTPUT)                                            
!                                                                               
!             -     B       -  INTERPOLATED TRACE                               
!                                                                               
!                 ARGUMENTS (TEMPORARY)                                         
!                                                                               
!             -     C       -  WORK AREA (N + L)                                
!                                                                               
! ---------------------------------------------------------------------         
!                                                                               
      INTEGER I, IC, IE1, IE2, IK, J, K, LH
      REAL ZERO
!
      DATA ZERO/0./                                                             
!                                                                               
!       COPY INPUT DATA TO C PADDING WITH L/2 ZEROES ON EACH END                
!                                                                               
      LH   = L/2                                                                
      IC   = 1                                                                  
      IE2=IC+(LH-1)-1
      C(IC:IE2)=0.0
      IC = IC + LH - 1
      IE2=IC+N-1
      C(IC:IE2)=A(1:N)
      IC   = IC +  N                                                            
      IE2=IC+LH-1
      C(IC:IE2)=0.0
!                                                                               
      K    = 1                                                                  
      IK   = (100/M)*L                                                          
      IE2=N*M
      B(1:IE2)=0.0
!
!
!       ADD ARRAY VALUES BASED ON THE STRIDE VALUES
!
      DO J = 1,M
          DO I = 1,L
              IE1=I+N-1
              IE2=J+M*(N-1)
              B(J:IE2:M)=(C(I:IE1)*F(K+I-1))+B(J:IE2:M)
          ENDDO
          K  = K + IK
      ENDDO
!                                                                               
!                                                                               
! ---------------------------------------------------------------------         
!                                                                               
      RETURN                                                                    
      END SUBROUTINE AP1_TRINT
!!----------------------------- end of module -----------------------------!!
!!----------------------------- end of module -----------------------------!!
!!----------------------------- end of module -----------------------------!!
      END MODULE AP1_MODULE
