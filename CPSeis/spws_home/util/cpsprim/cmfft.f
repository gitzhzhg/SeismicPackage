C ***************************************************************************
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
      SUBROUTINE CMFFT(DI,DO,NLN,ISIGN)
C***************************** COPYRIGHT NOTICE *****************************
C*									    *
C*		  CONFIDENTIAL AND PROPRIETARY INFORMATION		    *
C*			       OF CONOCO INC.				    *
C*		       PROTECTED BY THE COPYRIGHT LAW			    *
C*			   AS AN UNPUBLISHED WORK			    *
C*									    *
C****************************************************************************
C\USER DOC
C-----------------------------------------------------------------------
C
C		EXPLORATION RESEARCH AND SERVICES DIVISION
C			       CONOCO, INC.
C
C  Process Name:  CMFFT, RMFFT, NFCTR
C	 Author:  R. H. Stolt
C  Last Revised:  91/07/31
C
C  Purpose:  CMFFT performs a mixed radix FFT on complex data.	Its
C	     companion RMFFT performs a mixed radix FFT on real data.
C	     The routines are written in generic FORTRAN so as to be
C	     usable on any host computer, including CRAYs, VAXes, PC's,
C	     and workstations, without recourse to software packages
C	     which may not be available on all machines.  NFCTR is a
C	     supporting function which finds the next multiple of 2's,
C	     3's, and 5's larger than a given number.
C
C-----------------------------------------------------------------------
C			   INPUT PARAMETERS
C   See Program Documentation
C-----------------------------------------------------------------------
C   These subroutines are re-enterable
C-----------------------------------------------------------------------
C			      NOTES
C
C  1. These routines differ from "standard" FFT's in that they do not
C     require data length to be a power of two.  They do require that
C     data length NLN be a product of 2's, 3's, and 5's; i.e., that
C		     NLN = 2^M2 * 3^M3 * 5^M5.
C     Not all the exponents need be nonzero; however, for CMFFT, M2
C     must be nonzero (that is, NLN must be even) and for RMFFT, M2
C     must be larger than one (that is, NLN must be divisible by four).
C
C  2. If, on entry to CMFFT or RMFFT, NLN does not meet the above
C     requirements, it is replaced by the smallest bigger number which
C     does, and the input data extended to the new NLN by appending
C     zeroes.
C
C  3. The smallest even multiple of 2's, 3's, and 5's can be found
C     prior to calling the FFT's by calling the function NFCTR:
C		    NLN = NFCTR( N ),
C     where N is the minimum required FFT length.  For RMFFT, the
C     number to find is
C		    NLN = 2 * NFCTR ( (N+1)/2 ).
C     This makes NLN a multiple of 4, which is required because RMFFT
C     calls CMFFT with NLN = NLN/2.
C
C  4. Sizes of DI and DO are as follows:
C     _____________________________________
C     |	    |	CMFFT | RMFFT	|   RMFFT |
C     |	    |	      |	ISIGN=1 | ISIGN=-1|
C     |___________________________________|
C     | DI  |	2*NLN |	 NLN+2	|   NLN	  |
C     |	    |	      |		|	  |
C     | DO  |	2*NLN |	  NLN	|  NLN+2  |
C     |___________________________________|
C
C     For RMFFT, the frequency-domain array contains only positive
C     frequencies 0 through NLN/2; CMFFT contains positive and negative
C     in the order 0,1,...,NLN/2-1,+-NLN/2,-NLN/2+1,...,-1.
C
C  5. Scaling.	Both CMFFT and RMFFT are scaled so that a forward FFT
C     followed by an inverse FFT multiplies the data by NLN.  That is,
C     to recover the data's original scale factor, multiply each
C     element by 1./FLOAT( NLN ).  I did not provide a routine to do
C     this for you, so you will have to do it yourself.
C
C  6. Why do mixed radix FFT's?
C     a) You can use shorter arrays.  A 1025 length array can be
C	 accomodated by a mixed radix length of 1080; the nearest
C	 power of two is 2048.
C     b) Its faster.  It turns out that for a given length, the mixed
C	 radix FFT, if equivalently optimized, will be a little faster.
C	 This is mainly because radix-four FFT's go like gangbusters.
C	 Threes and fives are a little slower, but not too bad, so the
C	 net result for most numbers is some improvement.
C
C  7. Is there a reason not to?
C	 CMFFT has a lot more code to it than a simple power-of-two
C	 FFT, hence hogs more memory.  It also requires separate
C	 arrays for input and output data.
C
C  8. Where is the SINE-COSINE table?
C	 These routines generate their exponents as they need them.  A
C	 SINE-COSINE table would decrease execution times by 5 to 8
C	 percent, which was not in my judgment a sufficient incentive
C	 to use them.  They are a pain for the user to generate and
C	 keep track of, especially when doing multi-dimensional FFT's.
C
C  9. Why didn't I use a single array for input and output?
C	 That's harder to do for mixed radix transforms.  The bit-
C	 reversal step required in a power-of-two FFT becomes a factor-
C	 order reversal in the mixed-radix case.  The former is a
C	 simple transposition of pairs of data elements; the latter
C	 mapping is not a transposition, and I did not see an easy
C	 and messless way to perform it in place.  Sorry.
C-----------------------------------------------------------------------
C \END DOC
C\PROG DOC
C-----------------------------------------------------------------------
C			  REVISION HISTORY
C 91/07/30     Reduced number of scratch variables in CMFFT, added zero
C	       padding to inverse RMFFT.
C 91/07/30     Touched up double precision to satisfy CRAY compiler.
C 91/07/31     Removed some inessential integer variables.
C-----------------------------------------------------------------------
C			  CALLING SEQUENCE
C
C   SUBROUTINE CMFFT ( DI, DO, NLN, ISIGN )
C   SUBROUTINE RMFFT ( DI, DO, NLN, ISIGN )
C   INTEGER FUNCTION NFCTR ( NLN )
C
C     DI    =  INPUT ARRAY (Complex for CMFFT and RMFFT with ISIGN
C	       set to 1; real for RMFFT with ISIGN set to -1)
C     DO    =  OUTPUT ARRAY (Complex for CMFFT and for RMFFT if
C	       ISIGN = -1; real for RMFFT if ISIGN = 1)
C     NLN   =  ARRAY LENGTH
C     ISIGN =  SIGN OF EXPONENT IN TRANSFORM.  FOR A FORWARD FFT,
C	       ISIGN = - 1.  FOR INVERSE, ISIGN = 1.
C     NFCTR =  SMALLEST EVEN PRODUCT OF 2'S, 3'S, AND 5'S WHICH
C	       CONTAINS NLN
C
C-----------------------------------------------------------------------
C			      NOTES
C
C  1. NFCTR hides more parameters in common block /NFCTRPA/.  They are
C	 NFAC		-- Number of factors in NFCTR
C	 M2, M3, M4, M5 --  Number of 2's, 3's, 4's, and 5's in NFCTR,
C	    respectively.  (M4 is always chosen so that M2 < 2.)
C	 MFAC(12)	-- MFAC(J) is the Jth factor in NFCTR.	Thus,
C	    each factor is a 5, 4, 3, or 2.  They are ordered so that
C	    5's come before 4's, etc.  Note that MFAC allows for only
C	    twelve factors, so the largest FFT these routines can
C	    reliably perform without modification is 354,294.
C
C-----------------------------------------------------------------------
C     LIST OF ALL SUBROUTINE, FUNCTION, ENTRY AND COMMON BLOCK NAMES
C			     IN THIS MODULE
C Subroutines:
C		 RMFFT,      CMFFT
C Functions:
C		 NFCTR
C Common Blocks:
C		/NFCTRPA/
C
C-----------------------------------------------------------------------
C	    LIST OF ALL EXTERNALS REFERENCED BY THIS MODULE
C
C	DCOS	     DSIN	 DASIN
C-----------------------------------------------------------------------
C			  MEMORY REQUIREMENTS
C Storage   -  none
C Scratch   -  none
C Parms     -  none
C-----------------------------------------------------------------------
C\END DOC
C
      REAL DI(*), DO(*)
      DOUBLE PRECISION XPR,XPI,DXPR,DXPI,XPR2,XPI2,XPR3,XPI3,XPR4,XPI4
      DOUBLE PRECISION DARG,DTMP,TWOPI
      COMMON /NFCTRPA/ NFAC, M2, M3, M4, M5, MFAC( 12 )

C
C   FFT OF SINGLE COMPLEX TRACE
C   DB	  = INPUT DATA ARRAY
C   DA	  = OUTPUT DATA ARRAY
C   NLN   = ARRAY LENGTH
C   ISIGN = SENSE OF FFT
C     LENGTH N OF FFT IS PRODUCT OF FACTORS:
C	 N = MFAC(1) * MFAC(2) * ... * MFAC(NFAC)
C     THE FACTORS MFAC(J) CAN BE 2'S, 3'S, 4'S, AND 5'S, AND MUST INCLUDE
C     AT LEAST ONE TWO OR FOUR.  THE PROGRAM WILL PICK THE SMALLEST SUCH
C     N NOT SMALLER THAN NLN.  IF NLN < N, DATA WILL BE PADDED WITH 0'S.
C   ISIGN = SENSE OF FFT.  ALGORITHM IS
C     FFT (K) = SUM OVER J EXP { i ISIGN TWOPI/N J*K }
C     FOR USUAL CONVENTION,
C     FOR FORWARD FFT,	ISIGN = - 1;
C     FOR INVERSE FFT,	ISIGN = + 1.
C   NOTE THAT AFTER FORWARD AND INVERSE FFT, DATA IS EFFECTIVELY
C   MULTIPLIED BY N.

C   FIND THE SMALLEST POSSIBLE N AND ITS FACTORS.
      N = NFCTR( NLN )
C  THE NUMBER NFAC OF FACTORS AND THE SEQUENCE MFAC(J) OF FACTORS ARE
C  RETURNED IN THE COMMON BLOCK /NFCTRPA/

C   FACTOR REVERSAL STEP
      ND = 2*N
      NH = N/2
      IREV = 1

      DO 150 IFWD = 1, ND, 2

C   IFWD IS DECOMPOSABLE AS
C IFWD = 1 + 2*[L(1)+MFAC(1)*[L(2)+MFAC(2)*[L(3)+...MFAC(NFAC-1)*L(NFAC)]..]]]
C   WHERE O <= L(J) < MFAC(J).	THE SET OF DIGITS [ L(1), L(2), ..., L(NFAC) ]
C   THUS DESCRIBES THE NUMBER IFWD.
C   IREV IS THE NUMBER FORMED BY REVERSING THE ORDER OF THE DIGITS L:
C IREV = 1 + 2*[L(NFAC)+MFAC(NFAC)*[L(NFAC-1)+...+MFAC(2)*L(1)]..]].
C   THIS IS A GENERALIZATION OF RADIX TWO "BIT REVERSAL".  THE DATA MUST BE
C   MAPPED WITH ITS FACTORS REVERSED IN ORDER FOR THE FFT TO OUTPUT RESULTS
C   IN PROPER ORDER.
	 IF ( IREV .LT. NLN*2 ) THEN
	    DO( IFWD )	 = DI( IREV )
	    DO( IFWD+1 ) = DI( IREV+1 )
	 ELSE
	    DO( IFWD )	 = 0.
	    DO( IFWD+1 ) = 0.
	 END IF

C   FIND IREV FOR NEXT IFWD.  ADDING 2 TO IFWD BASICALLY MEANS ADDING 1
C   TO L(1) (WHICH ADDS 2*N/MFAC(1) TO IREV) UNLESS L(1) = MFAC(1) - 1,
C   IN WHICH CASE L(1) BECOMES 0 AND L(2) BECOMES L(2) + 1 ( WHICH ADDS
C   - 2*N/MFAC(1) * ( MFAC(1) - 1 - 1/MFAC(2) ) TO IREV ), UNLESS ETC.
      MPC = ND / MFAC( 1 )
      MPF = ND - MPC

      IF ( IREV .LE. MPF ) GOTO 150
      JFAC = 2
  130	 IREV = IREV - MPF
	 MPF = MPC
	 MPC = MPC / MFAC( JFAC )
	 MPF = MPF - MPC
	 JFAC = JFAC + 1
      IF( JFAC .LE. NFAC .AND. IREV .GT. MPF ) GOTO 130
  150 IREV = IREV + MPC

C   BEGIN FFT
      NA = 1
C  CALCULATE EXPONENTS FOR RADIX 3 AND 5 FFT'S
      TWOPI = DASIN(1.D0) * 4.
      SGN = ISIGN
      IF ( M3 .GT. 0 ) THEN
	 DARG = TWOPI / 3.D0
	 CS3 = DCOS(DARG)
	 SN3 = ISIGN * DSIN(DARG)
      END IF
      IF ( M5 .GT. 0 ) THEN
	 DARG = TWOPI / 5.D0
	 CS51 = DCOS(DARG)
	 SN51 = ISIGN * DSIN(DARG)
	 DARG = DARG + DARG
	 CS52 = DCOS(DARG)
	 SN52 = ISIGN * DSIN(DARG)
      END IF

C   ONE PASS IS REQUIRED THRU THE DATA FOR EACH FACTOR IN N
      DO 290 IPASS = 1, NFAC
	 M = MFAC( IPASS )
	 NAD = 2*NA
	 NAN = NA*M
	 DARG = TWOPI / (NAN * ISIGN )
	 DXPR = DCOS( DARG )
	 DXPI = DSIN( DARG )
	 XPR = 1.D0
	 XPI = 0.D0

C   LOOP OVER K VALUES
	 DO 280 K = 1, NA

C   IF FACTOR FOR THIS PASS IS A TWO
	    IF ( M .EQ. 2 ) THEN
	       DO 210 IND0 = K+K-1, ND, NAD*2

		 IND1 = IND0 + NAD
		 TMPR = DO( IND1 ) * XPR - DO( IND1+1 ) * XPI
		 TMPI = DO( IND1 ) * XPI + DO( IND1+1 ) * XPR

		 DO( IND1 ) = DO( IND0 ) - TMPR
		 DO( IND0 ) = DO( IND0 ) + TMPR

		 DO( IND1+1 ) = DO( IND0+1 ) - TMPI
		 DO( IND0+1 ) = DO( IND0+1 ) + TMPI
  210	      CONTINUE

C   IF FACTOR FOR THIS PASS IS A THREE
	    ELSEIF ( M .EQ. 3 ) THEN
	       XPR2 = XPR * XPR - XPI * XPI
	       XPI2 = 2.D0 * XPR * XPI
	       DO 230 IND0 = K+K-1, ND, NAD*3

		  IND1 = IND0 + NAD
		  TR1 = DO( IND1 ) * XPR - DO( IND1+1 ) * XPI
		  TI1 = DO( IND1 ) * XPI + DO( IND1+1 ) * XPR

		  IND2 = IND1 + NAD
		  TR2 = DO( IND2 ) * XPR2 - DO( IND2+1 ) * XPI2
		  TI2 = DO( IND2 ) * XPI2 + DO( IND2+1 ) * XPR2

		  TMPR = TR1 + TR2
		  TMPI = TI1 - TI2
		  TTR	     = DO(IND0) + TMPR * CS3
		  TSR	     = TMPI * SN3
		  DO(IND2)   = TTR + TSR
		  DO(IND1)   = TTR - TSR
		  DO(IND0)   = DO(IND0) + TMPR

		  TMPR = TR1 - TR2
		  TMPI = TI1 + TI2
		  TTR	     = DO(IND0+1) + TMPI * CS3
		  TSR	     = TMPR * SN3
		  DO(IND2+1) = TTR - TSR
		  DO(IND1+1) = TTR + TSR
		  DO(IND0+1) = DO(IND0+1) + TMPI
  230	      CONTINUE

C   IF FACTOR FOR THIS PASS IS FOUR
	 ELSEIF ( M .EQ. 4 ) THEN
	       XPR2 = XPR * XPR - XPI * XPI
	       XPI2 = 2.D0 * XPR * XPI
	       XPR3 = XPR2 * XPR - XPI2 * XPI
	       XPI3 = XPR2 * XPI + XPI2 * XPR

	       DO 250 IND0 = K+K-1, ND, NAD*4

		  IND1 = IND0 + NAD
		  TR1 = DO( IND1 ) * XPR - DO( IND1+1 ) * XPI
		  TI1 = DO( IND1 ) * XPI + DO( IND1+1 ) * XPR

		  IND2 = IND1 + NAD
		  TR2 = DO( IND2 ) * XPR2 - DO( IND2+1 ) * XPI2
		  TI2 = DO( IND2 ) * XPI2 + DO( IND2+1 ) * XPR2

		  IND3 = IND2 + NAD
		  TR3 = DO( IND3 ) * XPR3 - DO( IND3+1 ) * XPI3
		  TI3 = DO( IND3 ) * XPI3 + DO( IND3+1 ) * XPR3

		  TTR = DO( IND0 ) - TR2
		  TSR = SGN * ( TI1 - TI3 )
		  DO( IND3 ) = TTR + TSR
		  DO( IND1 ) = TTR - TSR

		  TTR = DO(IND0+1) - TI2
		  TSR = SGN * ( TR1 - TR3 )
		  DO(IND3+1) = TTR - TSR
		  DO(IND1+1) = TTR + TSR

		  TTR = DO( IND0 ) + TR2
		  TSR = TR1 + TR3
		  DO( IND2 ) = TTR - TSR
		  DO( IND0 ) = TTR + TSR

		  TTR = DO(IND0+1) + TI2
		  TSR = TI1 + TI3
		  DO(IND2+1) = TTR - TSR
		  DO(IND0+1) = TTR + TSR

  250	      CONTINUE

C   IF FACTOR FOR THIS PASS IS FIVE
	    ELSEIF ( M .EQ. 5 ) THEN
	       XPR2 = XPR * XPR - XPI * XPI
	       XPI2 = 2.D0 * XPR * XPI
	       XPR3 = XPR2 * XPR - XPI2 * XPI
	       XPI3 = XPR2 * XPI + XPI2 * XPR
	       XPR4 = XPR2 * XPR2 - XPI2 * XPI2
	       XPI4 = 2.D0 * XPR2 * XPI2

		  DO 270 IND0 = K+K-1, ND, NAD*5

		  IND1 = IND0 + NAD
		  TR1 = DO( IND1 ) * XPR - DO( IND1+1 ) * XPI
		  TI1 = DO( IND1 ) * XPI + DO( IND1+1 ) * XPR

		  IND2 = IND1 + NAD
		  TR2 = DO( IND2 ) * XPR2 - DO( IND2+1 ) * XPI2
		  TI2 = DO( IND2 ) * XPI2 + DO( IND2+1 ) * XPR2

		  IND3 = IND2 + NAD
		  TR3 = DO( IND3 ) * XPR3 - DO( IND3+1 ) * XPI3
		  TI3 = DO( IND3 ) * XPI3 + DO( IND3+1 ) * XPR3

		  IND4 = IND3 + NAD
		  TR4 = DO( IND4 ) * XPR4 - DO( IND4+1 ) * XPI4
		  TI4 = DO( IND4 ) * XPI4 + DO( IND4+1 ) * XPR4

		  TMPR = TR1 + TR4
		  TMPI = TI1 - TI4
		  TRBP = TR2 + TR3
		  TIBM = TI2 - TI3

		  TTR	 = DO(IND0)   + TMPR * CS51 + TRBP * CS52
		  TSR	 =		TMPI * SN51 + TIBM * SN52
		  DO(IND4)   =		TTR + TSR
		  DO(IND1)   =		TTR - TSR

		  TTR	 = DO(IND0)   +	TMPR * CS52 + TRBP * CS51
		  TSR	 =		TMPI * SN52 - TIBM * SN51
		  DO(IND3)   =		TTR + TSR
		  DO(IND2)   =		TTR - TSR

		  DO(IND0)   = DO(IND0)   + TMPR + TRBP

		  TMPR = TR1 - TR4
		  TMPI = TI1 + TI4
		  TRBP = TR2 - TR3
		  TIBM = TI2 + TI3

		  TTR	 = DO(IND0+1) + TMPI * CS51 + TIBM * CS52
		  TSR	 =		TMPR * SN51 + TRBP * SN52
		  DO(IND4+1) =		TTR - TSR
		  DO(IND1+1) =		TTR + TSR

		  TTR	 = DO(IND0+1) + TMPI * CS52 + TIBM * CS51
		  TSR	 =		TMPR * SN52 - TRBP * SN51
		  DO(IND3+1) =		TTR - TSR
		  DO(IND2+1) =		TTR + TSR

		  DO(IND0+1) = DO(IND0+1) + TMPI + TIBM

  270	      CONTINUE

C   ALL POSSIBLE FACTORS PROCESSED
	   END IF
C   UPDATE EXPONENTIAL
	   DTMP = XPR * DXPR - XPI * DXPI
	   XPI = XPR * DXPI + XPI * DXPR
	   XPR = DTMP

C   NEXT K VALUE
  280	 CONTINUE

C   NEXT PASS
  290 NA = NAN

      NLN = N
      RETURN
      END

C
C *********************************************************************
      SUBROUTINE RMFFT ( DI, DO, N, ISIGN )
      DIMENSION DI(*), DO(*)
      DOUBLE PRECISION DXPR,DXPI,XPR,XPI,TWOPI,DARG,DTMP
C
C   SUBROUTINE TO DO A REAL FFT OF LENGTH N.
C   IT WORKS BY CONVERTING THE DATA TO A COMPLEX ARRAY OF LENGTH N/2,
C   THEN CALLS CMFFT.
C
C   DI	    =  INPUT ARRAY , LENGTH N  FOR FORWARD FFT, N+2 FOR INVERSE
C   DO	    =  OUTPUT ARRAY, LENGTH N + 2 FOR FORWARD FFT, N FOR INVERSE
C   N	    =  DESIRED LENGTH.	IF NOT FACTORIZABLE BY 2'S, 3'S, 4'S,
C	       AND 5'S, WITH AT LEAST ONE FACTOR OF FOUR, N WILL BE
C	       REPLACED BY THE SMALLEST SUCH NUMBER CONTAINING N AND
C	       THE DATA WILL BE PADDED WITH ZEROS.
C   ISIGN   =  SIGN OF EXPONENT IN FFT.
C	       ISIGN = - 1 FOR A FORWARD REAL TO COMPLEX FFT.
C	       ISIGN = + 1 FOR AN INVERSE COMPLEX TO REAL FFT.

C  BASIC CONSTANTS
      TWOPI = DASIN(1.D0) * 4.D0
      NH = ( N+1 ) / 2


C  FORWARD FFT
      IF (ISIGN .EQ. -1 ) THEN
	 CALL CMFFT( DI, DO, NH, ISIGN )
	 N = NH * 2
	 DARG = - TWOPI / N
	 DO(N+1) = DO(1) - DO(2)
	 DO(N+2) = 0.
	 DO(1)	 = DO(1) + DO(2)
	 DO(2)	 = 0.
	 DO (NH+2) = - DO(NH+2)
	 DXPR = DCOS(DARG)
	 DXPI = DSIN(DARG)
	 XPR =	DXPR * .5D0
	 XPI =	DXPI * .5D0
	 DO 180 K = 3, NH-1, 2

	    OR = ( DO(K+1)   + DO(N+3-K) )
	    OI = ( DO(N+2-K) - DO(K)	 )

	    ER = ( DO(K)     + DO(N+2-K) ) * .5
	    FR = XPR * OR - XPI * OI
	    DO(K)     =	 ER + FR
	    DO(N+2-K) =  ER - FR

	    ER = ( DO(K+1)   - DO(N+3-K) ) * .5
	    FR = XPI * OR + XPR * OI
	    DO(K+1)   =	 ER + FR
	    DO(N+3-K) = -ER + FR

	    DTMP = XPR * DXPR - XPI * DXPI
	    XPI  = XPR * DXPI + XPI * DXPR
	    XPR = DTMP
  180	 CONTINUE
C  INVERSE COMPLEX TO REAL FFT
      ELSEIF (ISIGN .EQ. 1 ) THEN
	 NN = 2 * NFCTR( NH )
	 IF ( NN .GT. N ) THEN
	    DO 210 K = N+3, NN+2
	       DI(K) = 0.
  210	    CONTINUE
	    N = NN
	    NH = N/2
	 END IF
	 DARG = TWOPI / N
	 ER    =  (DI(1) + DI(N+1) )
	 DI(2) =  (DI(1) - DI(N+1) )
	 DI(1) =   ER
	 DI (NH+1) =   DI(NH+1) * 2.
	 DI (NH+2) = - DI(NH+2) * 2.
	 DXPR = DCOS(DARG)
	 DXPI = DSIN(DARG)
	 XPR = - DXPR
         XPI = - DXPI
	 DO 280 K = 3, NH-1, 2

	    OR = ( DI(K+1)   + DI(N+3-K) )
	    OI = ( DI(N+2-K) - DI(K)	 )

	    ER = ( DI(K)     + DI(N+2-K) )
	    FR =  XPR * OR - XPI * OI
	    DI(K)     =	 ER + FR
	    DI(N+2-K) =  ER - FR

	    ER = ( DI(K+1)   - DI(N+3-K) )
	    FR =  XPI * OR + XPR * OI
	    DI(K+1)   =  ER + FR
	    DI(N+3-K) = -ER + FR

	    DTMP = XPR * DXPR - XPI * DXPI
	    XPI  = XPR * DXPI + XPI * DXPR
	    XPR = DTMP
  280	 CONTINUE
	    CALL CMFFT ( DI, DO, NH, ISIGN )
      ELSE
	 WRITE (*,*) ' ISIGN MUST BE PLUS OR MINUS ONE '
      END IF

      RETURN
      END

C**********************************************************************
      FUNCTION NFCTR( N )
      COMMON /NFCTRPA/ NFAC, M2, M3, M4, M5, IFAC( 12 )
C
C  FUNCTION TO FIND THE SMALLEST EVEN PRODUCT OF 2'S, 3'S, 4'S AND 5'S
C  WHICH CONTAINS N.
C
C   CHECK FOR N SMALLER THAN 5
      IF(N.LT.5) THEN
	 NFCTR = 4
	 NFAC = 2
	 M2 = 2
	 M3 = 0
	 M4 = 0
	 M5 = 0
	 IFAC(1) = 2
	 IFAC(2) = 2
         RETURN
      END IF
C   STRATEGY:  EXAMINE ALL POSSIBLE COMBINATIONS, PICK THE SMALLEST
C
C  EXAMINE ALL FACTORS OF TWO, BEGINNING WITH 2
      NFCTR = N*2
      N2 = 1
      M2 = 0
      M4 = 0
  200 N2 = N2 * 2
      M2 = M2 + 1
C  FOR A GIVEN NUMBER OF TWO'S, EXAMINE ALL POWERS OF THREE,
C  STARTING WITH 3^0.
	 N23 = N2
	 M3 = 0
	 IF (N23.GE.N) GOTO 600

C  FOR A GIVEN NUMBER OF TWO'S AND THREE'S, EXAMINE ALL POWERS OF FIVE,
C  STARTING WITH 5^0.
  300	    N235 = N23
	       M5 = 0
  500	       N235 = N235 * 5
	       M5 = M5 + 1
               IF(N235.LT.N) GOTO 500
	    IF(N235.LT.NFCTR) THEN
	       NFCTR = N235
	       M2R = M2
	       M3R = M3
	       M5R = M5
	    END IF
	    N23 = N23 * 3
	    M3 = M3 + 1
	    M5 = 0
         IF (N23.LT.N) GOTO 300
  600 IF (N23.LT.NFCTR) THEN
	 NFCTR = N23
	 M2R = M2
	 M3R = M3
	 M5R = M5
      END IF
      IF(N2.LT.N) GOTO 200
      M4 = M2R/2
      M2 = M2R	- M4*2
      M3 = M3R
      M5 = M5R
      NFAC = M2 + M3 + M4 + M5
      DO 680 J = 1, NFAC
      IF ( M5 .GE. J ) THEN
	 IFAC(J) = 5
      ELSEIF ( M4 + M5 .GE. J ) THEN
	 IFAC(J) = 4
      ELSEIF ( M3 + M4 + M5 .GE. J ) THEN
	 IFAC(J) = 3
      ELSEIF ( M2 + M3 + M4 + M5 .GE. J ) THEN
	 IFAC(J) = 2
      ELSE
	 IFAC(J) = 0
      END IF
  680 CONTINUE
      RETURN
      END
