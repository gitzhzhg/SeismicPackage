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
C  Process name:  NUMVAL_OF_ALFA
C        Author:  Bob Baumel
C  Last revised:  89/01/05
C  Revisions:
C   4. 89/01/05; B Baumel; Remove 2nd argument from REALVAL_OF_ALFA.
C   3. 88/06/29; B Baumel; Replace original logic by calls to ALF2NUM.
C   2. 87/08/11; B Baumel; Change INTVAL reformatting to left-justified.
C   1. 87/01/13; B Baumel; Original working version.
C
C  Purpose: A pair of VAX Fortran FUNCTION subprograms for obtaining
C           the numeric (real or integer) values of alphanumeric fields.
C           This duplicates the function of the newer ALF2NUM routines
C           (and, in fact, internally calls ALF2NUM).  The present
C           routines have been retained because many existing modules
C           call them, and because they are sometimes more convenient to
C           use -- because they are FUNCTIONs, and let you avoid use of
C           statement numbers. (Error conditions are handled by special
C           returned values rather than by alternate returns.)
C-----------------------------------------------------------------------
C                           CALLING SEQUENCE
C                                                                      
C   This routine has two entry points, one for real values and the
C   other for integer values.  Both are used as FUNCTION subprograms:
C                                        
C               avar  =  REALVAL_OF_ALFA (ALFSTR)
C               ivar  =  INTVAL_OF_ALFA (ALFSTR)
C              
C   ALFSTR  is a character variable containing the alphanumeric string
C           whose numeric value is desired.
C   avar    is a real variable to receive the (real) value of ALFSTR.
C   ivar    is an integer variable to receive the (integer) value of
C           string ALFSTR.
C-----------------------------------------------------------------------
C                                 NOTES
C
C 1. This routine (with either entry) does two things:
C    a) It determines the numeric value of ALFSTR and returns this value
C       in the appropriate variable (avar or ivar).
C    b) It re-writes the string ALFSTR left-justified, with all embedded
C       blanks removed.
C
C 2. The function returns special values in the following circumstances:
C    a) If the input field ALFSTR is completely blank, then it REMAINS
C       blank, and:
C          REALVAL_OF_ALFA  returns the value  -1.E6
C          INTVAL_OF_ALFA  returns the value  -1000000
C    b) If a conversion error occurs then:
C          REALVAL_OF_ALFA  returns the value  -1.E7
C          INTVAL_OF_ALFA  returns the value  -10000000
C
C 3. These routines may be used in conjunction with EZED 'N' format,
C    which produces a field that requires numeric input, but is assigned
C    a CHARACTER variable.  Such a field can have a BLANK value, which
C    will not be interpreted as equal to zero.  See Chuck Burch for more
C    information on 'N' format.
C
C 4. Special care is needed when using these functions with (character-
C    valued) EZED arrays.  It's a good idea NOT to define local numeric
C    arrays paralleling the EZED-generated character arrays.  That's
C    because the user may make use of certain EZED editing operations,
C    such as  ~D, ~I, and ~C,  which will alter the values in the EZED
C    character arrays, but will NOT similarly alter the values in your
C    parallel numeric arrays.  The safest approach is to maintain ONLY
C    the EZED character arrays; then reference the present functions
C    each time you need to know the numeric values in those arrays.
C
C 5. For compatibility with previous versions of this routine, the
C    REALVAL_OF_ALFA function allows you to specify a second argument,
C    but the value of that second argument is ignored.
C-----------------------------------------------------------------------
C\END DOC
      FUNCTION REALVAL_OF_ALFA (ALFSTR,FMT)
      PARAMETER ( BLANKVAL=-1.E6,    ERRVAL=-1.E7,
     *           IBLANKVAL=-1000000,IERRVAL=-10000000)
      CHARACTER*(*) ALFSTR
      CALL ALF2NUMR (ALFSTR,0,VALUE,*1,*2)
      REALVAL_OF_ALFA = VALUE
      RETURN
   1  REALVAL_OF_ALFA = ERRVAL
      RETURN
   2  REALVAL_OF_ALFA = BLANKVAL
      RETURN
C***********************************************************************
      ENTRY INTVAL_OF_ALFA (ALFSTR)
      CALL ALF2NUMI (ALFSTR,0,IVALUE,*3,*4)
      INTVAL_OF_ALFA = IVALUE
      RETURN
   3  INTVAL_OF_ALFA = IERRVAL
      RETURN
   4  INTVAL_OF_ALFA = IBLANKVAL
      RETURN
      END
