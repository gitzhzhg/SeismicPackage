      SUBROUTINE BHIST_DUMMY ()
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
      ENTRY BHIST_GET_CHARGE (VAL)
      ENTRY BHIST_GET_USERID (VAL)
      ENTRY BHIST_GET_JOBTIME (VAL)
      ENTRY BHIST_GET_JOBDATE (VAL)
      ENTRY BHIST_GET_HISTNEW (VAL)
      ENTRY BHIST_GET_HISTOLD (VAL)
      ENTRY BHIST_PARIO_SET (PARIO)
      ENTRY PHIST_SETPAR (PRN, FILE)
      END
      INTEGER FUNCTION BHIST_IFLGBH ()
      END
      INTEGER FUNCTION BHIST_NNTOLD ()
      END
      INTEGER FUNCTION BHIST_NPROLD ()
      END
      INTEGER FUNCTION BHIST_NHROLD ()
      END
      INTEGER FUNCTION BHIST_NHRNEW ()
      END
      INTEGER FUNCTION MPI_INITIALIZED ()
      END
      INTEGER FUNCTION BHIST_MEM ()
      END
