C       SUBROUTINE LIB$SPAWN(COMD,P1,P2,P3,P4,P5,P6,P7,P8,P9,P10,P11)
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
C        CHARACTER*(*) COMD,P1,P2,P3,P4,P5,P6,P7,P8,P9,P10,P11
C        write(*,*) 'lib$spawn is spawning'
C        write(*,*) COMD
C        RETURN
C        END
C
C
C
        SUBROUTINE LIB_GET_LUN( ILUN)
        INTEGER ILUN
        ILUN= 73
        RETURN
        END
C
C
C
        SUBROUTINE LIB_FREE_LUN( ILUN)
        INTEGER ILUN
        CLOSE(ILUN)
        RETURN
        END
C
C
C
        SUBROUTINE STR_TRANSLATE( OUTPUT, INPUT, IN, OUT)
        CHARACTER*(*) OUTPUT, INPUT, IN, OUT
        INTEGER LO,LI,L,I 
        LO = LEN( OUTPUT)
        LI = LEN( INPUT)
        L=LO
        IF (LI.LT.LO) L=LI
        DO 100 I=1,L
           IF( INPUT(I:I).EQ.IN(1:1)) THEN
              OUTPUT(I:I)=OUT(1:1)
           ELSE
              OUTPUT(I:I)=INPUT(I:I)
           ENDIF
 100     CONTINUE
        RETURN
        END
C
C
C
        SUBROUTINE DELETE_FILE( FILE_NAME)
        CHARACTER*(*) FILE_NAME
        LOGICAL FILE_EXIST
        INQUIRE(FILE=FILE_NAME,EXIST=FILE_EXIST,ERR=100)
        IF (FILE_EXIST) THEN
           OPEN(73, FILE=FILE_NAME, STATUS='OLD',ERR=100)
           CLOSE(73,STATUS='DELETE',ERR=100)
        ENDIF
 100     CONTINUE
        RETURN
        END
