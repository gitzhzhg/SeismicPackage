C\USER DOC
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
C-----------------------------------------------------------------------
C		    CONOCO PROCESSING SYSTEM                           
C	      EXPLORATION RESEARCH & SERVICES DIVISION                 
C			   CONOCO, INC.                                
C								       
C    Primitive name:  RCPXFRF*
C  Source directory:  tfio
C      Last revised:  99/04/29       Day
C           Purpose:  Fortran wrappers to rcpxfr.c routines
C
C  Related Documentation:
C-----------------------------------------------------------------------
C     SUBROUTINE RCPXFRF(LFILE,RFILE,RNODE,RUSER,DIR,I_ERR)
C       - transfers file rfile on node rnode to/from lfile using
C         the user id ruser.
C
C     INTEGER FUNCTION RCPXFRF_NET(LFILE,NNAME,LOCAL,DIR)
C       - transfers file NNAME to/from LFILE, returns OK=0
C         Also sets LOCAL=1/0 to indicate if NNAME points/does not
C         point to the local node.(no transfer if LOCAL=0)
C         
C     INTEGER FUNCTION RCPXFRF_NET_CPS(LFILE,NNAME,LOCAL,DIR)
C       - Same as rcpxfrf_net but augments any missing info
C         in NNAME from _netinfo(if it exists).
C
C RNODE  A remote host name(e.g. poepsn03)  or 'NONE'
C RUSER  A remote userid name or 'NONE'
C RFILE  A file name on the remote host RNODE
C NNAME  A network file name.
C        e.g. RNODE::RUSER;;RFILE or RNODE::RFILE or  RUSER@RNODE:RFILE
C LFILE  A file name on the local host.
C LOCAL  A flag indicating if NNAME points to the current node.
C        LOCAL=0 if NNAME is not on the local node.
C DIR    Controls direction of transfer
C            0 sequential get of rfile from rnode.
C            1 sequential put of rfile to rnode.
C            2 asynchronous get of rfile from rnode.
C            3 asynchronous put of rfile to rnode.
C I_ERR  Returns as NOT OK=-1, OK=0 
C-----------------------------------------------------------------------
C                                 NOTES
C
C  1. 
C-----------------------------------------------------------------------
C                           REVISION HISTORY
C
C     Date      Author     Description
C     ----      ------     -----------
C 2. 99/05/18   R.Day      Added RCPXFRF_NET_CPS, and altered arguments
C                          of RCPXFRF_NET
C 1. 99/04/29   R.Day      Original version
C-----------------------------------------------------------------------
C\END DOC
C
      SUBROUTINE RCPXFRF(LFILE,RFILE,RNODE,RUSER,DIR,I_ERR)
      IMPLICIT NONE
      CHARACTER*(*) LFILE,RFILE,RNODE,RUSER
      INTEGER       INODE(20),IUSER(20),IRFILE(40),ILFILE(40)
      INTEGER       DIR,I_ERR
      CALL CONVERT_CC2HH(RNODE,0,INODE,-20)
      CALL CONVERT_CC2HH(RUSER,0,IUSER,-20)
      CALL CONVERT_CC2HH(LFILE,0,ILFILE,-40)
      CALL CONVERT_CC2HH(RFILE,0,IRFILE,-40)
      CALL RCPXFR(ILFILE,IRFILE,INODE,IUSER,DIR,I_ERR)
      RETURN
      END
CCC
      INTEGER FUNCTION RCPXFRF_NET(LFILE,NETNAME,LOCAL,DIR)
      IMPLICIT NONE
      CHARACTER*(*) LFILE,NETNAME
      INTEGER       INET(40),ILFILE(40),LOCAL
      INTEGER       DIR,RCPXFR_NET
      CALL CONVERT_CC2HH(NETNAME,0,INET,-40)
      CALL CONVERT_CC2HH(LFILE,0,ILFILE,-40)
      RCPXFRF_NET = RCPXFR_NET(ILFILE,INET,LOCAL,DIR)
      CALL CONVERT_HH2CC(ILFILE,0,LFILE,0)
      RETURN
      END
CCC
      INTEGER FUNCTION RCPXFRF_NET_CPS(LFILE,NETNAME,LOCAL,DIR)
      IMPLICIT NONE
      CHARACTER*(*) LFILE,NETNAME
      INTEGER       INET(40),ILFILE(40),LOCAL
      INTEGER       DIR,RCPXFR_NET_CPS
      CALL CONVERT_CC2HH(NETNAME,0,INET,-40)
      CALL CONVERT_CC2HH(LFILE,0,ILFILE,-40)
      RCPXFRF_NET_CPS = RCPXFR_NET_CPS(ILFILE,INET,LOCAL,DIR)
      CALL CONVERT_HH2CC(ILFILE,0,LFILE,0)
      RETURN
      END
CCC
