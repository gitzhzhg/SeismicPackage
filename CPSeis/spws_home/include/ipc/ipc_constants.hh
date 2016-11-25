/*<license>
 -------------------------------------------------------------------------------
  Copyright (c) 2007 ConocoPhillips Company
 
  Permission is hereby granted, free of charge, to any person obtaining a copy
  of this software and associated documentation files (the "Software"), to deal
  in the Software without restriction, including without limitation the rights
  to use, copy, modify, merge, publish, distribute, sublicense, and/or sell
  copies of the Software, and to permit persons to whom the Software is
  furnished to do so, subject to the following conditions:
 
  The above copyright notice and this permission notice shall be included in all
  copies or substantial portions of the Software.
 
  THE SOFTWARE IS PROVIDED "AS IS", WITHOUT WARRANTY OF ANY KIND, EXPRESS OR
  IMPLIED, INCLUDING BUT NOT LIMITED TO THE WARRANTIES OF MERCHANTABILITY,
  FITNESS FOR A PARTICULAR PURPOSE AND NONINFRINGEMENT. IN NO EVENT SHALL THE
  AUTHORS OR COPYRIGHT HOLDERS BE LIABLE FOR ANY CLAIM, DAMAGES OR OTHER
  LIABILITY, WHETHER IN AN ACTION OF CONTRACT, TORT OR OTHERWISE, ARISING FROM,
  OUT OF OR IN CONNECTION WITH THE SOFTWARE OR THE USE OR OTHER DEALINGS IN THE
  SOFTWARE.
 -------------------------------------------------------------------------------
 </license>*/
#ifndef IPC_CONSTANTS_HH
#define IPC_CONSTANTS_HH

#include <sys/socket.h>
#include <netinet/in.h>

#include <assert.h>
#include <ctype.h>
#include <string.h>

class IPC {

public:

  enum {
    UPPER,                             /* upper case name                  */
    LOWER,                             /* lower case name                  */
    MIXED,                             /* mixed case name                  */
    DESCRIPTION                        /* description of application       */
  };

  enum {
    APPL_NOT_SET = -1,                 /* application instance not set     */
    CBYT,                              /* cbyt                             */
    VA,                                /* va                               */
    CSV,                               /* csv                              */
    CFG,                               /* cfg                              */
    GEOPRESS,                          /* geopress                         */
    CPS,                               /* cps engine                       */
    CFE,                               /* cfe                              */
    JOBMON,                            /* jobmon                           */
    ANY_APPL                           /* any application will do          */
  };

  enum {
    _PROTOCOL = 0,                     /* use TCP/IP protocol              */
    _DOMAIN = AF_INET,                 /* use the internet network         */
    _TYPE = SOCK_STREAM,               /* use bidirectional stream'n sockt */
    MAX_SOCKET_BUF = 2048              /* socket maximum buffer size       */
  };

  enum {
    _SERVER,                           /* server flag                      */
    _CLIENT                            /* client flag                      */
  };

  enum {
    UNSUPPORTED_PORT = -1,             /* port number not set yet          */
    MAX_ALLOWED_PORTS = 256,           /* maximum ports allowed            */
    PORT_NUMBER_OFFSET = 5287,         /* offset into valid port numbers   */
    MAX_HOSTNAME_LEN = 120             /* maximum length of a hostname     */
  };

// trace header numbers as per CPS
  enum {
    HDR_SIZE = 64
  };

  enum {
    HDR_TRACE = 1,
    HDR_TOP_MUTE,
    HDR_CURRENT_GROUP,
    HDR_CURRENT_CHANNEL,
    HDR_FOLD,
    HDR_OFFSET,
    HDR_MIDPOINT_XGRID,
    HDR_MIDPOINT_YGRID,
    HDR_ORIGINAL_GROUP,
    HDR_ORIGINAL_CHANNEL,
    HDR_SOURCE_XLOC,
    HDR_SOURCE_YLOC,
    HDR_SOURCE_ELEV,
    HDR_RECEIVER_XLOC,
    HDR_RECEIVER_YLOC,
    HDR_RECEIVER_ELEV,
    HDR_MIDPOINT_XLOC,
    HDR_MIDPOINT_YLOC,
    HDR_MIDPOINT_ELEV,
    HDR_SOURCE_DEPTH,
    HDR_RECEIVER_DEPTH,
    HDR_SOURCE_COMPONENT,
    HDR_RECEIVER_COMPONENT,
    HDR_PANEL,
    HDR_LAV,
    HDR_SOURCE_LINE,
    HDR_RECEIVER_LINE,
    HDR_RECEIVER_SHOTPOINT,
    HDR_SOURCE_SHOTPOINT,
    HDR_SCRATCH_30,
    HDR_SCRATCH_31,
    HDR_SCRATCH_32,
    HDR_SOURCE_XGRID,
    HDR_SOURCE_YGRID,
    HDR_RECEIVER_XGRID,
    HDR_RECEIVER_YGRID,
    HDR_MIDPOINT_SHOTPOINT,
    HDR_MIDPOINT_LINE,
    HDR_PRE,
    HDR_POST,
    HDR_CUM_DATUM_STATIC,
    HDR_CUM_REFR_STATIC,
    HDR_CUM_RESID_STATIC,
    HDR_SOURCE_UPTIME,
    HDR_RECEIVER_UPTIME,
    HDR_SOURCE_GP,
    HDR_RECEIVER_GP,
    HDR_USER_48,
    HDR_USER_49,
    HDR_USER_50,
    HDR_USER_51,
    HDR_USER_52,
    HDR_USER_53,
    HDR_USER_54,
    HDR_USER_55,
    HDR_RPRE,
    HDR_RPOST,
    HDR_SCRATCH_58,
    HDR_SCRATCH_59,
    HDR_SCRATCH_60,
    HDR_SCRATCH_61,
    HDR_SCRATCH_62,
    HDR_GVS_MODIFIER,
    HDR_BOTTOM_MUTE
  };

  static int verifyApplication         /* return 0 if application invalid  */
    (int id);                          /*   id of appliation               */

  static int applIdFromString          /* return application id given      */
    (char *appl);                      /*   name of application            */

  static char *stringFromApplId        /* return appl name or description  */
    (int id,                           /*   id of application              */
     int which = LOWER);               /*   desired string: case or descr  */

  static int strcmpCaseInsensitive (const char *dis, const char *dat);

  static int numApplications ();       /* return number of applications    */

};

#endif
