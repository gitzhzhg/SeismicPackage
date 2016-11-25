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
#ifndef IPC_PACKET_HH
#define IPC_PACKET_HH

// To pack and unpack packets containing front-end lengths and or protocols

class IpcPacket {

public:
  IpcPacket ();                       /* Constructor                       */

  ~IpcPacket ();                      /* Destructor                        */

  int makeWithProtocol                /* mk packet front-ended with proto  */
    (char *protocol,                  /*   given protocol                  */
     char *data,                      /*   given input data                */
     char *packet,                    /*   populated with data             */
     int max_packet_size);            /*   given maximum packet size       */

  int makeWithLength                  /* make packet front-ended with leng */
    (char *data,                      /*   given input data                */
     char *packet,                    /*   populated with data             */
     int max_packet_size);            /*   given maximum packet size       */

  int parseWithProtocol               /* splt packet front-ended with proto*/
    (char *protocol,                  /*   given protocol                  */
     char *packet,                    /*   given input packet              */
     char *data);                     /*   populated with data from packet */

  char *protocolFound                 /* rtn ptr after protocol else NULL  */
    (char *protocol,                  /*   given protocol                  */
     char *packet);                   /*   given input packet              */

  int parseWithLength                 /* splt packet front-ended with leng */
    (char *packet,                    /*   given input packet              */
     char *data);                    /*   populated with data from packet */

  static int decimalStrLen            /* rtn string length to hold integer */
    (int number);                     /*   given integer number            */
};

#endif
