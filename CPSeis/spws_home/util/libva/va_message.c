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

/*----------------------- va_message.c ---------------------------------*/


/*----------------------- header files ---------------------------------*/

#include "va_message.h"


  
/*---------- fortran-callable routine to broadcast a message -----------*/
 
void fbroadcast_(void **vel2, long *sender, long *message,
           long *one, long *two, long *three, long *four, long *five)
{
  void *vel;
  vel = (void*)*vel2;
  broadcast(vel, *sender, *message, *one, *two, *three, *four, *five);
}



/*----------------- routine to broadcast a message ---------------------*/

void broadcast(void *vel, long sender, long message,
           long one, long two, long three, long four, long five)
{
/*
  char sss[20], mmm[20];
  if     (sender == SENDER_WBOX) strcpy(sss, "SENDER_WBOX");
  else if(sender == SENDER_SEMB) strcpy(sss, "SENDER_SEMB");
  else if(sender == SENDER_CMP ) strcpy(sss, "SENDER_CMP ");
  else if(sender == SENDER_GVS ) strcpy(sss, "SENDER_GVS ");
  else if(sender == SENDER_ISO ) strcpy(sss, "SENDER_ISO ");
  else                           strcpy(sss, "SENDER_????");
  if     (message == MESSAGE_NEWFILE) strcpy(mmm, "MESSAGE_NEWFILE");
  else if(message == MESSAGE_NUMBER ) strcpy(mmm, "MESSAGE_NUMBER ");
  else if(message == MESSAGE_SORT   ) strcpy(mmm, "MESSAGE_SORT   ");
  else if(message == MESSAGE_MODIFY ) strcpy(mmm, "MESSAGE_MODIFY ");
  else if(message == MESSAGE_INSERT ) strcpy(mmm, "MESSAGE_INSERT ");
  else if(message == MESSAGE_REMOVE ) strcpy(mmm, "MESSAGE_REMOVE ");
  else if(message == MESSAGE_ACTIVE ) strcpy(mmm, "MESSAGE_ACTIVE ");
  else if(message == MESSAGE_XHEAD  ) strcpy(mmm, "MESSAGE_XHEAD  ");
  else if(message == MESSAGE_YHEAD  ) strcpy(mmm, "MESSAGE_YHEAD  ");
  else if(message == MESSAGE_ORDER  ) strcpy(mmm, "MESSAGE_ORDER  ");
  else                                strcpy(mmm, "MESSAGE_???????");
  printf ("%d %s   %d %s   %d %d %d %d %d\n",
           sender, sss, message, mmm, one, two, three, four, five);
*/

/*
  message_semb(vel, sender, message, one, two, three, four, five);
  message_cmp (vel, sender, message, one, two, three, four, five);
  message_gvs (vel, sender, message, one, two, three, four, five);
*/
  message_iso (vel, sender, message, one, two, three, four, five);
  message_wbox_(&sender, &message, &one, &two, &three, &four, &five);
}
 


/*--------------------------- end --------------------------------------*/
