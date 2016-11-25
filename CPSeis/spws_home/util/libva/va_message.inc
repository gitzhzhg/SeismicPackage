


ccccccccccccc   va_message.inc   ccccccccccccccc


cccc   the defined parameters must exactly match corresponding constants
cccc   in the C header file va_message.h



c------------defined parameters for sender.

      integer    sender_wbox, sender_semb, sender_cmp
      integer    sender_gvs , sender_iso

      parameter (SENDER_WBOX=1)  ! sender of message is a windowbox popup.
      parameter (SENDER_SEMB=2)  ! sender of message is semblance window.
      parameter (SENDER_CMP =3)  ! sender of message is cmp window.
      parameter (SENDER_GVS =4)  ! sender of message is gvs window.
      parameter (SENDER_ISO =5)  ! sender of message is iso-velocity window.
   


c------------defined parameters for message.


      integer    MESSAGE_NEWFILE, MESSAGE_NUMBER , MESSAGE_SORT 
      integer    MESSAGE_MODIFY , MESSAGE_INSERT , MESSAGE_REMOVE
      integer    MESSAGE_ACTIVE , MESSAGE_XHEAD  , MESSAGE_YHEAD
      integer    MESSAGE_XBIN   , MESSAGE_YBIN   , MESSAGE_TOL
      integer    MESSAGE_ORDER
 
      parameter (MESSAGE_NEWFILE=1)  ! new velocity file (or none)
      parameter (MESSAGE_NUMBER =2)  ! number of velocity functions changed
      parameter (MESSAGE_SORT   =3)  ! velocity functions sorted
      parameter (MESSAGE_MODIFY =4)  ! velocity functions modified
      parameter (MESSAGE_INSERT =5)  ! one velocity function inserted
      parameter (MESSAGE_REMOVE =6)  ! one velocity function removed
      parameter (MESSAGE_ACTIVE =7)  ! switched to different active function
      parameter (MESSAGE_XHEAD  =8)  ! changed X header word number
      parameter (MESSAGE_YHEAD  =9)  ! changed Y header word number
      parameter (MESSAGE_XBIN  =10)  ! changed X bin coordinate 
      parameter (MESSAGE_YBIN  =11)  ! changed Y bin coordinate 
      parameter (MESSAGE_TOL   =12)  ! changed bin tolerances (center,width)
      parameter (MESSAGE_ORDER =13)  ! changed NMO order
 



c------------------------end.

