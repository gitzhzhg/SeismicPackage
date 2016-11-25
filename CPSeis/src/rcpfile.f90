!<CPS_v1 type="PRIMITIVE"/>
!!------------------------------ rcpfile.f90 --------------------------------!!
!!------------------------------ rcpfile.f90 --------------------------------!!
!!------------------------------ rcpfile.f90 --------------------------------!!


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
! Name       : rcpfile 
! Category   : io
! Written    : 1999-07-01   by: Donna K. Vunderink
! Revised    : 2000-10-06   by: Tom Stoeckley
! Maturity   : production   2000-10-19
! Purpose    : Transfer a disk file between a local and remote node.
! Portability: No known limitations.
!
!-------------------------------------------------------------------------------
!</brief_doc>


!<descript_doc>
!-------------------------------------------------------------------------------
!                         GENERAL DESCRIPTION                 
!
! Use rcp or cp to transfer a disk file from a local node to a remote node or
! vice-versa.  The file is always copied, even if it is already on the local
! node.
!
! The format of a file name on a remote node is like this:
!
!                      userid@node:dir/file
!
!       where   userid = user ID on the remote node.
!                 node = name of the remote node.
!             dir/file = path to the file on the remote node.
!
! The file is on the local node if the NODE refers to the local node, or
! if the USERID and NODE are missing.
!
! This primitive uses the PATH primitive to parse the file name.
!
!-------------------------------------------------------------------------------
!</descript_doc>


!<calling_doc>
!-------------------------------------------------------------------------------
!                      INPUT AND OUTPUT ARGUMENTS             
!
!
! For each subroutine or function documented below, each argument is
! flagged as follows:
!      i = value required upon INPUT.
!      o = value set by the routine upon OUTPUT.
!      b = value BOTH required upon input and changed upon output.
!
!-------------------------------------------------------------------------------
!                          CALLING SEQUENCE                    
!
! Fetch or dispose a file, always doing a copy:
!
!                                                          opt   opt    opt
!                              i          i         i       o     o      o
!    call rcpfile         (localfile, remotefile, direct, istat, msg, command)
!
!    call rcpfile_fetch   (localfile, remotefile,         istat, msg, command)
!    call rcpfile_dispose (localfile, remotefile,         istat, msg, command)
!                              i          i                 o     o      o
!                                                                       opt
!
!
! Fetch or dispose a file, doing a copy only when necessary:
!
!                                                                    opt
!                                   i           o        o     o      o
!    call rcpfile_pre_fetch    (remotefile, localfile, istat, msg, command)
!    call rcpfile_pre_dispose  (remotefile, localfile, istat, msg, command)
!
!    call rcpfile_post_fetch   (remotefile, localfile, istat, msg, command)
!    call rcpfile_post_dispose (remotefile, localfile, istat, msg, command)
!                                   i           i        o     o      o
!                                                                    opt
!
!
! character(len=*)  localfile = local file name.
! character(len=*) remotefile = remote file name.
! character(len=3)     direct = direction of transfer.
! integer               istat = status (result of the action taken).
! character(len=*)        msg = message (for possible printing).
! character(len=*)    command = shell command executed (for possible printing).
!
!       direct = 'GET' to get (fetch) the file from the remote node.
!       direct = 'PUT' to put (dispose) the file to the remote node.
!
!        istat =  0 = RCPFILE_OK if the action was successful.
!        istat = -1 = RCPFILE_ERROR if an error occurred.
!
! LOCALFILE is the name of a temporary file on the local node.
! REMOTEFILE is the name of the permanent file on the local node or a remote
! node.  REMOTEFILE can optionally contain the user ID and node name, which
! may refer to the local node or a remote node.  If the node name is absent,
! REMOTEFILE obviously refers to a file on the local node.
!
!-------------------------------------------------------------------------------
!             FETCH OR DISPOSE A FILE, ALWAYS DOING A COPY
!                              DETAILS
!
! The first three routines above always do a copy even if the file is on
! the local node.  When fetching a file, REMOTEFILE is copied to LOCALFILE
! in preparation for reading LOCALFILE.  When disposing a file, LOCALFILE
! is copied to REMOTEFILE after LOCALFILE is written.  In both cases, it is
! the user's responsibility to remove LOCALFILE when it is no longer needed.
!
! The routine RCPFILE is redundant but is retained for backward compatibility.
! RCPFILE simply calls RCPFILE_FETCH   if DIRECT == 'GET'.
! RCPFILE simply calls RCPFILE_DISPOSE if DIRECT == 'PUT'.
! RCPFILE returns istat == RCPFILE_ERROR if DIRECT is anything else.
!
!-------------------------------------------------------------------------------
!       FETCH OR DISPOSE A FILE, DOING A COPY ONLY WHEN NECESSARY
!                              DETAILS
!
! The last four routines above do a copy only if the file is on a remote
! node.  The "pre" routines must be called before reading or writing
! LOCALFILE, and the "post" routines must be called after reading or writing
! LOCALFILE.  If REMOTEFILE is on the local node (even if it contains the
! node name), no copying will occur and LOCALNAME will point to the same
! file as REMOTEFILE.  Note that LOCALFILE is returned rather than input
! by the "pre" routines.
!
! If REMOTEFILE is on a remote node:
!  (1) RCPFILE_PRE_FETCH will make up a unique non-existing name for LOCALFILE
!        and copy REMOTEFILE to LOCALFILE in preparation for reading.
!  (2) RCPFILE_POST_FETCH will remove LOCALFILE.
!  (3) RCPFILE_PRE_DISPOSE will make up a unique non-existing name for
!        LOCALFILE in preparation for writing.
!  (4) RCPFILE_POST_DISPOSE will copy LOCALFILE to REMOTEFILE and then remove
!        LOCALFILE.
!
! If REMOTEFILE is on the local node:
!  (1) RCPFILE_PRE_FETCH will set LOCALFILE to point to the same existing
!        file as REMOTEFILE in preparation for reading.
!  (2) RCPFILE_POST_FETCH will do nothing.
!  (3) RCPFILE_PRE_DISPOSE will set LOCALFILE to point to the same (possibly
!        non-existent) file as REMOTEFILE in preparation for writing.
!  (4) RCPFILE_POST_DISPOSE will do nothing.
!
!-------------------------------------------------------------------------------
!</calling_doc>


!<history_doc>
!-------------------------------------------------------------------------------
!                           REVISION HISTORY                   
!
!     Date        Author       Description
!     ----        ------       -----------
!  5. 2000-10-19  Stoeckley    Add required missing documentation section.
!  4. 2000-02-04  Stoeckley    Make minor documentation corrections; make sure
!                               optional argument COMMAND is always set if
!                               present.
!  3. 2000-01-07  Stoeckley    Remove calls to the parameter cache; add
!                               calls to the PATH primitive to parse the file
!                               name; split the single public subroutine into
!                               fetch and dispose subroutines.
!  2. 1999-10-22  Vunderink    No longer lowercase filename.
!  1. 1999-07-01  Vunderink    Initial version.
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


!<programming_doc>
!-------------------------------------------------------------------------------
!                           PROGRAMMING NOTES
!
! (1) Donna's original RCPFILE code is retained in this primitive (commented
!     out) for reference.
!
!-------------------------------------------------------------------------------
!</programming_doc>


!!-------------------------- start of module ------------------------------!!
!!-------------------------- start of module ------------------------------!!
!!-------------------------- start of module ------------------------------!!


      module rcpfile_module

      use getsys_module
      use putsys_module
      use path_module
      use cio_module
      use string_module
      use finquire_module
      use exptilde_module
      use tempname_module

      implicit none

      private
      public :: rcpfile
      public :: rcpfile_fetch
      public :: rcpfile_dispose
      public :: rcpfile_pre_fetch
      public :: rcpfile_post_fetch
      public :: rcpfile_pre_dispose
      public :: rcpfile_post_dispose

      integer,public,parameter :: RCPFILE_OK     =  0
      integer,public,parameter :: RCPFILE_ERROR  = -1

      contains


!!-------------------------------- rcpfile ---------------------------------!!
!!-------------------------------- rcpfile ---------------------------------!!
!!-------------------------------- rcpfile ---------------------------------!!


      subroutine rcpfile (localfile,remotefile,direct,istat,msg,command)
      implicit none
      character(len=*),intent(in)           :: localfile      ! argument
      character(len=*),intent(in)           :: remotefile     ! argument
      character(len=*),intent(in)           :: direct         ! argument
      integer         ,intent(out),optional :: istat          ! argument
      character(len=*),intent(out),optional :: msg            ! argument
      character(len=*),intent(out),optional :: command        ! argument
      integer                               :: istat2         ! local
      character(len=80)                     :: msg2           ! local

      select case (direct)
        case ('GET')
           call rcpfile_fetch   (localfile,remotefile,istat2,msg2,command)
        case ('PUT')
           call rcpfile_dispose (localfile,remotefile,istat2,msg2,command)
        case default
           istat2 = RCPFILE_ERROR
           msg2   = 'invalid direction for RCPFILE transfer'
           if (present(command)) command = 'no action taken'
      end select
      if (present(istat)) istat = istat2
      if (present(msg  )) msg   = msg2
      return
      end subroutine rcpfile


!!--------------------------- rcpfile fetch --------------------------------!!
!!--------------------------- rcpfile fetch --------------------------------!!
!!--------------------------- rcpfile fetch --------------------------------!!


      subroutine rcpfile_fetch (localfile,remotefile,istat,msg,command)
      implicit none
      character(len=*),intent(in)           :: localfile         ! argument
      character(len=*),intent(in)           :: remotefile        ! argument
      integer         ,intent(out)          :: istat             ! argument
      character(len=*),intent(out)          :: msg               ! argument
      character(len=*),intent(out),optional :: command           ! argument
      character(len=200)                    :: copy,remotefile2  ! local

      if (present(command)) command = ' '
      call rcpfile_private_status (remotefile,istat,msg,copy,remotefile2)
      if (istat == RCPFILE_ERROR) return
      if(copy == 'cp') call exptilde (remotefile2)
      call rcpfile_private_fetch  &
                         (copy,remotefile2,localfile,istat,msg,command)
      return
      end subroutine rcpfile_fetch


!!-------------------------- rcpfile dispose -------------------------------!!
!!-------------------------- rcpfile dispose -------------------------------!!
!!-------------------------- rcpfile dispose -------------------------------!!


      subroutine rcpfile_dispose (localfile,remotefile,istat,msg,command)
      implicit none
      character(len=*),intent(in)           :: localfile         ! argument
      character(len=*),intent(in)           :: remotefile        ! argument
      integer         ,intent(out)          :: istat             ! argument
      character(len=*),intent(out)          :: msg               ! argument
      character(len=*),intent(out),optional :: command           ! argument
      character(len=200)                    :: copy,remotefile2  ! local

      if (present(command)) command = ' '
      call rcpfile_private_status (remotefile,istat,msg,copy,remotefile2)
      if (istat == RCPFILE_ERROR) return
      if(copy == 'cp') call exptilde (remotefile2)
      call rcpfile_private_dispose &
                          (copy,localfile,remotefile2,istat,msg,command)
      return
      end subroutine rcpfile_dispose


!!--------------------- rcpfile private status ---------------------------!!
!!--------------------- rcpfile private status ---------------------------!!
!!--------------------- rcpfile private status ---------------------------!!

! This subroutine does not test for the actual existence or accessibility
! of any file.

!     condition                       copy    remotefile2
!     ---------                      -------  -----------
!     remotefile is on remote node    'rcp'   remotefile
!     remotefile is on local node     'cp'    local path to remotefile
!     remotefile is not valid         ' '     remotefile
!     remotefile is on local node,    'cp'    invalid local path to remotefile
!       but error made in local path


      subroutine rcpfile_private_status (remotefile,istat,msg,copy,remotefile2)
      implicit none
      character(len=*),intent(in)  :: remotefile                ! argument
      integer         ,intent(out) :: istat                     ! argument
      character(len=*),intent(out) :: msg                       ! argument
      character(len=*),intent(out) :: copy                      ! argument
      character(len=*),intent(out) :: remotefile2               ! argument
      character(len=100)           :: userid,node,dir,file      ! local
      character(len=100)           :: local_userid,local_node   ! local
      character(len=6),parameter   :: NONE  = 'NONE'            ! local
      character(len=1),parameter   :: SLASH = '/'               ! local
      character(len=1),parameter   :: TILDE = '~'               ! local

!!!!!!!!!! get started:

      remotefile2 = remotefile
      call path_validate (remotefile2,istat,msg)
      if (istat /= PATH_VALID) then
           istat = RCPFILE_ERROR
           copy  = ' '
           return
      end if

!!!!!!!!!! find out whether file is on local or remote node:

      call path_parse          (remotefile,  userid,node,dir,file)
      call getsys_username     (local_userid)
      call getsys_hostname     (local_node)
      call string_strip_blanks (local_userid)
      call string_strip_blanks (local_node)

      if (node == NONE .or. node == local_node) then
           copy = 'cp'
      else
           istat = RCPFILE_OK
           msg = 'the pathname refers to a remote node'
           copy = 'rcp'
           return
      end if

!!!!!!!!!! build local path to the file:

      if (userid == NONE) userid = local_userid

      if (node == NONE) then              ! userid should also have been NONE.
           continue
      else if (dir(1:1) == SLASH) then    ! absolute directory path.
           continue
      else if (dir == NONE .or. dir == TILDE) then
           dir = TILDE//userid
      else if (dir(1:2) == TILDE//SLASH) then
           dir = TILDE//trim(userid)//dir(2:)
      else if (dir(1:1) == TILDE) then    ! tilde is followed by a userid.
           continue
      else
           dir = TILDE//trim(userid)//SLASH//dir
      end if

      userid = NONE
      node   = NONE
      call path_build (remotefile2,  userid,node,dir,file)

!!!!!!!!!! check validity of local path to the file:

      call path_validate (remotefile2,  istat,msg)
      if (istat /= PATH_VALID) then
           istat = RCPFILE_ERROR
           msg = 'unsuccessful getting valid local path to the file'
           return
      end if
      call path_parse (remotefile2,  userid,node,dir,file)
      if (userid /= NONE .or.  node /= NONE) then
           istat = RCPFILE_ERROR
           msg = 'unsuccessful getting desired local path to the file'
           return
      end if

!!!!!!!!!! finish up:

      istat = RCPFILE_OK
      msg   = 'the pathname refers to the local node'
      return
      end subroutine rcpfile_private_status


!!----------------------- Donna's original rcpfile ------------------------!!
!!----------------------- Donna's original rcpfile ------------------------!!
!!----------------------- Donna's original rcpfile ------------------------!!


!      subroutine rcpfile (lfile,rfile,direct,istat)
!
!      implicit none
!      character(len=*),intent(in)    :: lfile         ! argument
!      character(len=*),intent(in)    :: rfile         ! argument
!      character(len=*),intent(in)    :: direct        ! argument
!      integer,optional,intent(out)   :: istat         ! argument
!
!      character(len=80)              :: lnode         ! local
!      character(len=80)              :: luser         ! local
!      character(len=80)              :: rnode         ! local
!      character(len=80)              :: ruser         ! local
!      character(len=240)             :: rpath         ! local
!      character(len=240)             :: rname         ! local
!      character(len=240)             :: rtemp         ! local
!      character(len=132)             :: cmd           ! local
!      character(len=1)               :: quote         ! local
!
!      integer                        :: nlfile        ! local
!      integer                        :: nlnode        ! local
!      integer                        :: nluser        ! local
!      integer                        :: nrnode        ! local
!      integer                        :: nruser        ! local
!      integer                        :: nrpath        ! local
!      integer                        :: nrname        ! local
!      integer                        :: ncmd          ! local
!      integer                        :: i1            ! local
!      integer                        :: i2            ! local
!
!      quote = char(34)
!
!      nlfile = len_trim (lfile)
!
!      call rcpfile_parse (rfile,rnode,ruser,rname)
!      call string_strip_blanks (rnode,nrnode)
!      call string_to_lower     (rnode)
!      call string_strip_blanks (ruser,nruser)
!      call string_to_lower     (ruser)
!      call string_strip_blanks (rname,nrname)
!
!      if (rnode(1:1) .eq. ' ') then
!         call pc_get_jdata ('FRONTEND_NODE',rnode)
!         call string_strip_blanks (rnode,nrnode)
!      endif
!
!      if (ruser(1:1) .eq. ' ') then
!         call pc_get_jdata ('FRONTEND_USER',ruser)
!         call string_strip_blanks (ruser,nruser)
!      endif
!
!      i1 = index(rname(1:nrname),']')
!      i2 = index(rname(1:nrname),'/')
!      if (i1 .eq. 0 .and. i2 .eq. 0) then
!         call pc_get_jdata ('FRONTEND_PATH',rpath)
!         call string_strip_blanks (rpath,nrpath)
!         rpath = rpath(1:nrpath) // rname(1:nrname)
!         rname = rpath
!         call string_strip_blanks (rname,nrname)
!      endif
!
!      call getsys_hostname (lnode)
!      call string_strip_blanks (lnode,nlnode)
!
!      call getsys_username (luser)
!      call string_strip_blanks (luser,nluser)
!
!      if (lnode(1:nlnode) .eq. rnode(1:nrnode) .and.       &
!          luser(1:nluser) .eq. ruser(1:nruser) ) then
!         if (rname(1:1) .ne. '/') then
!           rtemp = rname
!           rname = '$HOME/' // rtemp
!           call string_strip_blanks (rname,nrname)
!         endif
!         if (direct .eq. 'GET') then
!           cmd = 'cp '//quote//rname(1:nrname)//quote//' '//  &
!                  lfile(1:nlfile)//char(0)
!         else if (direct .eq. 'PUT') then
!           cmd = 'cp '//lfile(1:nlfile)//' '//  &
!                  quote//rname(1:nrname)//quote//char(0)
!         endif
!      else
!         if (direct .eq. 'GET') then
!          cmd = 'rcp '//ruser(1:nruser)//'@'//rnode(1:nrnode)//':'//  &
!                 quote//rname(1:nrname)//quote//' '//lfile(1:nlfile)//char(0)
!         else if (direct .eq. 'PUT') then
!          cmd = 'rcp '//lfile(1:nlfile)//' '//ruser(1:nruser)//'@'//  &
!                 rnode(1:nrnode)//':'//quote//rname(1:nrname)//quote//char(0)
!         endif
!      endif
!      ncmd = index(cmd,char(0))
!      print *,cmd(1:ncmd)
!      call putsys_cmd (cmd(1:ncmd))
!
!      return
!      end subroutine rcpfile


!!--------------------- Donna's original rcpfile_parse --------------------!!
!!--------------------- Donna's original rcpfile_parse --------------------!!
!!--------------------- Donna's original rcpfile_parse --------------------!!


!      subroutine rcpfile_parse (str_in,node,user,file)
!
!      implicit none
!      character(len=*)                 :: str_in      ! argument
!      character(len=*)                 :: node        ! argument
!      character(len=*)                 :: user        ! argument
!      character(len=*)                 :: file        ! argument
!
!      character(len=1024)              :: str         ! local
!      integer                          :: nstr        ! local
!      integer                          :: i           ! local
!      integer                          :: i1          ! local
!      integer                          :: i2          ! local
!      integer                          :: i3          ! local
!      integer                          :: i4          ! local
!      integer                          :: jj          ! local
!      integer                          :: ic          ! local
!      integer                          :: nfile       ! local
!
!      call string_strip_blanks (str_in,str,nstr)
!
!      i1   = index(str,'@')
!      i2   = index(str,':')
!      i3   = index(str,'"')
!      i4   = index(str,':[')
!
!      user = ' '
!      node = ' '
!      file = ' '
!
!      if (i2.eq.0 .or. i2.eq.i4) then            ! no node
!         if (i3 .le. 0) then
!            file = str
!         else
!            file = str(i3:nstr)
!         endif
!      else
!         if (i3 .eq. 0) i3 = nstr + 1
!         if (i1.gt.0 .and. i1.lt.i2 .and. i1.lt.i3) then
!            user = str(1:i1-1)
!         endif
!
!         if (i2.lt.i3) then
!            if (user .eq. ' ') then
!               node = str(1:i2-1)
!            else
!               node = str(i1+1:i2-1)
!            endif
!         endif
!
!         if (str(i2+1:i2+1).eq.':') i2 = i2 + 1
!         if (i2+1.eq.i3) then
!            file = str(i3+1:nstr)
!         else
!            file = str(i2+1:nstr)
!         endif
!      endif
!
!      i = len(file)
!      do
!         if (i.le.0) exit
!         if (file(i:i) .ne. ' ') exit
!         i = i - 1
!      enddo
!      nfile = i
!      if (file(nfile:nfile) .eq. '"') nfile = nfile - 1
!      file(nfile+1:nfile+1) = char(0)
!
!      return
!      end subroutine rcpfile_parse


!!------------------------ rcpfile pre fetch -------------------------------!!
!!------------------------ rcpfile pre fetch -------------------------------!!
!!------------------------ rcpfile pre fetch -------------------------------!!


      subroutine rcpfile_pre_fetch (remotefile,localfile,istat,msg,command)
      implicit none
      character(len=*),intent(in)           :: remotefile          ! argument
      character(len=*),intent(out)          :: localfile           ! argument
      integer         ,intent(out)          :: istat               ! argument
      character(len=*),intent(out)          :: msg                 ! argument
      character(len=*),intent(out),optional :: command             ! argument
      character(len=200)                    :: copy,remotefile2    ! local

      if (present(command)) command = ' '
      call rcpfile_private_status (remotefile,istat,msg,copy,remotefile2)
      if (istat == RCPFILE_ERROR) return
      if (copy == 'cp') then
           localfile = remotefile2
           istat = finquire_file(localfile)
           if (istat == FINQUIRE_NOT_FOUND) then
                istat = RCPFILE_ERROR
                msg   = 'file on local node not found'
                if (present(command)) command = 'file to be read: '//localfile
                return
           end if
           istat     = RCPFILE_OK
           msg       = 'ready to read file on local node'
           if (present(command)) command = 'file to be read: '//localfile
      else
           localfile = tempname(remotefile)
           if (localfile == '') then
                istat = RCPFILE_ERROR
                msg   = 'no permission to copy file to local node'
                if (present(command)) command = 'file name: '//remotefile
                return
           end if
           call rcpfile_private_fetch  &
                              (copy,remotefile,localfile,istat,msg,command)
      end if
      return
      end subroutine rcpfile_pre_fetch


!!------------------------ rcpfile pre dispose -----------------------------!!
!!------------------------ rcpfile pre dispose -----------------------------!!
!!------------------------ rcpfile pre dispose -----------------------------!!


      subroutine rcpfile_pre_dispose (remotefile,localfile,istat,msg,command)
      implicit none
      character(len=*),intent(in)           :: remotefile          ! argument
      character(len=*),intent(out)          :: localfile           ! argument
      integer         ,intent(out)          :: istat               ! argument
      character(len=*),intent(out)          :: msg                 ! argument
      character(len=*),intent(out),optional :: command             ! argument
      character(len=200)                    :: copy,remotefile2    ! local

      if (present(command)) command = ' '
      call rcpfile_private_status (remotefile,istat,msg,copy,remotefile2)
      if (istat == RCPFILE_ERROR) return
      if (copy == 'cp') then
           localfile = remotefile2
           istat = finquire_file(localfile)
           if (istat == FINQUIRE_FOUND) then
                istat = RCPFILE_OK
                msg   = 'ready to overwrite file on local node'
                if (present(command))  &
                             command = 'file to be overwritten: '//localfile
                return
           end if
           istat = RCPFILE_OK
           msg   = 'ready to write file on local node'
           if (present(command)) command = 'file to be written: '//localfile
      else
           localfile = tempname(remotefile)
           if (localfile == '') then
                istat = RCPFILE_ERROR
                msg   = 'no permission to create file on local node'
                if (present(command)) command = 'file name: '//remotefile
                return
           end if
           if (present(command)) command = 'file to be written: '//localfile
      end if
      return
      end subroutine rcpfile_pre_dispose


!!------------------------ rcpfile post fetch -------------------------------!!
!!------------------------ rcpfile post fetch -------------------------------!!
!!------------------------ rcpfile post fetch -------------------------------!!

  
      subroutine rcpfile_post_fetch (remotefile,localfile,istat,msg,command)
      implicit none
      character(len=*),intent(in)           :: remotefile          ! argument
      character(len=*),intent(in)           :: localfile           ! argument
      integer         ,intent(out)          :: istat               ! argument
      character(len=*),intent(out)          :: msg                 ! argument
      character(len=*),intent(out),optional :: command             ! argument
      character(len=200)                    :: copy,remotefile2    ! local

      if (present(command)) command = ' '
      call rcpfile_private_status (remotefile,istat,msg,copy,remotefile2)
      if (istat == RCPFILE_ERROR) return
      if (copy == 'cp') then
           istat = RCPFILE_OK
           msg   = 'finished reading file on local node'
           if (present(command)) command = 'file read: '//localfile
      else
           call rcpfile_private_remove (localfile)
           istat = RCPFILE_OK
           msg   = 'finished reading local copy of file on remote node'
           if (present(command)) command = 'file read: '//localfile
      end if
      return
      end subroutine rcpfile_post_fetch


!!------------------------ rcpfile post dispose -----------------------------!!
!!------------------------ rcpfile post dispose -----------------------------!!
!!------------------------ rcpfile post dispose -----------------------------!!


      subroutine rcpfile_post_dispose (remotefile,localfile,istat,msg,command)
      implicit none
      character(len=*),intent(in)           :: remotefile          ! argument
      character(len=*),intent(in)           :: localfile           ! argument
      integer         ,intent(out)          :: istat               ! argument
      character(len=*),intent(out)          :: msg                 ! argument
      character(len=*),intent(out),optional :: command             ! argument
      character(len=200)                    :: copy,remotefile2    ! local

      if (present(command)) command = ' '
      call rcpfile_private_status (remotefile,istat,msg,copy,remotefile2)
      if (istat == RCPFILE_ERROR) return
      if (copy == 'cp') then
           istat = RCPFILE_OK
           msg   = 'finished writing file on local node'
           if (present(command)) command = 'file written: '//localfile
      else
           call rcpfile_private_dispose &
                               (copy,localfile,remotefile,istat,msg,command)
           if (istat == RCPFILE_ERROR) return
           call rcpfile_private_remove (localfile)
      end if
      return
      end subroutine rcpfile_post_dispose


!!--------------------- rcpfile private copy ---------------------------!!
!!--------------------- rcpfile private copy ---------------------------!!
!!--------------------- rcpfile private copy ---------------------------!!

! Copy the file using copy = rcp or cp.

      subroutine rcpfile_private_copy (copy,fromfile,tofile,istat,command)
      implicit none
      character(len=*),intent(in)           :: copy           ! argument
      character(len=*),intent(in)           :: fromfile       ! argument
      character(len=*),intent(in)           :: tofile         ! argument
      integer         ,intent(out)          :: istat          ! argument
      character(len=*),intent(out),optional :: command        ! argument
      character(len=200)                    :: command2       ! local   

      command2 = trim(copy)//' '//trim(fromfile)//' '//trim(tofile)
      call putsys_cmd (command2,istat)
      if (istat == 0) then
           istat = RCPFILE_OK
      else
           istat = RCPFILE_ERROR
      end if
      if (present(command)) command = command2
      return
      end subroutine rcpfile_private_copy


!!--------------------- rcpfile private fetch ---------------------------!!
!!--------------------- rcpfile private fetch ---------------------------!!
!!--------------------- rcpfile private fetch ---------------------------!!

! Fetch the file using copy = rcp or cp.

      subroutine rcpfile_private_fetch  &
                                    (copy,fromfile,tofile,istat,msg,command)
      implicit none
      character(len=*),intent(in)           :: copy           ! argument
      character(len=*),intent(in)           :: fromfile       ! argument
      character(len=*),intent(in)           :: tofile         ! argument
      integer         ,intent(out)          :: istat          ! argument
      character(len=*),intent(out)          :: msg            ! argument
      character(len=*),intent(out),optional :: command        ! argument

      call rcpfile_private_copy (copy,fromfile,tofile,istat,command)
      if (istat == RCPFILE_OK) then
           msg = 'file successfully fetched from remote node (using ' &
                                     //trim(copy)//')'
      else
           msg = 'error trying to fetch file from remote node (using ' &
                                     //trim(copy)//')'
      end if
      return
      end subroutine rcpfile_private_fetch


!!--------------------- rcpfile private dispose ---------------------------!!
!!--------------------- rcpfile private dispose ---------------------------!!
!!--------------------- rcpfile private dispose ---------------------------!!

! Dispose the file using copy = rcp or cp.

      subroutine rcpfile_private_dispose  &
                                    (copy,fromfile,tofile,istat,msg,command)
      implicit none
      character(len=*),intent(in)           :: copy           ! argument
      character(len=*),intent(in)           :: fromfile       ! argument
      character(len=*),intent(in)           :: tofile         ! argument
      integer         ,intent(out)          :: istat          ! argument
      character(len=*),intent(out)          :: msg            ! argument
      character(len=*),intent(out),optional :: command        ! argument

      call rcpfile_private_copy (copy,fromfile,tofile,istat,command)
      if (istat == RCPFILE_OK) then
           msg = 'file successfully disposed to remote node (using ' &
                                     //trim(copy)//')'
      else
           msg = 'error trying to dispose file to remote node (using ' &
                                     //trim(copy)//')'
      end if
      return
      end subroutine rcpfile_private_dispose


!!--------------------- rcpfile private remove ---------------------------!!
!!--------------------- rcpfile private remove ---------------------------!!
!!--------------------- rcpfile private remove ---------------------------!!

! Remove the file.

      subroutine rcpfile_private_remove (localfile)
      implicit none
      character(len=*),intent(in)  :: localfile       ! argument

      call putsys_cmd ('\rm -f '//trim(localfile))
      return
      end subroutine rcpfile_private_remove


!!----------------------------- end of module -----------------------------!!
!!----------------------------- end of module -----------------------------!!
!!----------------------------- end of module -----------------------------!!


      end module rcpfile_module


!!--------------------------------- end -----------------------------------!!
!!--------------------------------- end -----------------------------------!!
!!--------------------------------- end -----------------------------------!!

