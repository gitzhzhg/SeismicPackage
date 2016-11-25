


ccccccccccccc   vel_boxes.inc   ccccccccccccccc

cccc  This include file is needed by vel_boxes.f and vel_fbridge.f
cccc  Common block vb must exactly match structure VbStruct in vel_boxes.h


c------------include files.

      include 'vel_data.inc'
      include 'va_message.inc'

c------------common block vb.

      integer vel           ! pointer to main application structure.
      integer autosave      ! automatic workfile save  if not zero.
      integer am_in_va      ! =1 if this is program va; =0 if is vel.

      integer ireadbox      ! windowbox number for read_box.
      integer isavebox      ! windowbox number for save_box.
      integer ipickbox      ! windowbox number for pick_box.
      integer ifunbox       ! windowbox number for fun_box.
      integer isetbox       ! windowbox number for fun_box.
      integer ivfidbox      ! windowbox number for vfid_box.
      integer iheadbox      ! windowbox number for head_box.
      integer iresbox       ! windowbox number for res_box.
      integer ilatbox       ! windowbox number for lat_box.
      integer ilat2box      ! windowbox number for lat2_box.
      integer iraybox       ! windowbox number for ray_box.
      integer idelbox       ! windowbox number for del_box.
      integer imultbox      ! windowbox number for mult_box.
      integer imiscbox      ! windowbox number for misc_box.
      integer ioffmutebox   ! windowbox number for offmute_box.
      integer iinfobox      ! windowbox number for info_box.

      integer nselect,nerrmsg,nraymsg,kode(nfunmax),sifun,cifun

      character*80 read_filename        ! name of velocity file to read.
      character*80 save_filename        ! name of velocity file to save.
      character*40 work_filename        ! name of automatic workfile.

      common/vb/
     $   vel,autosave,am_in_va,
     $   ireadbox,isavebox,ipickbox,ifunbox,isetbox,ivfidbox,
     $   iheadbox,iresbox,ilatbox,ilat2box,iraybox,idelbox,
     $   imultbox,imiscbox,ioffmutebox,iinfobox,
     $   nselect,nerrmsg,nraymsg,kode,sifun,cifun,
     $   read_filename,save_filename,work_filename


c------------------------end.

