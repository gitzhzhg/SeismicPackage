;; ;;=========================================================================
;; ;;
;; ;; Functions:  segy_write, segy_read, segy_header
;; ;;
;; ;; Author:     Martin Luethi, VAW, ETH Zurich, Switzerland
;; ;;             luthi@vaw.baum.ethz.ch
;; ;;
;; ;; Date:       26. September 1996
;; ;;
;; ;; Purpose:    Write and read data files in SEGY format
;; ;;             
;; ;;             The SEGY format is a standard file format for seismic data.
;; ;;             It consists mainly of headers containing all the information
;; ;;             on a trace and a body with the measured seismic data. The
;; ;;             data is stored in binary form.
;; ;;
;; ;;             The header fields are those defined by Seismic Unix which
;; ;;             do not correspond in all detail to standard SEGY format
;; ;;             (see the documentation of Seismic Unix for further details).
;; ;;
;; ;;             Seismic Unix is powerful free seismic software which is
;; ;;             available via ftp from the Colorado School of Mines:
;; ;;                   ftp.cwp.mines.edu. 
;; ;;             Their homepage is located at 
;; ;;                   http://www.cwp.mines.edu
;; ;;
;; ;;=========================================================================
;; ;;
;; ;; Example:    Save 50 traces of seismic data with 600 samples per trace
;; ;;             to a SEGY-file. Set some header fields with important data
;; ;;             such as coordinates of the shots
;; 
;; ;; Save ntr=50 traces with ns=600 samples each
;; 
;;    ntr = 50
;;    ns = 600
;; 
;; ;; The header structure is defined using SEGY_header:
;; 
;;    SEGY_header, header
;; 
;; ;; Now make an array of ntr headers and define the data array
;; 
;;    head = Replicate(header, ntr)
;;    data = FltArr(ntr, ns)
;; 
;; ;; Fill the header fields with important values (this fields will be set
;; ;; automatically by segy_write
;; 
;;    head.ntr = ntr
;;    head.ns  = ns
;; 
;; ;; here we fill the header with any information we want
;; ;; see SU documentation or SEGY_header.pro for field names
;; 
;;    head.dt = dt ;;                            sample time
;;    FOR i = 0, ntr-1 DO BEGIN
;;       head(i).sx = source_coordinate_x(i) ;;  cooridates of shotpoints
;;       head(i).sy = source_coordinate_y(i)
;;    ENDFOR
;; 
;; ;; now let's write the data to disk
;; 
;;    SEGY_write, '/usr/data/seis/myprofile.su', head, data
;; 
;;;;==========================================================================

PRO SEGY_write, filename, head, data

   sh = Size(head)
   sd = Size(data)
   IF (sh(1) NE sd(1)) THEN $
     message, 'SEGY_write: array sizes of header and data different'
   ntr = sd(1)
   ns = sd(2)
   head.ns = ns
   head.ntr = ntr
   segy_header, header ; define a SEGY header
   data = Float(data)
   OpenW, segyunit, filename, /Get_Lun
   FOR i = 0, ntr-1 DO BEGIN
      IF (i EQ 0) THEN ofs = 0 ELSE ofs = i*(240L+4L*ns) ; file offset
      SegyHead  = ASSOC(segyunit, header, ofs)
      SegyHead(0) =  head(i) ;                             write head
      Point_Lun, segyunit, ofs+240L                 
      WriteU, segyunit, data(i, *) ;                       write data
   ENDFOR
   Free_Lun, segyunit
   
END; {SEGY_write}

