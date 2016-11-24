;; ;;==========================================================================
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
;; ;;==========================================================================
;; ;;
;; ;; Example:    Read some seismic traces from a SEGY file
;; 
;; ;; read all data from the file (head and data will be declared on output)
;; 
;;    SEGY_read, '/usr/data/seis/myprofile.su', head, data
;; 
;; ;; if we need only some traces, we may pick them from the file
;; 
;;    trace_idx = indgen(20)+35 ;;  read only traces 55..75
;;    SEGY_read, '/usr/data/seis/myprofile.su', head, data, trace_idx
;; 
;; ;; get information on the traces from the header
;; 
;;    ntr = head(0).ntr ;;                    number of trace in profile
;;    ns  = head(0).ns  ;;                    number of samples per trace
;;    
;; ;; now read all the source coordinates in an array
;; 
;;    scoord = fltarr(2,ntr)
;;    scoord(0,*) = head.sx
;;    scoord(1,*) = head.sy
;; 
;; ;; the names and contents of the other header fields are documented in the
;; ;; source code of segy_header.pro or in the Seismic Unix Manual
;; 
;; ;;==========================================================================

   
PRO SEGY_read, filename, head, data, tracenr, ignore_ntr=ignore_ntr

   ;; ignore_ntr allows to read data without check of filesize
   IF (NOT KEYWORD_SET(ignore_ntr)) THEN ignore_ntr = 0

   segy_header, header  ; define a SEGY header
   OpenR, segyunit, filename, /Get_Lun
   SegyHead = ASSOC(segyunit, header)
   head = SegyHead(0)
   ns  = LONG(float(head.ns))  ; nuber of samples in this trace
   ntr = LONG(float(head.ntr)) ; number of traces
   ntridx = N_ELEMENTS(tracenr)
   IF (ntridx EQ 0) THEN BEGIN
      tracenr = indgen(ntr)
      ntridx = ntr
   ENDIF
   head = Replicate(header, ntridx)
   data = FltArr(ntridx, ns)
   dataipt = FltArr(ns)
   tracenr = LONG(float(tracenr))
   FOR i = 0, ntridx-1 DO BEGIN
      tri = tracenr(i)
      IF ((tri GE 0) AND ((tri LE ntr) OR ignore_ntr)) THEN BEGIN
         IF (tri EQ 0) THEN ofs = 0 ELSE ofs = tri*(240L+4L*ns) ; file offset
         SegyHead  = ASSOC(segyunit, header, ofs)
         head(i)   = SegyHead(0)                       ; read head
         Point_Lun, segyunit, ofs+240L                 
         ReadU, segyunit, dataipt                      ; read trace
         data(i, *) = dataipt
      ENDIF
   ENDFOR
   Free_Lun, segyunit

END ; {SEGY_read}

