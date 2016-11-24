;==============================================================================
;
; Function:   segy_header.pro
; Author:     Martin Luethi, VAW, ETH Zurich
;             luthi@vaw.baum.ethz.ch
; Date:       26. September 1996
;
; Purpose:    SEGY-Header declaration
;             The fields are the same as described in the SU documentation
;
; Example:    segy_header, header  ; define a SEGY header
;
;==============================================================================

PRO SEGY_header,header

header= {segyheader,$
          tracl: 0L, $ ;	/* trace sequence number within line */
          tracr: 0L, $ ;	/* trace sequence number within reel */
          fldr:  0L, $ ;	/* field record number */
          tracf: 0L, $ ;	/* trace number within field record */
          ep: 0L, $ ;	/* energy source point number */
          cdp: 0L, $ ;	/* CDP ensemble number */
          cdpt: 0L, $ ;	/* trace number within CDP ensemble */
          trid: 0, $ ;	/* trace identification code:
;; 			1 = seismic data
;; 			2 = dead
;; 			3 = dummy
;; 			4 = time break
;; 			5 = uphole
;; 			6 = sweep
;; 			7 = timing
;; 			8 = water break
;; 			9---, N = optional use (N = 32,767)
;; 
;; 			Following are CWP id flags:
;; 
;; 			 9 = autocorrelation
;; 
;; 			10 = Fourier transformed - no packing
;; 			     xr[0],xi[0], ..., xr[N-1],xi[N-1]
;; 
;; 			11 = Fourier transformed - unpacked Nyquist
;; 			     xr[0],xi[0],...,xr[N/2],xi[N/2]
;; 
;; 			12 = Fourier transformed - packed Nyquist
;; 	 		     even N:
;; 			     xr[0],xr[N/2],xr[1],xi[1], ...,
;; 				xr[N/2 -1],xi[N/2 -1]
;; 				(note the exceptional second entry)
;; 			     odd N:
;; 			     xr[0],xr[(N-1)/2],xr[1],xi[1], ...,
;; 				xr[(N-1)/2 -1],xi[(N-1)/2 -1],xi[(N-1)/2]
;; 				(note the exceptional second & last entries)
;; 
;; 			13 = Complex signal in the time domain
;; 			     xr[0],xi[0], ..., xr[N-1],xi[N-1]
;; 
;; 			14 = Fourier transformed - amplitude/phase
;; 			     a[0],p[0], ..., a[N-1],p[N-1]
;; 
;; 			15 = Complex time signal - amplitude/phase
;; 			     a[0],p[0], ..., a[N-1],p[N-1]
;; 
;; 			16 = Real part of complex trace from 0 to Nyquist
;; 
;; 			17 = Imag part of complex trace from 0 to Nyquist
;; 
;; 			18 = Amplitude of complex trace from 0 to Nyquist
;; 
;; 			19 = Phase of complex trace from 0 to Nyquist
;; 
;; 			21 = Wavenumber time domain (k-t)
;; 
;; 			22 = Wavenumber frequency (k-omega)
;; 
;; 			23 = Envelope of the complex time trace
;; 
;; 			24 = Phase of the complex time trace
;; 
;; 			25 = Frequency of the complex time trace
;; 
;; 			30 = Depth-Range (z-x) traces
;; 
;; 			101 = Seismic data packed to bytes (by supack1)
;; 			
;; 			102 = Seismic data packed to 2 bytes (by supack2)
;;			*/
	nvs: 0,$ ;	/* number of vertically summed traces (see vscode
;;			   in bhed structure) */
	nhs: 0,$ ;	/* number of horizontally summed traces (see vscode
;;			   in bhed structure) */
	duse: 0,$ ;	/* data use:
;; 				1 = production
;; 				2 = test */
	offset: 0L,$ ;	/* distance from source poto receiver
;;			   group (negative if opposite to direction
;;			   in which the line was shot) */
	gelev: 0L,$ ;	/* receiver group elevation from sea level
;;			   (above sea level is positive) */
	selev: 0L,$ ;	/* source elevation from sea level
;;			   (above sea level is positive) */
	sdepth: 0L,$ ;	/* source depth (positive) */
	gdel: 0L,$ ;	/* datum elevation at receiver group */
	sdel: 0L,$ ;	/* datum elevation at source */
	swdep: 0L,$ ;	/* water depth at source */
	gwdep: 0L,$ ;	/* water depth at receiver group */
	scalel: 0,$ ;	/* scale factor for previous 7 entries
;; 			   with value plus or minus 10 to the
;; 			   power 0, 1, 2, 3, or 4 (if positive,
;; 			   multiply, if negative divide) */
	scalco: 0,$ ;	/* scale factor for next 4 entries
;; 			   with value plus or minus 10 to the
;; 			   power 0, 1, 2, 3, or 4 (if positive,
;; 			   multiply, if negative divide) */
	 sx: 0L,$ ;	/* X source coordinate */
	 sy: 0L,$ ;	/* Y source coordinate */
	 gx: 0L,$ ;	/* X group coordinate */
	 gy: 0L,$ ;	/* Y group coordinate */
	counit: 0,$ ;	/* coordinate units code:
;; 				for previous four entries
;; 				1 = length (meters or feet)
;; 				2 = seconds of arc (in this case, the
;; 				X values are longitude and the Y values
;; 				are latitude, a positive value designates
;; 				the number of seconds east of Greenwich
;; 				or north of the equator */
	wevel: 0,$ ;	/* weathering velocity */
	swevel: 0,$ ;	/* subweathering velocity */
	sut: 0,$ ;	/* uphole time at source */
	gut: 0,$ ;	/* uphole time at receiver group */
	sstat: 0,$ ;	/* source static correction */
	gstat: 0,$ ;	/* group static correction */
	tstat: 0,$ ;	/* total static applied */
	laga: 0,$ ;	/* lag time A, time in ms between end of 240-
;; 			   byte trace identification header and time
;; 			   break, positive if time break occurs after
;; 			   end of header, time break is defined as
;; 			   the initiation pulse which maybe recorded
;; 			   on an auxiliary trace or as otherwise
;; 			   specified by the recording system */
	lagb: 0,$ ;	/* lag time B, time in ms between the time break
;; 			   and the initiation time of the energy source,
;; 			   may be positive or negative */
	delrt: 0,$ ;	/* delay recording time, time in ms between
;; 			   initiation time of energy source and time
;; 			   when recording of data samples begins
;; 			   (for deep water work if recording does not
;; 			   start at zero time) */
	muts: 0,$ ;	/* mute time--start */
	mute: 0,$ ;	/* mute time--end */
	ns: 0,$ ;	/* number of samples in this trace */
	dt: 0,$ ;	/* sample rval: 0L,$ ; in micro-seconds */
	gain: 0,$ ;	/* gain type of field instruments code:
;; 				1 = fixed
;; 				2 = binary
;; 				3 = floating point
;;                              4 = ----optional use */
	igc: 0,$ ;	/* instrument gain constant */
	igi: 0,$ ;	/* instrument early or initial gain */
	corr: 0,$ ;	/* correlated:
;; 				1 = no
;; 				2 = yes */
	sfs: 0,$ ;	/* sweep frequency at start */
	sfe: 0,$ ;	/* sweep frequency at end */
	slen: 0,$ ;	/* sweep length in ms */
	styp: 0,$ ;	/* sweep type code:
;; 				1 = linear
;; 				2 = cos-squared
;; 				3 = other */
	stas: 0,$ ;	/* sweep trace length at start in ms */
	stae: 0,$ ;	/* sweep trace length at end in ms */
	tatyp: 0,$ ;	/* taper type: 1=linear, 2=cos^2, 3=other */
	afilf: 0,$ ;	/* alias filter frequency if used */
	afils: 0,$ ;	/* alias filter slope */
	nofilf: 0,$ ;	/* notch filter frequency if used */
	nofils: 0,$ ;	/* notch filter slope */
	lcf: 0,$ ;	/* low cut frequency if used */
	hcf: 0,$ ;	/* high cut frequncy if used */
	lcs: 0,$ ;	/* low cut slope */
	hcs: 0,$ ;	/* high cut slope */
	year: 0,$ ;	/* year data recorded */
	day: 0,$ ;	/* day of year */
	hour: 0,$ ;	/* hour of day (24 hour clock) */
	minute: 0,$ ;	/* minute of hour */
	sec: 0,$ ;	/* second of minute */
	timbas: 0,$ ;	/* time basis code:
;; 				1 = local
;; 				2 = GMT
;; 				3 = other */
	trwf: 0,$ ;	/* trace weighting factor, defined as 1/2^N
;;			   volts for the least sigificant bit */
	grnors: 0,$ ;	/* geophone group number of roll switch
;;			   position one */
	grnofr: 0,$ ;	/* geophone group number of trace one within
;;			   original field record */
	grnlof: 0,$ ;	/* geophone group number of last trace within
;;			   original field record */
	gaps: 0,$ ;	/* gap size (total number of groups dropped) */
	otrav: 0,$ ;	/* overtravel taper code:
;; 				1 = down (or behind)
;; 				2 = up (or ahead) */
;;	/* local assignments */
        d1: 0., $ ;	/* sample spacing for non-seismic data */
	f1: 0.,$ ;	/* first sample location for non-seismic data */
	d2: 0.,$ ;	/* sample spacing between traces */
	f2: 0.,$ ;	/* first trace location */
	ungpow: 0.,$ ;	/* negative of power used for dynamic
;;			   range compression */
	unscale: 0.,$ ;	/* reciprocal of scaling factor to normalize
;;			   range */
	ntr: 0L,$ ; 	/* number of traces */
        mark: 0,$ ;	/* mark selected traces */
        unass: intarr(15)$ ;	/* unassigned--NOTE: last entry causes 
;; 			   a break in the word alignment, if we REALLY
;; 			   want to maintain 240 bytes, the following
;; 			   entry should be an odd number of UINT2
;; 			   OR do the insertion above the "mark" keyword
;; 			   entry */
        }


END; {SEGY_Header}

