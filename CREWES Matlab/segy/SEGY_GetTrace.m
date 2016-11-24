function trace = SEGY_GetTrace()
% segytrace = SEGY_gettrace()
%
% This function will return an initialized trace structure including all
% the header information plus an empty trace. With this trace, you
% set the header values that you need/have and then include the actual
% time series information. See the source file of this function for
% definitions of the fields in the trace structure.
%
% Chad Hogan, 2004
%
%
% NOTE: This SOFTWARE may be used by any individual or corporation for any purpose
% with the exception of re-selling or re-distributing the SOFTWARE.
% By using this software, you are agreeing to the terms detailed in this software's
% Matlab source file.

% BEGIN TERMS OF USE LICENSE
%
% This SOFTWARE is maintained by the CREWES Project at the Department
% of Geology and Geophysics of the University of Calgary, Calgary,
% Alberta, Canada.  The copyright and ownership is jointly held by
% its 'AUTHOR' (identified above) and the CREWES Project.  The CREWES
% project may be contacted via email at:  crewesinfo@crewes.org
%
% The term 'SOFTWARE' refers to the Matlab source code, translations to
% any other computer language, or object code
%
% Terms of use of this SOFTWARE
%
% 1) This SOFTWARE may be used by any individual or corporation for any purpose
%    with the exception of re-selling or re-distributing the SOFTWARE.
%
% 2) The AUTHOR and CREWES must be acknowledged in any resulting publications or
%    presentations
%
% 3) This SOFTWARE is provided "as is" with no warranty of any kind
%    either expressed or implied. CREWES makes no warranties or representation
%    as to its accuracy, completeness, or fitness for any purpose. CREWES
%    is under no obligation to provide support of any kind for this SOFTWARE.
%
% 4) CREWES periodically adds, changes, improves or updates this SOFTWARE without
%    notice. New versions will be made available at www.crewes.org .
%
% 5) Use this SOFTWARE at your own risk.
%
% END TERMS OF USE LICENSE

% $Id: SEGY_GetTrace.m,v 1.3 2008/10/23 17:36:26 gary Exp $

% SEGY trace header values.

trace.tracl  = 0;       % trace sequence number within the line
trace.tracr  = 0;       % trace sequence number within the reel
trace.fldr   = 1;       % field record number
trace.tracf  = 0;       % trace num in the field record
trace.ep     = 0;       % energy source point number
trace.cdp    = 0;       % Ensemble number. Doesn't have to be CDP
trace.cdpt   = 0;       % trace number within cdp ensemble 

trace.trid   = 1;       % trace id code, where
                        % 1 = seismic data
                        % 2 = dead
                        % 3 = dummy
                        % 4 = time break
                        % 5 = uphole
                        % 6 = sweep
                        % 7 = timing
                        % 8 = water break

trace.nvs    = 1;       % number of vertically summed traces
trace.nhs    = 1;       % number of horizontally summed traces
trace.duse   = 1;       % data use, 1 = production 2 = test

trace.offset = 0;       % distance from source point to receiver
trace.gelev  = 0;       % group elevation from sea level, up is positive
trace.selev  = 0;       % source elevation from sea level, up is positive
trace.sdepth = 0;       % source depth from surface, down is positive. 
trace.gdel   = 0;       % datum elevation at rec. group 
trace.sdel   = 0;       % datum elevation at source
trace.swdep  = 0;       % water depth at source
trace.gwdep  = 0;       % water depth at receiver group
trace.scalel = 0;       % scaling factor for gelev-gwdep, as an exponent
                        % to 10, valid from -4 to +4. So, scale from
                        % 0.0001 to 10000
trace.scalco = 0;       % scale for sx-gy, same deal as scalel
trace.sx     = 0;       % x source coordinate
trace.sy     = 0;       % y source coordinate
trace.gx     = 0;       % x group coordinate
trace.gy     = 0;       % y group coordinate
trace.counit = 1;       % coordinate units code
                        % 1 = m or ft
                        % 2 = arc seconds, x=lat and y=long, in seconds
                        % n of eq or e of greenwich (+=n/e, -=s/w)

trace.wevel  = 0;       % weathering velocity
trace.swevel = 0;       % subweathering velocity
trace.sut    = 0;       % uphole time at source
trace.gut    = 0;       % uphole time at receiver group
trace.sstat  = 0;       % source static correction
trace.gstat  = 0;       % rec. group static correction
trace.tstat  = 0;       % total applied static
trace.laga   = 0;       % lag time a, time in ms between the end of the
                        % 240 byte trace id header and the time
                        % break. This will be positive if the time break
                        % occurs after the end of the header. The time
                        % break is defined as the initiation pulse which
                        % may be recoreded on an auxiliary trace or as
                        % otherwise specified by the recording system. I
                        % imagine that if you're using this, you know
                        % what it means.

trace.lagb   = 0;       % lag time b, time in ms between the time berak
                        % and the initiation time of the energy
                        % source. This can be positive or negative.

trace.delrt  = 0;       % delay recording time. ms between shot and start
                        % of receivers.

trace.muts   = 0;       % start of the mute
trace.mute   = 0;       % end of the mute
trace.ns     = 0;       % number of samples
trace.dt     = 0;       % microseconds for sample interval

trace.gain   = 1;       % gain type of field instruments, 
                        % 1 = fixed, 
                        % 2 = binary, 
                        % 3 = floating point

trace.igc    = 0;       % instrument gain constant
trace.igi    = 0;       % instrument early/initial gain
trace.corr   = 0;       % Is it correlated? 1 = no, 2 = yes.

% Vibe stuff

trace.sfs    = 0;       % starting sweep frequency
trace.sfe    = 0;       % ending sweep frequency
trace.slen   = 0;       % sweep length in ms
trace.styp   = 1;       % sweep type: 
                        % 1 = linear
                        % 2 = cos-squared
                        % 3 = something else

trace.stas   = 0;       % sweep trace length at start, in ms
trace.stae   = 0;       % sweep trace length at end in ms

trace.tatype = 1;       % taper type
                        % 1 = linear
                        % 2 = cos-squared
                        % 3 = something else

% Filter stuff

trace.afilf  = 0;       % alias filter frequency if used
trace.afils  = 0;       % alias filter slope
trace.nofilf = 0;       % notch filter frequency if used
trace.nofils = 0;       % notch filter slope
trace.lcf    = 0;       % low cut filter frequency if used
trace.lcs    = 0;       % low cut slow
trace.hcf    = 0;       % high cut filter frequency if used
trace.hcs    = 0;       % high cut slope

trace.year   = 0;       % year this data was recorded
trace.day    = 0;       % day of the year
trace.hour   = 0;       % hour of the day, 24 h clock
trace.minute = 0;       % minute
trace.sec    = 0;       % second
trace.timbas = 1;       % time basis code
                        % 1 = local time
                        % 2 = UT/GMT
                        % 3 = something else

trace.trwf   = 0;       % trace weighting factor, 0.5 ^ N volts for least
                        % significant bit.

trace.grnors = 0;       % Geophone group number of roll switch position
                        % one. Whatever that means.
trace.grnofr = 0;       % Geophone group number of trace one within
                        % original field record. 
trace.grnlof = 0;       % Geophone group number of last trace within
                        % original field record.

trace.gaps   = 0;       % gap size -- total number of dropped groups
trace.otrav  = 0;       % overtravel taper code, 1 = down/behind,
                        % 2=up/ahead

% These are CWP additions. What the heck.

trace.d1     = 0;       % sample spacing for non-seismic data
trace.f1     = 0;       % first sample location for non-seismic data
trace.d2     = 0;       % sample spacing between traces
trace.f2     = 0;       % first trace location
trace.ungpow = 0;       % negative of power used for dynamic range
                        % compression
trace.unscale= 0; 
trace.ntr    = 0;       % number of traces
trace.mark   = 0;       % mark selected traces


trace.data =  [];