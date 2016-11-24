function [sgram,tlog,rcs,pm,p,tz,zt]=seismotz(sonicref,densref,z,sonictz,fmult,fpress,wlet,tw)
% [sgram,tlog,rcs,pm,p,tz,zt]=seismotz(sonicref,densref,z,sonictz,fmult,fpress,wlet,tw)
%
% SEISMOTZ computes a 1-D seismic model (seismogram) from well
% log information. It uses the sonictz log to build a time-depth
% curve and then uses sonicref to calcualte the seismogram.
%
% sonicref ... vector of sonic log samples used to create the reflection 
%   coefficents. Must be in typical sonic units (e.g. micro-sec/meter).
% densref ... vector of same length as sonicref giving density values.
%	For a constant density result, provide this as ones(size(sonic)).
% z ... vector of same size as sonic giving the depths of the log.
% sonictz ... sonic log samples used to create the time depth curve.
%   Must be in typical sonic units (e.g. micro-sec/meter).
% fmult ... multiple flag
%            0 -> produce a primaries only convolutional seismogram
%            1 -> produce a primaries (with attenuation losses) plus
%                  multiples seismogram
% fpress ... flag for a pressure or displacement seismogram
%            1 -> a pressure seismogram is desired
%            0 -> a displacement seismogram is desired
%          For a pressure seismogram, the source is a unit compression. For
%          a displacement seismogram, the source is a unit displacement in
%          the positive z (down) direction.
%
% wlet ... vector of wavelet samples
%   Wavelets can be generated using any of WAVEMIN, WAVEZ, WAVEVIB,
%   RICKER, and FILTF in the Seismic_Toolbox
% tw ...  vector of time coordinates for the wavelet. Note that
%         a non-causal wavelet should have tw(1)<0. The convolution
%         will be such that the wavelet sample at tw==0 will be
%         superimposed on the reflectivity spike
%
% sgram ... n length vector containing the seismogram samples.
%         (n is determined by the algorithm).
% tlog ... nlength vector containing the times for the seismogram
% rcs ... n length vector of computed reflection coefficients in time
% pm ... nlength vector of attenuated primaries plus multiples. 
%         If fmult == 0, this is identical to rcs.
% p ... nlength vector with attenuated primary rcs. If fmult==0, this
%         is identical to rcs. If fmult==1, then the multiple content can
%         be obtained as: pm-p
% tz = vector of length nlegs+2 containing the 2-way times of the  
% 	approximate t-z curve using sonictz
% zt = vector of length nlegs+2 containing the depths of the approximate
% 	t-z curve using sonictz
%
% H.J.E. Lloyd and G.F. Margrave, October 2013
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

% [w1,tw1]=ricker(.000001,40,.2); % create a finely sampled wavelet
% [s1,t1,r1,pm1,p1]=seismo(sonicref,densref,z,fmult,fpress,w1,tw1); % use seismo and the sonic ref to create a seismogram
% [tz1,zt1,vins]=sonic2tz(sonicref,z,-10000); % find the time-depth curves of the sonicref
% [tz,zt,vins]=sonic2tz(sonictz,z,-10000);% find the time-depth curves of the sonictz
% z1=interp1(tz1,zt1,t1);% calculate the depth curve for the sonic ref
% t2=interp1(zt,tz,z1);% calculate a new time curve based on the sample rate of tw
% tlog=t2(1):tw(2)-tw(1):t2(end);
% rcs=interp1(t1,r1,tlog);% calculate new reflection coefficents using the new t
% rcs(isnan(rcs))=0;
% p=interp1(t1,p1,tlog);% calculate new primaries using the new t
% p(isnan(p))=0;
% pm=interp1(t1,pm1,tlog);% calculate new primaries and multiples using the new t
% pm(isnan(pm))=0;
% if all(tw>=0)
%     sgram=convm(rcs,wlet); % if wavelet is causal use convm to calculate sgram 
% else
%     sgram=convz(rcs,wlet); % if wavelet is zero phase use convz to calculate sgram 
% end

% a bit of error checking
	if(length(densref)~=length(z))
		error('Density log and Z vector must be the same size');
	end
	if( length(sonicref)~=length(z) )
		error('Sonicref and Z vector must be the same size');
    end
    if( length(sonictz)~=length(z) )
		error('Sonictz and Z vector must be the same size');
	end
	if( length(wlet)~=length(tw) )
		error('Wavelet and TW must be the same size');
	end
%obtain a time-depth function
nlegs=length(sonictz)/100;
if(nlegs>150) nlegs=150; end
if( nlegs< 10) nlegs=10; end
[tz,zt]=sonic2tz(sonictz,z,nlegs);% calculate time depth curves with sonictz
%convert sonic to vins
vins=1.e06 ./ sonicref;% calculate interval velocity with sonicref
%call seismogram
[sgram,tlog,rcs,pm,p]=seismogram(vins,densref,z,wlet,tw,[zt tz],0,fmult,fpress);