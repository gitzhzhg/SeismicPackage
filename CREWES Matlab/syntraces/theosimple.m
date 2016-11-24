function [theo,tlog,rcs,pm,p]=theosimple(sonic,dens,z,fmult,wlet,tw)
% [theo,tlog,rcs,pm,p]=theosimple(sonic,dens,z,fmult,wlet,tw)
%
% THEOSIMPLE computes a 1-D seismic model (Theogram) from well
% log information. It uses the sonic log to build a time-depth
% curve and then calls THEOGRAM. (See THEOGRAM help for more info)
%
% sonic ... vector of sonic log samples. Must be in typical sonic
%	units (e.g. micro-sec/meter).
% dens ... vector of same length as sonic giving density values.
%	For a constant density result, provide this as ones(size(sonic)).
% z ... vector of same size as sonic giving the depths of the log.
% fmult ... multiple flag
%            0 -> produce a primaries only convolutional theogram
%            1 -> produce a primaries (with attenuation losses) plus
%                  multiples theogram
% wlet ... vector of wavelet samples
%   Wavelets can be generated using any of WAVEMIN, WAVEZ, WAVEVIB,
%   RICKER, and FILTF in the Seismic_Toolbox
% tw ...  vector of time coordinates for the wavelet. Note that
%         a non-causal wavelet should have tw(1)<0. The convolution
%         will be such that the wavelet sample at tw==0 will be
%         superimposed on the reflectivity spike
%
% theo ... n length vector containing the theogram samples.
%         (n is determined by the algorithm).
% tlog ... nlength vector containing the times for the theogram
%	rcs ... n length vector of computed reflection coefficients in time
%  pm ... nlength vector of attenuated primaries plus multiples. 
%         If fmult == 0, this is identical to rcs.
%	p ... nlength vector with attenuated primary rcs. If fmult==0, this
%         is identical to rcs. If fmult==1, then the multiple content can
%         be obtained as: pm-p
%
% G.F. Margrave, October 1994
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
% a bit of error checking
	if(length(sonic)~=length(dens))
		error('Sonic and density logs must be the same size');
	end
	if( length(sonic)~=length(z) )
		error('Sonic and Z vector must be the same size');
	end
	if( length(wlet)~=length(tw) )
		error('Wavelet and TW must be the same size');
	end
%obtain a time-depth function
nlegs=length(sonic)/100;
if(nlegs>150) nlegs=150; end
if( nlegs< 10) nlegs=10; end
[tz,zt]=sonic2tz(sonic,z,nlegs);
%convert sonic to vins
sonic=1.e06 ./ sonic;
%call theogram
[theo,tlog,rcs,pm,p]=theogram(sonic,dens,z,wlet,tw,[zt tz],0,fmult);