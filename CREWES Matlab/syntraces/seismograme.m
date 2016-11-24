function [theo,t,rcs,pm,p]=seismogram(vins,dens,z,wlet,tw,tzobj,x,fmult,fpress)
% [theo,tlog,rcs,pm,p]=seismogram(vins,dens,z,wlet,tw,tzobj,x,fmult,fpress)
%
% SEISMOGRAM computes a 1-D synthetic seismogram given regularly sampled
% vectors of instantaneous velocity and density (in depth), a wavelet,
% and a t-z function. Currently a convolutional algorithm is 
% available and works as follows:
%	- compute the impedence in depth as vins.*dens
%	- stretch the impedence to time at a sample rate required to
%		avoid aliasing
%	- compute RC's as the derivative of the log(impedence)
%	- resample RC's to the sample rate of the wavelet
%	- convolve the wavelet and the RC's
%
% Multiples can also be included and are computed using the algorithm of
% Waters, "Reflection Seismology", 1981, John Wiley, pp 128-135.
% The RC's as computed above at the sample rate of the wavelet are run
%	through Waters' algorithm to compute the earth impulse response which
%	has primaries with transmission losses and all multiples.
%
%	vins ... vector of instantaneous velocity
%	dens ... vector of density. (Note: you can compute
%            a constant density seismogram by providing this as
%            ones(size(vins)).   )
%	z ... vector of depths corresponding to vins and dens
%	wlet ... vector of wavelet samples
%	tw ... vector of time coordinates for the wavelet. Note that
%         a non-causal wavelet should have tw(1)<0. The convolution
%         will be such that the wavelet sample at tw==0 will be
%         superimposed on the reflectivity spike
%	tzobj ... Time-depth function. 
%         this can be a Random Earth Object as used by LOGSEC or
%         a simple [n,2] matrix where the first column is depth
%         second is time
%	x ... x coordinate at which seismogram is computed. Only needed
%         if tzobj is a LOGSEC tz object. If its a simple 2 column
%         time-depth matrix then default this.
%		******** default =0.0 *******
%   fmult ... multiple flag
%            0 -> produce a primaries only convolutional seismogram
%            1 -> produce a primaries (with attenuation losses) plus
%               	multiples seismogram
%      ******* default =0 ********
%   fpress ... flag for a pressure or displacement seismogram
%            1 -> a pressure seismogram is desired
%            0 -> a displacement seismogram is desired
%          For a pressure seismogram, the source is a unit compression. For
%          a displacement seismogram, the source is a unit displacement in
%          the positive z (down) direction.
%      ******* default = 1 ******
%   theo ... n length vector containing the seismogram samples.
%         (n is determined by the algorithm).
%	t ... nlength vector containing the times for the seismogram
%	rcs ... n length vector of computed reflection coefficients in time
%  pm ... nlength vector of attenuated primaries plus multiples. 
%         If fmult == 0, this is identical to rcs.
%	p ... nlength vector with attenuated primary rcs. If fmult==0, this
%         is identical to rcs. If fmult==1, then the multiple content can
%         be obtained as: pm-p
%
% G.F. Margrave Aug 94
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
[m,n]=size(vins);
flip=0;
if(n~=1)
	vins=vins';
	flip=1;
end
[m,n]=size(dens);
if(n~=1)
	dens=dens';
end
nz=length(vins);
if( length(dens)~=nz )
	error('Density vector must have same length as vins');
end
if( length(z) ~= nz)
	error('Z vector must have same length as vins');
end
if(nargin<8)
	fmult=0;
end
if(nargin<9)
	fpress=1;
end
if(nargin<7)
	x=0;
end
if(length(wlet)~=length(tw))
	error('wlet and tw must have same length');
end
%compute impedence log in time
impdnc= vins.*dens;
%stretch to time
dtout=tw(2)-tw(1);
if( nargin < 7)
	imptime = logtotime([z impdnc],tzobj,dtout);
else
	imptime = logtotime([z impdnc],tzobj,dtout,x);
end
tlog=imptime(:,1);
imptime=imptime(:,2);
%compute rc's. The transpose is there to force gradient to compute
%a real result
%rcs= gradient( log( imptime' ) )';
rcs=imp2rcse(imptime);
%put in multiples if requested
if(fmult)
	[pm,p]=theo_mult_w(rcs,fpress);
else
	pm=rcs;
	p=rcs;
end
%convolve
inot=near(tw,0);
theo=convz(pm,wlet,inot);
t=(0:length(theo)-1)*dtout;
%thats all