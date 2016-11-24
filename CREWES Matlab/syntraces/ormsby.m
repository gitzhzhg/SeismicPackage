function [w,t]=ormsby(f1,f2,f3,f4,tlen,dt)
% ORMSBY: Creates an Ormsby bandpass filter
%
% [w,t]=ormsby(f1,f2,f3,f4,tlen,dt)
%
% Make a ormsby wavelet given the four frequency parameters
% f1 = low frequency stop
% f2 = lowest frequency to pass unattenuated
% f3 = highest frequency to pass attenuated`
% f4 = high frequency stop
% tlen = wavelet length in seconds. Wavelet will go from
%	-tlen/2 to tlen/2
% dt = wavelet sample rate in seconds
% 
% by G.F. Margrave, 1995-2015
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

%test the inputs for reasonableness
fnyq=.5/dt;
if(any(diff([0,f1,f2,f3,f4,fnyq])<0))
    error('you must have f1<f2<f3<f4<fnyq');
end
if(tlen<dt)
    error('wavelet length, tlen, must be greater than dt');
end

% make the time coordinate vector
  n=round(tlen/dt)+1;
  nzero=ceil((n+1)/2);%zero time sample is here.
  nr=n-nzero;%number of samples to the right of nzero
  nl=n-nr-1;%number of samples to the right of nzero
  t=dt*(-nl:nr)';

%make the wavelet

	w= (pi*f4^2) * (sinque(pi*f4*t)).^2/(f4-f3);
	w= w- (pi*f3^2) * (sinque(pi*f3*t)).^2/(f4-f3);
	w= w- (pi*f2^2) * (sinque(pi*f2*t)).^2/(f2-f1);
	w= w+ (pi*f1^2) * (sinque(pi*f1*t)).^2/(f2-f1);
 
%normalize
    w=wavenorm(w,t,2);