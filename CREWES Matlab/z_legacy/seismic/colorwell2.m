function [cop,tcop,specmodel,fsm]=colorwell2(r,t,sigma,Tc)
% COLORWELL2: compute an operator to restore well color after decon
%
% [cop,tcop,specmodel,fsm]=colorwell(r,t,sigma,n)
%
% COLORWELL designs a convolutional minimum phase operator which
% can be applied to the output of any spiking decon to correct
% for a non-white reflectivity.
%
% r= well log reflectivity series whose amplitude spectrum shows the expected 
%    non-white behavior
% t= time coordinate for r
% sigma= standard deviation of Gaussian window to be applied to the
%       autocorrelation of r
% ********** default = .01*(t(end)-t(1)) ************
% Tc= desired lengthof color operator (seconds)
% ********** default = t(end)-t(1) *************
%
% cop= output color operator. This will be minimum phase and of length n
% tcop= time coordinate for cop
% specmodel= spectral model designed to approximate reflectivity spectrum
%       at low frequencies
% fsm= frequency coordinate for specmodel
%
% Compare the specmodel to the well with
% [R,f]=fftrl(r,t)
% plot(f,abs(R),fsm,specmodel)
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
  
if(nargin<4)
   Tc=t(end)-t(1);
end
if(nargin<3)
   sigma=.01*(t(end)-t(1));
end
if(length(t)~=length(r))
    error('r and t must be the same length')
end
r=r(:); %want a column vector
t=t(:)-t(1);
dt=t(2);
%compute autocorrelation of r
ar=conv(r,flipud(r));
[am,iam]=max(ar);
tau=dt*(0:length(ar)-1)'-(iam-1)*dt;
g=exp(-(tau/sigma).^2);
[Ar,f]= fftrl(ar.*g/am,tau);
A=abs(Ar);
[Am,imax]=max(A);
A(imax(1):end)=Am;
specmodel=A;
% compute the minimum phase time domain operator
n=round(Tc/dt)+1;
cop= minwave(specmodel,f,0,n);
fsm=f;
tcop=(0:length(cop)-1)*(t(2)-t(1));