function [cop,tcop,specmodel,fsm]=colorwell(r,t,n)
% COLORWELL: compute an operator to restore well color after decon
%
% [cop,tcop,specmodel,fsm]=colorwell(r,t,n)
%
% COLORWELL designs a convolutional minimum phase operator which
% can be applied to the output of any spiking decon to correct
% for a non-white reflectivity.
%
% r= well log reflectivity series whose amplitude spectrum shows the expected 
%    non-white behavior
% t= time coordinate for r
% n= desired length of the color operator (samples)
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
  
%compute reflectivity spectrum
r=padpow2(r);
t=(t(2)-t(1))*(0:length(r)-1);
[R,f]= fftrl(r,t);
% fit a curve of the form a+b*atan(f/f0) to the spectrum from 1Hz to 1/2 Nyquist
% Here f0 will be taken as 1/4 Nyquist
fhalfnyq=f(end)/2;
% f0=f(end)/10;
%search for optimal f0
ftest=40:120;
obj=zeros(size(ftest));
indf=near(f,1,fhalfnyq);
for k=1:length(ftest)
    f0=ftest(k);
    A=[ones(size(f(indf))) atan(f(indf)/f0)];
    ab=A\abs(R(indf));%solve the spectral model
    specmodel=zeros(size(f));
    specmodel(2:end)=ab(1)+ab(2)*atan(f(2:end)/f0);
    specmodel(1)=specmodel(2);%avoid a zero at zero Hz
    obj(k)=sum((specmodel(1:ftest(end))-abs(R(1:ftest(end)))).^2);
end
[omin,imin]=min(obj);
f0=ftest(imin);
A=[ones(size(f(indf))) atan(f(indf)/f0)];
ab=A\abs(R(indf));%solve the spectral model
specmodel=zeros(size(f));
specmodel(2:end)=ab(1)+ab(2)*atan(f(2:end)/f0);
specmodel(1)=specmodel(2);%avoid a zero at zero Hz
% compute the minimum phase time domain operator
cop= minwave(specmodel,f,0,n);
fsm=f;
tcop=(0:length(cop)-1)*(t(2)-t(1));