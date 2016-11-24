function out=ps_space(spec,f,dx,v,dz)
%out=ps_space(spec,f,dx,v,dz)
%
%This function extrapolates an input Fourier spectrum through dz
%by space domain phase screen.
%
%out...x-w spectrum of extrapolated seismic data
%spec...x-w spectrum of input seismic data
%f...frequency axis of spec in Hz (cycles/second)
%dx...x interval
%v...v(x) (must correspond to the range of x spanned by the seismic data)
%dz...depth step
%
%Rob Ferguson 2003
%Assistant Professor
%Department of Geological Sciences
%University of Texas, Austin
%512 471 6405
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

%fergusonr@mail.utexas.edu

%***find sizes of things***
f=f(:);%force f to be a column vector
v=v(:)';%force v to be a row vector
[rs cs]=size(spec);
[rf cf]=size(f);
[rv cv]=size(v);
[rz cz]=size(dz);
%***************************

%***check input***
if rs~=rf;disp('ERROR: something is wrong with the size of the input spectrum and/or f axis');end
if cs~=cv;disp('ERROR: the velocity has to have as many elements as the spectrum is wide');end
if and(rz~=1,cz~=1);disp('ERROR: dz should be a scalar');end
%*****************

%***innitialize some variables***
vbar=mean(v(:));
%********************************

%***extrapolate each monochromatic (in w) set of phases***
temp1=exp(i*2*pi*dz*(f(:)*ones(1,cv)*vbar./((ones(rf,1)*v).^2)-f(:)*ones(1,cv)/vbar)).*spec;
temp2=ifft(temp1,[],2);
spec=ips(temp2,f,dx,vbar,dz);
out=fft(spec,[],2);
%*********************************************************