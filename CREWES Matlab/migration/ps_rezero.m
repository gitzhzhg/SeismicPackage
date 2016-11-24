function fdataout=ps_rezero(fdata,f,dx,tmax)
% PS_REZERO ... rezero the temporal zero pad in phase shift extrapolation
%
% fdataout=ps_rezero(fdata,f,dx,tmax)
%
% Designed for use with ips and pspi_mig etc.
%
% fdata ... fk spectrum of data with k axis wrapped and positive f's only
% f ... f coordinate vector for fdata
% dx ... spatial sample size for fdata
% tmax ... time (in seconds) beyond which the data will be set to zero
% fdataout ... fkspectrum of data with the zero pad re-zero'd
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

% 

%ok, we must do an ifft over f, re-zero, and then an fft over t

%figure out tmax and dt
[r,c]=size(fdata);
df=f(2)-f(1);
Tmax=1/df; %true maximum time including any zero pad
fmax=f(end);
dt=.008;
fnyq=.5/dt;
while(fnyq<fmax)
    dt=dt/2;
    fnyq=.5/dt;
end
Tmax=Tmax-dt;%necessary fiddle
n=round(tmax/dt)+1;
%make f and k axes
fnew=0:df:fnyq;
kx=fftshift(1/2/c/dx*[-c:2:c-2]);
data=zeros(length(fnew),c);
indf=near(fnew,f(1),f(end));
data(indf,:)=fdata;
data(end,:)=0;
%inverse fk transform
[tdata,t,x]=ifktran(data,fnew,kx,0,0);
indt=near(t,tmax,Tmax);
tdata(indt,:)=0;
%forward fk transform
data=fktran(tdata,t,x,0,0,0,0);
fdataout=data(indf,:);