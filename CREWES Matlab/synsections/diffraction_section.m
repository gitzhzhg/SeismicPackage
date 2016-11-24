%make a section full of diffractions
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

dt=.002;t=((0:500)*dt)';
dx=10;x=(0:200)*dx;
fmin=[10 5];fmax=[125 25];
nt=length(t);nx=length(x);
seis=zeros(nt,nx);
%determine diffraction locations
itdiff=1:50:nt;
ixdiff=1:20:nx;    
tmp=zeros(size(t));
tmp(itdiff)=1;
%bandlimit
tmp=filtf(tmp,t,fmin,fmax);
%install
for k=ixdiff
    seis(:,k)=tmp;
end
%linear v(z) function
z=0:dx:2000;
vz=1800+.6*z;
tv=2*vint2t(vz,z);
v=interp1(tv,vz,t);
%constant velocity
vm=mean(v);
vc=ones(size(v))*vm;
%set up for modelling
params=nan*(1:13);
params(1)=150;
params(2)=0;
params(13)=1;
[seismod,tmod,xmod]=vz_fkmod(seis,v,t,x,params);
plotimage(seismod,tmod,xmod);
[seismodc,tmod,xmod]=vz_fkmod(seis,vc,t,x,params);
plotimage(seismodc,tmod,xmod);