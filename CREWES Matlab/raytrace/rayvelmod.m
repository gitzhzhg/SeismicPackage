function rayvelmod(v,dg,z,x)
% RAYVELMOD: establish a velocity model for vxz raytracing
%
% function rayvelmod(v,dg,z,x)
%
% Establish velocity model for raytracing
% v ... 2D matrix of velocities
% dg ... grid size for v. (i.e. both dx and dz are dg)
% z ... depth coordinate vector for v
% ******* default dg*(0:size(v,1)-1)' ***********
% x ... lateral coordinate vector for v
% ******* default dg*(0:size(v,2)-1) ***********
%
% This function stores the velocity model as a global. If
% you wish to save space, you can delete your model after
% calling RAYVELMOD.
%
% Globals established
% RTV2 ... matrix of velocity squared
% RTDLNVDX ... matrix of d(ln(v))/dx
% RTDLNVDZ ... martix of d(ln(v))/dz
% RTDG ... grid spaceing (x&z) in physical units
% RTVAVE ... a single average velocity function (lateral mean)
% RTVRMS ... a single rms velocity function (lateral mean)
% RTT ... one-way time coordinate for RTVAVE and RTVRMS
% RTZ ... z coordinate for velocity model
% RTX ... x coordinate for velocity model
%
% global RTV2 RTDLNVDX RTDLNVDZ RTDG RTVRMS RTVAVE RTT RTZ
%
% G.F. Margrave, CREWES, June 2000
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

global RTV2 RTDLNVDX RTDLNVDZ RTDG RTVRMS RTVAVE RTT RTZ RTX

RTDG=dg;

if(nargin<3)
   z=((0:size(v,1)-1)')*dg;
end
if(nargin<4)
   x=(0:size(v,2)-1)*dg;
end

z=z(:);
x=x(:)';
   
%pad velocity model with four cells all around
npad=4;
nz=size(v,1);nx=size(v,2);
vp=zeros(nz+2*npad,nx+2*npad);
vp(npad+1:nz+npad,npad+1:nx+npad)=v;
%top
vp(1:npad,:)=ones(npad,1)*...
		[v(1,1)*ones(1,npad) v(1,1:nx) v(1,nx)*ones(1,npad)];
%bottom
vp(nz+npad+1:nz+2*npad,:)=ones(npad,1)*...
		[v(nz,1)*ones(1,npad) v(nz,1:nx) v(nz,nx)*ones(1,npad)];
%left
vp(npad+1:nz+npad,1:npad)=v(:,1)*ones(1,npad);
%right
vp(npad+1:nz+npad,nx+npad+1:nx+2*npad)=v(:,nx)*ones(1,npad);

RTV2=vp.^2;

[RTDLNVDX,RTDLNVDZ]=gradient(log(vp),dg,dg);

vm=mean(vp')';
%nz=length(vm);
RTZ=[z(1)-dg*((npad:-1:1)');z;z(nz)+dg*((1:npad)')];
RTX=[x(1)-dg*(npad:-1:1) x x(nx)+dg*(1:npad)];

RTT=vint2t(vm,RTZ);
RTVAVE=vint2vave(vm,RTT);
RTVRMS=vint2vrms(vm,RTT);

disp('velocity model successfully converted to global variables');