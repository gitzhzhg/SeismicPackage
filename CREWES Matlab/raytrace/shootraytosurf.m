function [tray,r]=shootraytosurf(dt,tmax,r0)
% SHOOTRAYTOSURF: shoot a ray to z=0 in v(x,z)
%
% [tray,r]=shootraytosurf(dt,tmax,r0)
%
% Ray tracer for a 2D gridded velocity model. The model
% is built by defining a matrix of velocity values and calling
% RAYVELMOD. This establishes 3 global matrices containing v^2
% and the logarithmic derivatives of v with respect to x and z.
% (The velocity model matrix may be deleted after calling RAYVELMOD
% if memory is limited.) The raytracer implements an RK4 (4th order 
% Runge-Kutta) solution to the ray equations and, by default, uses
% nearest neighbor interpolation.  
% Rays will be terminated when they reach the maximum time or leave the 
% bounds of the velocity model.
%
% dt ... size of time step
% tmax ... maximum time (aborts at this time)
% r0 ... initial values of the ray vector. r0(1) and r0(2) are the
%        starting x and z coordinates and r0(3) and r0(4) are the
%        horizontal and vertical slownesses.
% tray ... output time vector. If the ray stays within the bounds of
%        the model, this is the same as tstep, otherwise it may be
%        smaller.
% r ... output ray matrix. This is an N by 4 matrix, where N=length(t),
%       with each row being a ray vector for the corresponding time. The
%       ray vectors are as described in r0 above.
%
% G.F. Margrave and P.F. Daley, CREWES, June 2000
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

global RTV2 RTDLNVDX RTDLNVDZ RTDG BILINEAR RTZ RTX

if(isempty(RTDG))
	error('velocity model not defined. Use RAYVELMOD')
end


[m,n]=size(RTV2);
xmax=RTX(n-2);% set to abort within 2 dg of edge
zmax=RTZ(m-2);%
xmin=RTX(3);
zmin=RTZ(3);

r0=r0(:)';

nt=round(tmax/dt)+1;
tray=nan*zeros(nt,1);

r=zeros(nt,length(r0));
r(1,:)=r0;
tray(1)=0;
znow=r0(2);
tnow=-dt;
sides=0;
top=0;
k=1;

%determine tolerances
vmin=sqrt(min(min(RTV2)));
ztol=.5*dt*vmin;

while(znow > ztol & tnow<tmax)
   k=k+1;
   tnow=tnow+dt;
   tray(k)=tnow;
   rnow=r(k-1,:);

   %k1
   xx=(rnow(1)-RTX(1))/RTDG+1; zz=(rnow(2)-RTZ(1))/RTDG+1;% Actual fractional grid
   %nearest neighbor interpolation
   v2=RTV2(round(zz),round(xx));
   dlnvdx=RTDLNVDX(round(zz),round(xx));
   dlnvdz=RTDLNVDZ(round(zz),round(xx));
   k1=dt*[v2*rnow(3),v2*rnow(4),-dlnvdx,-dlnvdz];

   %k2
   rk=rnow+.5*k1;
   xx=(rk(1)-RTX(1))/RTDG+1;zz=(rk(2)-RTZ(1))/RTDG+1;
   v2=RTV2(round(zz),round(xx));
   dlnvdx=RTDLNVDX(round(zz),round(xx));
   dlnvdz=RTDLNVDZ(round(zz),round(xx));
   k2=dt*[v2*rk(3),v2*rk(4),-dlnvdx,-dlnvdz];

   %k3
   rk=rnow+.5*k2;
   xx=(rk(1)-RTX(1))/RTDG+1;zz=(rk(2)-RTZ(1))/RTDG+1;
   v2=RTV2(round(zz),round(xx));
   dlnvdx=RTDLNVDX(round(zz),round(xx));
   dlnvdz=RTDLNVDZ(round(zz),round(xx));
   k3=dt*[v2*rk(3),v2*rk(4),-dlnvdx,-dlnvdz];

   %k4
   rk=rnow+k3;
   xx=(rk(1)-RTX(1))/RTDG+1;zz=(rk(2)-RTZ(1))/RTDG+1;
   v2=RTV2(round(zz),round(xx));
   dlnvdx=RTDLNVDX(round(zz),round(xx));
   dlnvdz=RTDLNVDZ(round(zz),round(xx));
   k4=dt*[v2*rk(3),v2*rk(4),-dlnvdx,-dlnvdz];

   %solve
	r(k,:)=rnow+k1/6+k2/3+k3/3+k4/6;

	%renormalize p and q
	vapp=(r(k,3)^2+r(k,4)^2)^(-.5);
	r(k,3)=r(k,3)*vapp/sqrt(v2);
	r(k,4)=r(k,4)*vapp/sqrt(v2);

	znow=r(k,2);
	if((r(k,1)>xmax & r(k,3)>0) | (r(k,1)<xmin & r(k,3)<0))
		sides=1;
		break;
	end
	if(r(k,2)<zmin )
		top=1;
		break;
	end
end

%extrapolate the final values
if(tnow<tmax & ~sides)
	delz=znow;p=r(k,3);q=r(k,4);
	delx=delz*p/q;
	x0=r(k,1)+delx;
	ix=near(RTX,x0);
	v02=RTV2(1,ix);
	delt=delz/(abs(q)*v02);
	tray(k+1)=tray(k)+delt;
	r(k+1,:)=r(k,:)+delt*[v02*p v02*q -RTDLNVDX(1,ix) -RTDLNVDZ(1,ix)];

	r(k+2:end,:)=[];
	tray(k+2:end)=[];
end

if(sides)
	disp('ray failed, hit sides of model')
	r(k+1:end,:)=[];
	tray(k+1:end)=[];
end

if(tnow>=tmax)
	disp('ray failed, max time reached')
end