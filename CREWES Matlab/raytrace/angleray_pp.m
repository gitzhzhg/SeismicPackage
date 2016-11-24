function [t,p,x,L]=angleray_pp(vp,zp,zs,zr,zd,angles,dflag)
% ANGLERAY_PP: traces a P-P (or S-S) reflection at fixed incidence angle for v(z)
%
% [t,p,x,L]=angleray_pp(vp,zp,zs,zr,zd,angles,dflag)
%
% Angleray_PP traces a fan of rays such that each one corresponds to a
% desired incidence angle at the specified reflector depth.  The method is
% not iterative since the ray parameters are known apriori. Returned values
% are traveltimes, ray parameters, offsets, and spreading factors for each
% desired incidence angle.
%
% vp,zp   = velocity and depth vectors giving the p wave model. Depths are 
% 			considered to be the tops of homogeneous layers. 
% 	    Can also be used for an s-s reflection
% zs  	  = depth of the source (scalar)
% zr		  = depth of receivers (scalar)
% zd      = depth of the reflection (scalar)
% angles  = vector of desired incidence angles (degrees). These define the
%        ray parameters by p=sind(angle)/vd where vd is the velocity above
%        zd.
% dflag   = 0 no action
%			 = 1 then a new figure window will be opened and the raypaths plotted
% 			 = 2 raypaths will be plotted in the current window
% =========================== Default = 0 ================================
%
% t 	  = vector of traveltimes for each angle
% p 	  = vector of ray parameters for each angle
% x       = vector of offsets for each angle
% L     = vector of geometrical spreading factors for each angle
%     (if L is not asked for, it will not be calculated)
%
% NOTE: failed rays are flagged with inf (infinity) for traveltime and 
%      nan for ray parameter.
%
% G.F. Margrave, CREWES Project, April 2013
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

if(nargin<7)
    dflag=0;
end
if(~prod(double(size(vp)==size(zp))))
	error('vp and zp must be the same size');
end

if(length(zs)~=1 | length(zr)~=1 | length(zd)~=1 )
	error(' zs,zr, and zd must be scalars ')
end

ind=find(any(angles)<0, 1);
if(~isempty(ind))
	error('angles must be nonnegative');
end

ind=find(any(angles)>90, 1);
if(~isempty(ind))
	error('angles must be less than 90 degrees');
end

%make sure zs < Zd and zr<zd
if(zs > zd )
	error(' zs must be less than zd');
end
if(zr > zd )
	error(' zr must be less than zd');
end

%adjust zs,zr,and zd and z2 so that they won't be exactly on layer boundaries
 	zs=zs+100000*eps;
 	zr=zr+100000*eps;
 	zd=zd-100000*eps;

 if( zs < zp(1) )
 	error(' source depth outside model range');
 elseif(zr < zp(1))
	error('receiver depth outside model range');
 end
 
 %determine the layers we propagate through
 %down leg
 ind=find(zp>zs);
 if(isempty(ind))
 	ibegd=length(zp);
 else
 	ibegd=ind(1)-1;
 end
 ind=find(zp>zd);
 if(isempty(ind))
 	iendd=length(zp);
 else
 	iendd=ind(1)-1;
 end
 
 %test for sensible layer indices
 if(iendd<1 | iendd > length(zp) | ibegd <1 | iendd > length(zp))
     %somethings wrong. return nans
     t=inf*ones(size(angles));
     p=nan*ones(size(angles));
     x=p;
     return;
 end
 
 %create v and z arrays for down leg
 vp=vp(:);zp=zp(:);
 vpd=[vp(ibegd:iendd);vp(iendd)];%last v is irrelevent.
 zpd=[zs;zp(ibegd+1:iendd);zd];

%up leg
 ind=find(zp>zr);
 if(isempty(ind))
 	ibegu=length(zp);
 else
 	ibegu=ind(1)-1;
 end
 ind=find(zp>zd);
 if(isempty(ind))
 	iendu=length(zp);
 else
 	iendu=ind(1)-1;
 end
 
 %test for sensible layer indices
 if(iendu<1 | iendu > length(zp) | ibegu <1 | iendu > length(zp))
     %somethings wrong. return nans
     t=inf*ones(size(angles));
     p=nan*ones(size(angles));
     x=p;
     return;
 end
 
 %create v and z arrays for up leg
 vpu=[vp(ibegu:iendu);vp(iendu)];%last v is irrelevent.
 zpu=[zr;zp(ibegu+1:iendu);zd];
 
 % combine into one model. We shoot oneway rays through an
 % equivalent model consisting of the down-leg model followed by
 % the upleg model (upside down).
 vp1=[vpd(1:end-1);flipud(vpu(1:end-1))];
 %vp1=[vpd;flipud(vpu(1:end-1))];
 tmp=flipud(zpu);
 zp1=[zpd;cumsum(abs(diff(tmp)))+zpd(end)];
 %zp1=[zpd;cumsum(abs(diff(tmp(1:end-1))))+zpd(end)];
 z1=zs;%start depth
 z2=max(zp1)+100000*eps;%end depth
 
 %determine the p values that give the desired incidence angles
 p=sind(angles)./vp(iendd);

%shoot the rays
% 
[x,t]=shootray(vp1,zp1,p);

if(dflag)
	if(dflag==1)
		figure;
	end
	[hray,xray]=drawray(vpd,zpd,zs,zd,0,p,'k');
	[hray2,xray2]=drawray(vpu,zpu,zd,zr,xray,p,'k');
	if(dflag==1)
	    %#function flipy
		flipy;xlabel('offset');ylabel('depth');
		if(length(x)>1)
			dx=x(2)-x(1);
		else
			dx=100;
		end	
		axis([min([x(:);0])-dx max([x(:);0])+dx min(zp) max([zp;zs;zr;zd])])
	end
end

if(nargout>3)
   L=sphdiv(vp1,zp1,p);
end