function [t,p,x,L]=angleray_ps(vp,zp,vs,zs,zsrc,zr,zd,angles,dflag)
% ANGLERAY_PS: traces a P-S (or S-P) reflection for v(z)
%
% [t,p,L]=angleray_ps(vp,zp,vs,zs,zsrc,zr,zd,x,xcap,pfan,itermax,optflag,pflag,dflag)
%
% Angleray_ps traces a fan of p-s rays such that each one corresponds to a
% desired incidence angle at the specified reflector depth.  The method is
% not iterative since the ray parameters are known apriori. Returned values
% are traveltimes, ray parameters, offsets, and spreading factors for each
% desired incidence angle.
%
% vp,zp   = velocity and depth vectors giving the p wave model. Depths are 
% 			considered to be the tops of homogeneous layers.
% vs,zs   = velocity and depth vectors giving the s wave model. 
%  for both models, depths are considered to be the tops of homogeneous layers. Thus,
%  the first velocity  applies from the first depth to the second. A constant velocity
%  would be specified like vp=1500;zp=0; etc.  
% zsrc  	  = depth of the source (scalar)
% zr		  = depth of receiver (scalar)
% zd      = depth of the reflection (scalar)
% x       = vector of desired source-receiver offsets (horizontal)
%           MUST be non-negative numbers
% xcap 	  = (scalar) capture radius
% pfan    = vector of ray parameters to use for the initial fan of rays
%	    By default, the routine generates the fan using straight rays. 
%	    Setting pfan to -1 or omitting it activates default behavior.
%           Setting pfan to -2 causes the routine to use the pfan used in the
%           last call to this program. (pfan of -1 and -2 are identical on the
%	    first call.) 
% itermax = maximum number of iterations allowed for ray capture
% =========================== Default = 4 ================================
% optflag = if nonzero then refine captured rays by linear interpolation
% =========================== Default = 1 ================================
% pflag   = if nonzero, then print information about all failed rays
% =========================== Default = 0 ================================
% dflag   = 0 no action
%			 = 1 then a new figure window will be opened and the raypaths plotted
% 			 = 2 raypaths will be plotted in the current window
% =========================== Default = 0 ================================
% NOTE: All z variables must be specified relative to a common datum
%
% t 	  = vector of traveltimes for each x
% p 	  = vector of ray parameters for each x
% L     = vector of geometrical spreading factors for each x
%     (if L is not asked for, it will not be calculated)
%
% G.F. Margrave, CREWES Project, June 1995
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

if(nargin<9)
	dflag=0;
end


if(~prod(double(size(vp)==size(zp))))
	error('vp and zp must be the same size');
end
if(~prod(double(size(vs)==size(zs))))
	error('vs and zs must be the same size');
end

if(length(zsrc)~=1 | length(zr)~=1 | length(zd)~=1)
	error(' zsrc,zr, and zd must be scalars ')
end

ind=find(any(angles)>90, 1);
if(~isempty(ind))
	error('angles must be less than 90 degrees');
end

%make sure zsrc < Zd and zr<zd
if(zsrc > zd )
	error(' zsrc must be less than zd');
end
if(zr > zd )
	error(' zr must be less than zd');
end

%adjust zsrc,zr,and zd and z2 so that they won't be exactly on layer boundaries
 	zsrc=zsrc+100000*eps;
 	zr=zr+100000*eps;
 	zd=zd-100000*eps;

 if( zsrc < zp(1) | zsrc < zs(1))
 	error(' source depth outside model range');
 elseif(zr < zp(1) | zr < zs(1) )
	error('receiver depth outside model range');
 end
 
 %determine the layers we propagate through
 %down leg
 ind=find(zp>zsrc);
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
     t=inf*ones(size(x));
     p=nan*ones(size(x));
     return;
 end
 
 %create v and z arrays for down leg
 vp=vp(:);zp=zp(:);
 vpd=[vp(ibegd:iendd);vp(iendd)];%last v is irrelevent.
 zpd=[zsrc;zp(ibegd+1:iendd);zd];

%up leg
 ind=find(zs>zr);
 if(isempty(ind))
 	ibegu=length(zs);
 else
 	ibegu=ind(1)-1;
 end
 ind=find(zs>zd);
 if(isempty(ind))
 	iendu=length(zs);
 else
 	iendu=ind(1)-1;
 end
 
  %test for sensible layer indices
 if(iendu<1 | iendu > length(zs) | ibegu <1 | iendu > length(zs))
     %somethings wrong. return nans
     t=inf*ones(size(x));
     p=nan*ones(size(x));
     return;
 end
 
 %create v and z arrays for up leg
 vs=vs(:);zs=zs(:);
 vsu=[vs(ibegu:iendu);vs(iendu)];%last v is irrelevent.
 zsu=[zr;zs(ibegu+1:iendu);zd];
 
 % combine into one model. We shoot oneway rays through an
 % equivalent model consiting of the down-leg model followed by
 % the upleg model (upside down).
 vmod=[vpd(1:end-1);flipud(vsu(1:end-1))];
 tmp=flipud(zsu);
 zmod=[zpd;cumsum(abs(diff(tmp)))+zpd(end)];
 z1=zsrc;%start depth
 z2=max(zmod)+100000*eps;%end depth

%determine the p values that give the desired incidence angles
 p=sind(angles)./vp(iendd);


%shoot the rays
% 
[x,t]=shootray(vmod,zmod,p);

if(dflag)
   if(dflag==1)
		figure;
   end
	[hray,xray]=drawray(vpd,zpd,zsrc,zd,0,p,'k');
	[hray2,xray2]=drawray(vsu,zsu,zd,zr,xray,p,'r');
	
   if(dflag==1)
		flipy;xlabel('offset');ylabel('depth');
		if(length(x)>1)
			dx=x(2)-x(1);
		else
			dx=100;
		end
		axis([min([x(:);0])-dx max([x(:);0])+dx min(zp) max([zp;zsrc;zr;zd])])
   end
end

if(nargout>3)
   L=sphdiv(vmod,zmod,p);
end