function [t,p]=traceray_ps(vp,zp,vs,zs,zsrc,zr,zd,x,xcap,pfan,itermax,optflag,pflag,dflag)
%
% [t,p]=traceray_ps(vp,zp,vs,zs,zsrc,zr,zd,x,xcap,pfan,itermax,optflag,pflag,dflag)
%
% TRACERAY_PS uses the modified bisection algorithm 
% to trace a p-s reflection 
% in a stratified medium.
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

if(nargin<14)
	dflag=0;
end
if(nargin<13)
	pflag=0;
end
if(nargin<12)
	optflag=1;
end

if(nargin<11)
	itermax=4;
end
if( nargin<10)
	pfan=-1;
end

if(~prod(size(vp)==size(zp)))
	error('vp and zp must be the same size');
end
if(~prod(size(vs)==size(zs)))
	error('vp and zp must be the same size');
end

if(length(zsrc)~=1 | length(zr)~=1 | length(zd)~=1 | length(xcap)~=1 )
	error(' zsrc,zr,zd and xcap must be scalars ')
end

ind=find(x<0);
if(~isempty(ind));
	error('offsets must be nonnegative');
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

 %determine pmax
 vmax=max([vmod]);
 nearlyone=.999;
 pmax=nearlyone/vmax;
 
 %the meaning of pmax is that it is the maximum ray parameter which will not
 %be critically refracted anywhere in vp or vs.

if(pfan==-2)
	global PFAN
	if(isempty(PFAN))
	  pfan=-1;
	else
	  pfan=PFAN;
	  PFAN=-2;
	end
else
	PFAN=0;
end

if(pfan==-1)
	%shoot a fan of rays Iterate once if the fan does not span the xrange
	%guess angle range
	xmaxf=max(x);
	xminf=min(x);
	if(xminf~=xmaxf)
		nrays=3*length(x);
		xfan=linspace(xminf,xmaxf,nrays);
		th=atan(xfan/(z2-z1));
		%determine the fan ray parameters
	
		pfan=sin(th)/vmod(1);
		
		%put in a max and min p
		if(min(pfan)>0)
			pfan=[0 pfan];
			xfan=[0 xfan];
			nrays=nrays+1;
		end	
		
		if(max(pfan)>pmax)
			ind=find(pfan<pmax);
			pfan=pfan(ind);
			xfan=xfan(ind);
			nrays=length(ind);
		end
	
		if(max(pfan)<pmax)
			pfan=[pfan pmax];
			xfan=[xfan (z2-z1)*nearlyone/sqrt(1-nearlyone*nearlyone)];
			nrays=nrays+1;
		end
		
		xminf=2.0*min(xfan);
		xmaxf=2.0*max(xfan);
	else
		%3 rays 1 x
		th=atan(xmaxf/(z2-z1)); %straight ray to xmax
		nrays=3;
		po=sin(th)/vmod(1);
		if(po>pmax)
			po=pmax/2;
		end
		pfan=[0 po pmax];
		th=asin(pfan*vmod(1));
		xfan=(z2-z1)*tan(th);
		xminf=2.0*min(xfan);
		xmaxf=2.0*max(xfan);
	end

else

	%make sure its a row vector
	ind=find(isnan(pfan));
	if(~isempty(ind))
		pfan(ind)=[];
	end
	pfan=pfan(:)';
	pfan=sort(pfan);
	ind=find(pfan==0);
	if(isempty(ind))
		pfan=[0 pfan];
	end
	ind=find(pfan>=pmax);
	if(~isempty(ind))
		pfan(ind)=[];
	end
	pfan=[pfan pmax];
	nrays=length(pfan);
end


%shoot first fan
% 
[xx,tt]=shootray(vmod,zmod,pfan);

xmax=max(xx);
xmin=min(xx);

%see if we have spanned the x range and revise if needed
imin=surround(xx,min(x));
imax=surround(xx,max(x));
newfan=0;
if(isempty(imax))
	if(max(pfan)<pmax)
		pm= .5*(max(pfan)+pmax);
		pfan=[pfan pm];
		imax=length(pfan)-1;
		newfan=1;
	end
end
if(~isempty(imax))
	pfan=pfan(imin:imax+1);
end

%shoot new fan (if p changed)
if(length(pfan)~=nrays | newfan)
	% first p
	[xx,tt]=shootray(vmod,zmod,pfan);
	% invoke symmetry to double these
	xmax=max(xx);
	xmin=min(xx);
end

%
% loop over x
%	-1 find xx which brackets x(k)
%	-2 shoot a finer fan of rays
%	-3 find the rays which bracket x(k)
%	-4 repeat 2 and 3 until a ray is captured at x(k)
%

t=inf*ones(size(x));
p=nan*ones(size(x));

for k=1:length(x)

	ind=surround(xx,x(k));
	if(isempty(ind))
		if(x(k)>=xmax) %test for off high end
			i1=length(xx);
			i2=[];
		else
			i1=[]; %off low end
			i2=1;
		end
	else
		if(length(ind)>1) %use first arrival
			it=find(tt(ind)==min(tt(ind)));
			ind=ind(it);
		end
		i1=ind;
		i2=ind+1;
	end
	
	captured=0;
	hopeless=0;
	iter=0;
	x1=xx(i1); x2=xx(i2);
	t1=tt(i1); t2=tt(i2);
	p1=pfan(i1);  p2=pfan(i2);
	if( isempty(i2) & max(pfan)==pmax)
		hopeless=1;
	end
	
	while(~captured & iter<itermax & ~hopeless)
		
		iter=iter+1;
	
		%test for capture
		xtst1=abs(x(k)-x1);
		xtst2=abs(x(k)-x2);
		if( xtst1< xcap & xtst1<=xtst2)
			captured=1;
			p(k)=p1;
			t(k)=t1;
			%final adjustment
			if(optflag)
				%linear interpolation
				t(k)= t1 + (x(k)-x1)*(t2-t1)/(x2-x1);
				p(k)= p1 + (x(k)-x1)*(p2-p1)/(x2-x1);
			end
					
			%disp([' capture on iter= ' int2str(iter)])
		elseif( xtst2 < xcap & xtst2<=xtst1)
			captured=1;
			p(k)=p2;
			t(k)=t2;
			%final adjustment
			if(optflag)
				%linear interpolation
				t(k)= t2 + (x(k)-x2)*(t1-t2)/(x1-x2);
				p(k)= p2 + (x(k)-x2)*(p1-p2)/(x1-x2);
			end
			
			%disp([' capture on iter= ' int2str(iter)])
		end
		
		%shoot a new fan
		if(~captured)
			%end cases first
			if(isempty(i1))
				p1=0;
			elseif(isempty(i2))
				p2=.5*(p2+pmax);
			end
			dp= (x(k)-x1)*(p2-p1)/(x2-x1);
			p0= p1+ dp;
			p2= min([p1+2*dp, pmax]);
			p1= p1+.5*dp;
			pnew=[p1 p0 p2];
			
			if(isempty(pnew))
				hopeless=1;
			else
				%shoot and check for nans
				[xxnew,ttnew]=shootray(vmod,zmod,pnew);
	
				xmx=max(xxnew);
				xmn=min(xxnew);
				
				%analyze the fan. see if we have bracketed our target
				ind=surround(xxnew,x(k));
				if(isempty(ind))
					if(~isempty(xmx))
						if(x(k)>=xmx)
							n2=2*(x(k)-xxnew(2))/(xxnew(3)-xxnew(2));
							p2=min([p1+n2*dp, pmax]);
							pnew(3)=p2;
						end
						[xxnew,ttnew]=shootray(vmod,zmod,pnew);
	
						xmx=max(xxnew);
						xmn=min(xxnew);
						ind=surround(xxnew,x(k));
						if(isempty(ind));
							i1=3;
							i2=[];
						else
							i1=ind;
							i2=ind+1;
						end
					else
						i1=[];
						i2=1;
					end

				else
					if(length(ind)>1) %use first arrival
						it=find(ttnew(ind)==min(ttnew(ind)));
						ind=ind(it);
					end
					i1=ind;
					i2=ind+1;
				end
				if( isempty(i2) & max(pnew)==pmax)
					hopeless=1;
				else
					x1=xxnew(i1); x2=xxnew(i2);
					t1=ttnew(i1); t2=ttnew(i2);
					p1=pnew(i1);  p2=pnew(i2);
				end
			end
		end
		
	end
	%end the while loop
	if(~captured & pflag)
		disp([' capture failed for zsrc,zr,zd= ' num2str(zsrc) ',' num2str(zr) ...
		',' num2str(zd) ' and x= ' num2str(x(k))]);
	end
	
	%end the for loop
end

if(PFAN)
	PFAN=pfan;
end


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