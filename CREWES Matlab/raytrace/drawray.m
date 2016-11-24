function [hray,xray]=drawray(v,z,z1,z2,x1,p,kol,lns,lw,d3)
% DRAWRAY: draws rays given their ray parameters
%
% [hray,xray]=drawray(v,z,z1,z2,x1,p,kol,lns,lw,d3)
%
% DRAWRAY draws the rays whose rayparameters are found in the vector
% p in the stratified medium defined by v & z. Rays are drawn in
% the current axes and their handles are returned.
%
% v,z ... vectors describing the stratified medium, (Interval 
%	velocity versus depth)
% z1 ... depth the ray starts at
% z2 ... depth the ray ends at.
% x1 ... vector of x coordinates the rays start at
% p ... vector of ray parameters
% kol ... color to draw the ray in
%  ******* default 'g' *********
% lns ... linestyle to darw with
%  ******* default '-' *********
% lw ... linewidth to draw the ray
%  ******* default .5 *********
% d3 ... if 1, then the ray will be drawn in front of the current figure
%  ******* default 0 *********
% hray ... vector of handles of the rays (one per element of p)
% xray ... vector of x coordinates that the rays end at
%
% G.F. Margrave, CREWES Project, Feb 2000
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

if(nargin<10) d3=0; end
if(nargin<9) lw=.5; end
if(nargin<8) lns='-';end
if(nargin<7) kol='g'; end
if(length(x1)==1)
	x1=x1*ones(size(p));
end

%preliminaries
 z=z(:);
 v=v(:);
 p=p(:)'; %make p a row vector
 nz=length(z);
 %adjust z1 and z2 so that they won't be exactly on layer boundaries
 zt=min([z1 z2]);
 if(zt==z1)
 	z1=z1+100000*eps;
 	z2=z2-100000*eps;
   	inverted=0;
 else
 	z1=z1-100000*eps;
 	z2=z2+100000*eps;
	inverted=1;
 end
 if( z1< z(1) | z2< z(1) )
 	error(' start or end depth outside model range');
 end

%determine layers propagated through
if(~inverted)
	 ind=find(z>z1);%should never be ==
	 if(isempty(ind))
	 	ibeg=nz;
	 else
	 	ibeg=ind(1);
	 end

	 ind=find(z<z2);%should never be ==
	 if(isempty(ind))
	 	iend=nz;
	 else
	 	iend=ind(end);
	 end
	 if(ibeg~=iend)
	 	 % these are the layers propagated thru
	 	iprop=ibeg:iend;
	 else
	 	iprop=ibeg;
 	 end
	v1=v(ibeg-1);
	v2=v(iend);
else
	 ind=find(z<z1);%should never be ==
	 if(isempty(ind))
	 	error('logic error');
	 else
	 	ibeg=ind(end);
	 end

	 ind=find(z>z2);%should never be ==
	 if(isempty(ind))
	 	iend=nz;
	 else
	 	iend=ind(1);
	 end
	 if(ibeg~=iend)
	 	 % these are the layers propagated thru
	 	iprop=ibeg:-1:iend;
	 else
	 	iprop=ibeg;
	 end
	v1=v(ibeg);
	v2=v(iend-1);
end
 
 
 zprop=[z1;z(iprop);z2];%propagation depths

 %the reason for the following difference in the way velocity is assigned
 %is because a particular v applies to the model BELOW its corresponding
 %depth
 if(~inverted) 
	vprop=[v1;v(iprop)];%last point (v2) is irrelevant
 else
	vprop=[v1;v(iprop-1)];%last point (v2) is irrelevant
 end
 
 %
 % sn is an n by m matrix where n is the length of iprop (the number of
 %    layers propagated through) and m is the length of p (the number of
 %    unique ray parameters to use). Each column of sn corresponds to a
 %    single ray parameter and contains the sin of the vertical angle;
 %
 sn = vprop*p;
 ind=find(sn>1);
 sn(ind)=nan*ones(size(ind));
 cs=sqrt(1-sn.*sn);
 vprop=vprop*ones(1,length(p));%make a matrix
 thk=abs(diff(zprop))*ones(1,length(p));
 %x=x1*ones(length(zprop),length(p));
 x=ones(length(zprop),1)*(x1(:)');
 x(2:length(zprop),:)=(thk.*sn)./cs;
 x=cumsum(x);
 zr=zprop*ones(1,length(p));
 
 if(d3==1)
	zz=ones(size(zr));
 else
	zz=zeros(size(zr));
 end
 hray=line(x,zr,zz,'color',kol,'linestyle',lns,'linewidth',lw);
 xray=x(end,:);