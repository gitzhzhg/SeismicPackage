function amat=event_polyh(amat,t,x,v,xhyp,zhyp,ahyp,flag,dx,noversamp)
% EVENT_DIPH: construct a dipping event by diffraction superposition
%
% amat=event_polyh(amat,t,x,v,xhyp,zhyp,ahyp,flag,dx,noversamp)
%
% EVENT_POLYH inserts a polygonal event in a matrix. The event
% is constructed from a superposition of hyperbolae.
%
% amat ... the matrix of size nrows by ncols
% t ... vector of length nrows giving the matrix t coordinates
% x ... vector of length ncols giving the matrix x coordinates
% v ... velocity (scalar)
% xhyp ... vector of x coordinates of hyperbolae
% zhyp ... vector of z coordinates of hyperbolae
% ahyp ... vector of amplitudes of hyperbolae
% flag ... if 1, then amplitudes are divided by cos(theta)
%				otherwise no effect
%      ******** default = 1 *********
% dx ... nominal spacing of hyperbolae. Between each pair of xhyp, hyperbolae
%		will be placed at this horizontal increment if the local dip is zero. For
%		nonzero dip the horizontal spacing is dx*cos(theta).
%      ******** default = abs(x(2)-x(1)) ********
% noversamp ... each output trace is created by nearest-neighbor interpolation into
%		a temporary over-sampled trace that is then properly resampled. The greater the
%		oversampling, the better the result.
%      ******** default = 10 *********
%
% G.F. Margrave, CREWES, 2000
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

if(nargin<10) noversamp=10; end
if(nargin<9) dx=abs(x(2)-x(1)); end
if(nargin<8) flag=1; end

%loop over columns
%nc= between(xlims(1),xlims(2),x,2);
[nr,nc]=size(amat);

v=v/2;%exploding reflector

tmin=t(1);
tmax=t(length(t));
dt=t(2)-t(1);
dt2=dt/noversamp;
ttmp=tmin:dt2:tmax;

costheta=ones(size(xhyp));
if(flag==1) 
	tantheta=diff(zhyp)./diff(xhyp);
	costh=sqrt(1 ./(1+tantheta.^2));
	%costheta(2:end-1)=.5*(costh(1:end-1)+costh(2:end));
	%costheta(1)=costheta(2);costheta(end)=costheta(end-1);
	costheta(1:end-1)=costh;
	costheta(end)=costh(end);
end

% costheta is intended to be piecewise constant. That is, costheta(k)
% applies to all x such that xhyp(k)<= x < xhyp(k-1)

%ahyp=ahyp./costheta;

%resample such that the samples are dx apart measured along dip

%estimate number of samples needed
dxest=dx*mean(costheta);
nxest=round(2*abs(xhyp(end)-xhyp(1))/dxest);
xh2=nan*1:nxest;

%calculate new x locations
nx2=0;
for k=1:length(xhyp)-1
	x1=xhyp(k);x2=xhyp(k+1);
	dir=sign(x2-x1);
	dxk=dx*costheta(k)*dir;
	if(dir*(x1+dxk)<dir*x2)
		xnext=x1;
		while dir*xnext<dir*x2
			nx2=nx2+1;
			xh2(nx2)=xnext;
			xnext=xnext+dxk;
		end
	else
		nx2=nx2+1;
		xh2(nx2)=x1;
	end
end

ind=find(isnan(xh2));
if(~isempty(ind))
	xh2(ind(1):end)=[];
end

%interpolate depths, amplitudes, and dips
%zh2=interp1(xhyp,zhyp,xh2);
%ah2=interp1(xhyp,ahyp,xh2);
zh2=pwlint(xhyp,zhyp,xh2);
ah2=pwlint(xhyp,ahyp,xh2);
csth2=pcint(xhyp,costheta,xh2);

%adjust amplitudes for dip
ah2=ah2./csth2;
			
tnot=zh2/v;	

for k=1:nc %loop over traces
	trctmp=zeros(size(ttmp))';
	tk=sqrt(tnot.^2+((x(k)-xh2)/v).^2);
	ak=ah2.*tnot.*(tk.^(-1.5));
	ind2=between(tmin,tmax,tk);
	if(ind2~=0)
		ik=round(tk(ind2)/dt2)+1;
		for kh=1:length(ik)
			trctmp(ik(kh))=trctmp(ik(kh))+ak(ind2(kh));
		end
	
		%amat(:,k)=amat(:,k) + resample(trctmp,1,noversamp)/costheta;
		amat(:,k)=amat(:,k) + resample(trctmp,1,noversamp);
	end
end