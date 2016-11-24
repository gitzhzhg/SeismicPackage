function amat=event_hyp(amat,t,x,tnot,xnot,v,amp,flag,aper)

% amat=event_hyp(amat,t,x,tnot,xnot,v,amp,flag,aper)
%
% EVENT_HYP inserts a hyperbolic event in a matrix.
%
% amat ... the matrix of size nrows by ncols
% t ... vector of length nrows giving the matrix t coordinates
% x ... vector of length ncols giving the matrix x coordinates
% tnot ... t coordinate of the hyperbola apex
% xnot ... x coordinate of the hyperbola apex
% v ... velocity of hyperbola
% amp ... amplitude at the apex
% flag ... determines amplitude decay on hyperbola
%		0 -> no decay
%		1 -> decay as to/tx
%		2 -> decay as to/(tx^2)
% ******** default is 1 ********
% aper ... truncate hyperbola beyond this aperture
% ******** default is no truncation ********
%
% G.F. Margrave, CREWES Project, University of Calgary, 1996
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
if(nargin<8)
	flag=1;
end
if( nargin < 9 )
	aper = inf;
end

%loop over columns
[nsamp,nc]=size(amat);

dt=t(2)-t(1);
tmin=t(1);
for k=1:nc
	xoff=x(k)-xnot;
	if(abs(xoff) < aper)
		tk = sqrt(tnot^2+(xoff/v)^2);
		a=amp;
		if(flag==1)
			a = tnot*a/tk;
		elseif(flag==2)
			a = tnot*a/(tk*tk);
		end
		ik=(tk-tmin)/dt+1;
		if( between(1,nsamp,ik) )
			ik1=floor(ik);
			ik2=ceil(ik);
			if(ik1==ik2)
				amat(ik1,k)=amat(ik1,k)+a;
			else
				amat(ik1,k)=amat(ik1,k)+a*(ik-ik2)/(ik1-ik2);
				amat(ik2,k)=amat(ik2,k)+a*(ik-ik1)/(ik2-ik1);
			end
		end
	end
end