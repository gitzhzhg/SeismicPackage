function amat=event_dip(amat,t,x,tlims,xlims,amp)
% EVENT_DIP: inserts a dipping (linear) event in a matrix
%
% amat=event_dip(amat,t,x,tlims,xlims,amp)
%
% EVENT_DIP inserts a dipping (linear) event in a matrix.
%
% amat ... the matrix of size nrows by ncols
% t ... vector of length nrows giving the matrix t coordinates
% x ... vector of length ncols giving the matrix x coordinates
% tlims ... vector of length 2 giving the t limits of the event
% xlims ... vector of length 2 giving the x limits of the event
% amp ... vector of length 2 giving the amplitudes at either end
%	of the event. Intermediate amplitudes are interpolated in x.
%   *************** default [1 1] *****************
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

if(nargin<6)
    amp=[1 1];
end

%loop over columns
nc= between(xlims(1),xlims(2),x,2);

spike=zeros(length(t),1);
spike(1)=1;

if(length(amp)==1) amp=[amp amp]; end

if(nc~=0)
	tmin=t(1);
	tmax=t(length(t));
	dt=t(2)-t(1);
	for k=nc
		tk = interp1(xlims,tlims,x(k));
		a = interp1(xlims,amp,x(k));
		if( between(tmin,tmax,tk) )
			amat(:,k)=amat(:,k) + a*stat(spike,t,tk-tmin);
		end
	end
end