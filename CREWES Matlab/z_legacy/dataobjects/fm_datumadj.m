function fmout=fm_datumadj(fmat,xd,yd,xdold,ydold)

% fmout=fm_datumadj(fmat,xd,yd,xdold,ydold)
% fmout=fm_datumadj(fmat,xd,yd)
%
% FM_DATUMADJ performs datum adjustment on a fleximat.
% The traces in the input fleximat are looped over, stripped of
% nans on the beginning and end, datum shifted by the shift for
% the particular column, and stored in the output fleximat. 
% Note that shifts are rounded to the nearest sample to avoid
% costly resampling of each trace. The datum shift is accomplished
% by removing the old datum and installing the new one. The
% installation of a datum is done
% by subtracting the y coordinate of the datum at the column 
% x coordinate from the column y coordinates.
% Datum removal is the opposite of datum installation.
%
% Default for the old datum is all zeros
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

if(nargin<4)
	xdold=xd;
	ydold=zeros(size(xd));
end

%unpack the input
	y=fmget(fmat,'y');
	x=fmget(fmat,'x');
	mat=fmget(fmat,'mat');

	%sort x
	[x,is]=sort(x);

	fmout=[];

	%compute the shifts
	%first the shifts to remove the old datum
	dy=y(2)-y(1);
	delyold=interpextrap(xdold,ydold,x,0);
	delyold=dy*round(delyold/dy);
	%now to install the new one
	dely=interpextrap(xd,yd,x,0);
	dely= dy*round( dely/dy );

	%combine the shifts
	dely=dely-delyold;

	for k=1:length(x)
		trc=mat(:,is(k));

		ind=find(~isnan(trc));
		i1=min(ind);
		i2=max(ind);

		trc=trc(i1:i2);
		yout=y(i1:i2)-dely(k);

		fmout=fmset(fmout,x(k),yout,trc);
	end