function ylogout=log_datumadj(ylog,xlog,xd,yd,xdold,ydold)
% ylogout=log_datumadj(ylog,xlog,xd,yd,xdold,ydold)
% ylogout=log_datumadj(ylog,xlog,xd,yd)
%
% LOG_DATUMADJ performs datum adjustment on a log's vertical coordinates.
% The datum shift is accomplished
% by removing the old datum and installing the new one. The
% installation of a datum is done
% by subtracting the y coordinate of the datum at the logs x coordinate
% from the log's y coordinate vector.
% Datum removal is the opposite of datum installation.
% Datum shifts are rounded to the nearest whole sample (sample rate is 
%	taken to be ylog(2)-ylog(1) )
%
% ylog ... vector of y (vertical) coordinates for log
% xlog ... scalar giving the logs x coordinate
% (xd,yd) ... piecewise linear specification of the new datum
% (xdold,ydold) ... piecewise linear specification of the old
%		datum
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
if(nargin< 5 )
	xdold=xd;
	ydold=zeros(size(xd));
end
	%compute the shifts
	%first the shifts to remove the old datum
	dy=ylog(2)-ylog(1);
	delyold=interpextrap(xdold,ydold,xlog,0);
	delyold=dy*round(delyold/dy);
	%now to install the new one
	dely=interpextrap(xd,yd,xlog,0);
	dely= dy*round( dely/dy );
	%combine the shifts
	dely=dely-delyold;
	ylogout=ylog-dely;