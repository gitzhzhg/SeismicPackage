function y0=internan(x,y,x0)
% y0=interpnan(x,y,x0)
%
% INTERP is similar to MATLAB's INTERP1 unless x contains nans.
% If nans are present, then they are assumed to mark distinct segments
% of x which are handled independently. Each such segment must obey the
% usual restrictions imposed by interp1 (i.e. monotonic) but there need
% be no relation between the segments.
% If x contains no nans, there is still a difference between this and
% interp1. The latter will bomb if any x0 lie outside the range of x.
% This routine will not bomb and returns nans instead. Note that the
% actual interpolator used is lint not interp1 because lint is more 
% efficient when length(x0) >> length(x) .
%
% G.F. Margrave Jan 1995
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
y0=nan*ones(size(x0));
%test for segments
ind=find(isnan(x));
ind=[0 ind length(x)+1];
nsegs=length(ind)-1;
for k=1:nsegs
	%determine indicies for the segment
	iseg=ind(k)+1:ind(k+1)-1;
	if(length(iseg))% severl nans in a row produce zero length segments
		%now determine which x0 sites lie within the current segment
		ido=between(x(iseg(1)),x(iseg(length(iseg))),x0);
		if(~isempty(ido) & ido ~= 0 )
			y0(ido)=lint(x(iseg),y(iseg),x0(ido));
		end
	end
end