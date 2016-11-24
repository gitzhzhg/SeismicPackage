function ind=newest(t1,t2)
% ind=newest(t1,t2)
% 
% NEWEST compares two times (from clock or fix(clock)) and determines
% which is the most recent. If t1, then 1 is returned while a 2 
% indicates that t2 is more recent. If the times are identical, then
% 0 is returned.
%
% G.F. Margrave, April 1994
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
% compare years
if( t1(1)~=t2(1) )
	if(t1(1)<t2(1))
		ind=2;
	else
		ind=1;
	end
	return;
end
%compare months
if( t1(2)~=t2(2))
	if(t1(2)<t2(2))
		ind=2;
	else
		ind=1;
	end
	return;
end
delt= etime(t1,t2);
if( delt>0 )
	ind=1;
elseif(delt<0)
	ind=2;
else
	ind=0;
end