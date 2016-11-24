function [trout,tout]=strechlog(trin,t,zonein,zoneout)
% [trout,tout]=strechlog(trin,t,zonein,zoneout)
%
% STRECHLOG stretches a single time zone on a log into another. This done by
% sinc function resampling with zero phase anti-alias filtering if needed.
% trin = input trace or log (regularly sampled)
% t= vertical coordinate vector for trin. Ostensibly time but could equally well be
%		depth
%  ************** trin and t must be the same length ************
% zonein = time zone boundaries for the time zone to be streched. This should
%		be a two element vector giving [tmin tmax]
% zoneout = the input log, over the time zone zonein, will be streched to
% 			fill this time zone at the same sample rate as input
% The first output sample will be at zoneout(1)
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
% convert to column vectors
[l,m]=size(trin);
trin=trin(:);
t=t(:);
	
tmin1=zonein(1);
tmin2=zoneout(1);
tmax1=zonein(2);
tmax2=zoneout(2);
dt=t(2)-t(1);
	% compute number of output samples
	nsampout=round((tmax2-tmin2)/dt)+1;
		
	% determine the effective output sample rate
		
	dtout= (tmax1-tmin1)/(nsampout-1);
		
	trout=resamp(trin,t,dtout,zonein,0);% use zero phase anti-alias
	tout=tmin2:dt:tmax2;
		
	if(l==1)
		trout=trout';
	tout=tout';
 end