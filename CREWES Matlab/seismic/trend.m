function [st,p]=trend(s,t,m)
% TREND ... estimate the trend of a signal s by low order polynomial fit
%
% [st,p]=trend(s,t,m)
%
% s ... input signal
% t ... time coordinate for signal
% m ... degree of polynomial fit
% ******* default m=1 ********
%
% st ... trend of s
% p ... polynomial coefficients determining st. That is st=polyval(p,t).
%

if(nargin<3)
    m=1;
end
%normalize t
t=t/mean(t);
%adjust m
if(m>=length(s)); m=length(s)-1; end
if(m>0)
    p=polyfit(t,s,m);
    st=polyval(p,t);
else
    st=0;
end

%
% by G.F. Margrave October 2015
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
