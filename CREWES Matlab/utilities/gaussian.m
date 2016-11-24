function y=gaussian(x,xnot,xwid,yht)
%
% y=gaussian(x,xnot,xwid,yht)
% 
% x    ... x coordinate axis
% xnot ... center of the Gaussian
% xwid ... width of the Gaussian (the standard deviation of the Gaussian is
%           sigma=xwid/4)
% yht  ... height of the Gaussian
% ********* default =1 *********
%
% y ... a column vector the same length as x containing the Gaussian
%
% Notes: To window a signal, s, use sw2=s.*gau; where gau is a Gaussian
% window designed to be the same length as s. If the time coordinate for s
% is t, then gau=gaussian(t,mean(t),(t(end)-t(1))/10,1); is a unit-height
% Gaussian window placed in the center of t with width 10% of the span of
% t. sw will contain a lot of very small values. To truncate the windowed
% signal at gdb decibels below its maximum, use
% ind=todb(gau)>-gdb;swt=sw(ind),twt=t(ind); and swt will be the windowed,
% truncated signal and twt will be its time coordinate. If sigma is the
% standard deviation of the Gaussian, then at 2*sigma from xnot, the
% Gaussian is 17.4 db down and at t=4*sigma it is 69.5 db down.
%
% by G.F. Margrave, 2015
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

if(nargin<4)
    yht=1;
end
if(~between(x(1),x(end),xnot,2))
    error('xnot not contained in x')
end
sigma=xwid/4;
y=yht*exp(-.5*((x(:)-xnot)/sigma).^2);