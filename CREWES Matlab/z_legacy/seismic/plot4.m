function plot4(x1,y1,x2,y2,x3,y3,x4,y4)
% plot4(x1,y1,x2,y2,x3,y3,x4,y4)
%
% subdivides plot window into 4 and plots 4 graphs
%
% by G.F. Margrave, May 1991
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
clf
x1=xcoord(x1(1),x1(2)-x1(1),y1);
x2=xcoord(x2(1),x2(2)-x2(1),y2);
x3=xcoord(x3(1),x3(2)-x3(1),y3);
x4=xcoord(x4(1),x4(2)-x4(1),y4);
subplot(2,2,1),plot(x1,y1)
subplot(2,2,2),plot(x2,y2)
subplot(2,2,3),plot(x3,y3)
subplot(2,2,4),plot(x4,y4)