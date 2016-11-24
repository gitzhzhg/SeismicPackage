% simpledit_demo
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

%generate two spiky reflectivities
[r1,t]=reflec(1.0,.004,.5,3,pi);
[r2,t]=reflec(1.0,.004,.5,3,exp(1));
%make two fake horizons
x=1:.5:10;
y1=.4+(x-1)*(-.033);
y2= .8 -.4*cos( (x-4.5)/9 );
%plot
figure;
p=get(gcf,'position');
set(gcf,'position',[p(1:2) 600 p(4)]);
h1=line( x,y1, 'color','r');
h2=line( x,y2, 'color','c');
h3=line( r1+3, t, 'color','b');
h4=line( r2+6, t, 'color','b');
set(gca,'ydir','reverse');
simpledit
editlines('multnanoff');