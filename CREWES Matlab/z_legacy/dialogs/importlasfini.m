function [lognums,lognames,wellname,x,zstart,zend,topsflag,units,kb]...
          =importlasfini
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
%test for cancel
a=get(gca,'userdata');
if(a==-1)
    lognums=-1;
    lognames=-1;wellname=-1;x=-1;zstart=-1;
    zend=-1;topsflag=-1;units=-1;kb=-1;
    return;
end
ind=find(isnan(a));
lognums=abs(a(1:ind(1)-1));
lognames=setstr(a(ind(1)+3:ind(2)-1));
m=abs(a(ind(1)+1));
n=abs(a(ind(1)+2));
lognames=reshape(lognames,m,n);
wellname=setstr( char( a(ind(2)+1:ind(3)-1)) );
units=setstr(a(ind(3)+1:ind(4)-1));
x= abs(a(ind(4)+1));
zstart=abs(a(ind(4)+2));
zend=abs(a(ind(4)+3));
topsflag=abs(a(ind(4)+4));
kb=abs(a(ind(4)+5));