function[delt1,delt2,avgmdx1,avgmdx2]=fderv(mddiff3,mddiff4,x1,x2)
clear delt1
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

%clear dt1
[m,n]=size(x1);
if( size(mddiff3) ~= 0 )
  delt1=mddiff3(1:n-2)-mddiff3(3:n);
  avgmdx1=(x1(1:n-2)+x1(3:n))/2;
%  dt1 = clip(delt1,1);
else
  avgmdx1 = [];
end
clear delt2
%clear dt2
[m,n]=size(x2);
if( size(mddiff4) ~= 0 )
  delt2=mddiff4(1:n-2)-mddiff4(3:n);
  avgmdx2=(x2(1:n-2)+x2(3:n))/2;
%  dt2 = clip(delt2,1);
else 
  avgmdx2 = [];
end