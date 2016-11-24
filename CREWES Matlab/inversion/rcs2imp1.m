function imp=rcs2imp1(r,i0)
% RCS2IMP1 ... exact conversion of rc to impedance (pairs with imp2rcs1)
%
%  imp=rcs2imp1(r,i0)
%
%  Given vectors imp amd z that are exactly the same length N, we take
%  these to describe a layered medium where the z's are the depths to the
%  tops of constant impedance layers whose impedance values are in imp.
%  (Note: the 'depths' used here could equally well be times, or just layer
%  counters.) Then, in the system so described, there are N-1 reflection
%  coefficients which are naturally defined at the depths z(2:N). Thus, the
%  computation of rc (reflection coefficient) takes N numbers into N-1
%  numbers. The reverse computation from rcs to imp must then take N-1
%  numbers into N. This requires one extra number which is the first
%  impedance in the sequence. Thus
%  r=imp2rcs1(imp);
%  imp1=rcs2imp1(r,imp(1));
%  produces imp1 that is essentially identical to imp. Also, for any
%  sub-interval between z(1) and z(N), then
%  r=imp2rcs1(imp(ind));
%  imp1=rcs2imp1(r,imp(ind(1)));
%  produces imp1 that is essntially identical to imp(ind). Here ind is a
%  vector of consequtive integers pointing to the subinterval.
%  The kth impedance is given by
%  imp(k)= imp(k-1)*(1+r(k))/(1-r(k))
%
%  r ... vector of N-1 reflection coeficients.
%  i0 ... the first impedance
%  imp ... vector of N impedances 
%
%
% by G.F. Margrave, March 2016
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
ni=length(r)+1;

imp=zeros(ni,1);
imp(1)=i0;

for k=2:ni
    imp(k)=imp(k-1)*(1+r(k-1))/(1-r(k-1));
end