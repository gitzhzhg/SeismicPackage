function L=near(v,val1,val2)
% L=near(v,val1,val2)
% L=near(v,val1)
%
% NEAR searches the vector v and finds the index,L1, for which
%   v(l) is closest to val1 and L2 for which v(i) is closest to
% val2. The returned valued is the vector L=L1:L2 (L2>L1) or
% L=L1:-1:L2 for L2<L1 
%
% v= input vector
% val1= first input search value
% val2= second input serach value
%  ******** default= val1 ********
% L= output vector of indicies such that 
% abs(v(l(1))-val1)==minimum and abs(v(l(length(I))-val2)==minimum
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
  
% 
 if nargin<3
    val2=val1;
 end
 
  ilive=find(~isnan(v));
  test=abs(v(ilive)-val1);
  L1=find(test==min(test));
  test=abs(v-val2);
  L2=find(test==min(test));
  L1=ilive(L1);
  L2=ilive(L2);
 	if L1<=L2
  		L=min(L1):max(L2);
	else
    	L=max(L1):-1:min(L2);
 	end
 