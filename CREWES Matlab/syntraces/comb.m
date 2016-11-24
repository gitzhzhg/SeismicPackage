function trout=comb(trin,n,flag)
%COMB ... create a comb function (spikes every n samples)
%
%    trout=comb(trin,n,flag) 
%    trout=comb(trin,n)
%
% outputs a comb function which has unit spikes every n samples
% 
% trin= input trace (to give dimensions)
% n= interval between spikes in samples
%    if n is negative, the spikes will alternatly be +1 and -1
% flag= 0 ... first unit spike is at sample n/2 (*** default ***)
% flag= 1 ... first unit spike is at sample 1
% flag= 2 ... first unit spike is at sample n
% trout= output comb function
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

if nargin<3
   flag=0;
 end
%
   trout=zeros(size(trin));
   m=floor(abs(n/2));
   if flag==1
     m=1;
   end
   if flag==2
     m=abs(n);
   end
   if n>0
     while m<=length(trin)
       trout(m)=1.0;
       m=m+n;
     end
   else
     spike=-1.0;
     while m<=length(trin)
       spike=-1*spike;
       trout(m)=spike;
       m=m+abs(n);
     end
   end