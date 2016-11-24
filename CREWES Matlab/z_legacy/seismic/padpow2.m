function trout=padpow2(trin,flag);

% trout=padpow2(trin,flag);
% trout=padpow2(trin)
%
% PADPOW2 pads the input trace to the next power of two. The pad is
% attached in the column dimension so the trace must be a column vector. 
%
% flag= 1 --> If the trace is already an exact power of two, then
%             its length will be doubled.
%     = 0 --> If the trace is already an exact power of two, then
%             its length will be unchanged
%    ********* default = 0 ************
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

 if (nargin<2), flag=0; end
%
 [n,m]=size(trin);
 n2=2.^(nextpow2(n));
 if (n2==length(n))&(flag==1)
   n2=2.^(nextpow2(n+1));
 end
 
 trout=[trin;zeros(n2-n,m)];

 %if(nn==1)
 %		 trout=[trin zeros(1,n-length(trin))];
 %else
 %		 trout=[trin;zeros(n-length(trin),1)];
 %end