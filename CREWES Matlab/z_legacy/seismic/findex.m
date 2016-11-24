function iex=findex(trin,flag)
% iex=findex(trin,flag)
% iex=findex(trin)
%
% FINDEX returns a vector of indicies of the samples of trin 
% which are either local maxima or local minima.
% Note that extrema which persist for more than one sample will
% have only the final sample flagged.
%
% trin= input trace
% flag=1.0 .... find local maxima
%     =0.0 .... find both maxima and minima
%      -1.0 ... find local minima
%  ******* default=1.0 *******
%
% iex= vector of indicies of the extrema
%
% by G.F. Margrave June, 1991
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
if nargin<2, flag=1.0; end
 d1=diff(trin);
 ind=find(d1~=0);
 d1=d1(ind)./abs(d1(ind));
 %d1 is now +1 for a pos difference -1 for neg and zero otherwise
 d2=diff(d1);
%
% d2=-2.0 is a maximum and d2=+2.0 is a minimum
%
 
 if(flag>0.0),iex=find(d2<-1.9);end
 if(flag<0.0),iex=find(d2>1.9);end
 if(flag==0.0),iex=find(d2~=0.0);end
 iex=iex+1;
 iex=ind(iex);