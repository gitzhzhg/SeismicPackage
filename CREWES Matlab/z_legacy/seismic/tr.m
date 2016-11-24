function trout=tr(trin,flag)
% trout=tr(trin,flag)
%
% TR returns the time reverse of trin.
% This is accomplished such that the sample at length(trin)/2
% remains fixed. For length(trin) even, the last sample in trin
% is discarded
%
% trin= input trace
% flag= 0 ... keep the center sample (at length(trin)/2) fixed.
%             For length(trin) even, the last sample is discarded.
%     = 1 ... don't worry about the center sample, simply reverse
%             the time series.
%     ************ default= 0 *************
% trout= output trace
%
% by G.F. Margrave, June 1991
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
 if nargin<2, flag=0; end
 trout=trin(length(trin):-1:1);
if flag==0,
 if rem(length(trin),2)==0,
   trout=[trout(2:length(trin)) 0];
 end
end