function step=depths2step(depths,epsilon)
% Compute sample increment for well logs; check if it is uniform or not and
% set the appropriate value for "step".
%
% Written by: E. Rietsch: April 17, 2006
% Last updated: April 8, 2008: bug fix
%
%          step=depths2step(depths,epsilon)
% INPUT
% depths   vector of monotonically increasing depth values
% epsilon  maximum relative change increment from one depth value to the next
%          Default: epsilon = S4M.log_step_error
% OUTPUT
% step     depth increment or zero if
%          abs(max(diff(depths))/median(diff(depths))-1 < epsilon

global S4M

if nargin == 1
   epsilon=S4M.log_step_error;
end

nsamp=length(depths);
if nsamp == 1
   step=0;
elseif nsamp == 2
   step=depths(2)-depths(1);
else
   dd=diff(depths);
   mdd=median(dd);
   madd=max(dd);
   midd=min(dd);
   if (madd-midd)/mdd < epsilon
      step=mdd;
   else
      step=0;
   end
end
