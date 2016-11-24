function step=depths2step_with_checking(depths,epsilon)
% Compute sample increment for well logs
%
% Written by: E. Rietsch: April 18, 2006
% Last updated:
%
%          step=depths2step_with_checking(depths,epsilon)
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
   if step < 0
      error(' The selected "depth column" is decreasing.')
   elseif step == 0
      error('The two depth samples are identical.')
   end
else
   dd=diff(depths);
   if any(dd <= 0)
      error(' The selected "depth column" is not strictly monotonic.')
   end
   mdd=mean(dd);
   if isnearinteger(max(dd)/mdd,epsilon)
      step=mdd;
   else
      step=0;
   end
end
