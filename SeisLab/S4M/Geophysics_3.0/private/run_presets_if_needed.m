function run_presets_if_needed
% Function runs set-up function "presets" in case global variable "S4M" is empty.
% This avoids an abort that would otherwise happen because subsequent code
% accesses a field of the global structure "S4M".
%
% Written by: E. Rietsch: December 12, 2005
% Last updated: April 11, 2009: Remove last change

% UPDATE HISTORT
%      August 16, 2008: Check if S4M.name is "SeisLab"

global S4M

if isempty(S4M)  % || ~strcmpi(S4M.name,'SeisLab')
   presets
   S4M.script='';
   S4M.plot_label='';
end
