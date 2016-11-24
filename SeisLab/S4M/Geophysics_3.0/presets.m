function presets
% Set default parameters for SeisLab
%
% Written by: E. Rietsch
% Last updated: August 8, 2008: Update name of user-default file


%       Set system default parameters
systemDefaults4Seislab

%       Set user-defined parameters (or override system parameters) if desired
try
   userDefaults4Seislab
catch
   try
      userDefaults   
   catch
      % Do nothing
   end
end
