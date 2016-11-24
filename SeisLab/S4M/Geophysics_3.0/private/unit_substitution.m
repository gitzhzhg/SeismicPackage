function uo=unit_substitution(ui)
% Function substitutes standardized set of units for input units
% e.g. K/M3 ==> kg/m3
%
% Written by: E. Rietsch: February 12, 2000
% Last updated: April 8, 2008: Add more conversion rules
%
%        uo=unit_substitution(ui)
% INPUT
% ui     string (or cell) with input units of measurement
% OUTPUT
% uo    string (or cell) with output units of measurement

% UPDATE HISTORY
%       December 21, 2006

if isempty(ui)
   uo='';
   return
end

io={'API UNITS', 'API units'
    'BARNS' ,   'barns'
    'DEG'   ,   'degrees'
    'DEG C' ,   'degree C'
    'DEG F' ,   'degree F'
    'DEGF'  ,   'degree F'
    'FEET'  ,   'ft'
    'F'     ,   'ft'
    'FT'    ,   'ft'
    'FT/SEC',   'ft/s'
    'FT/S'  ,   'ft/s'
    'F/S'   ,   'ft/s'
    'F/SEC' ,   'ft/s'
    'FRAC'  ,   'fraction'
    'FRACT' ,   'fraction'
    'V/V'   ,   'fraction'
    'V/V_decimal', 'fraction'
    'DEC'   ,   'fraction'
    'GAPI'  ,   'API units'
    'G/C3'  ,   'g/cm3'
    'G/CC'  ,   'g/cm3'  
    'G/CM3' ,   'g/cm3'
    'GM/C3' ,   'g/cm3'
    'GM/CC' ,   'g/cm3'
    'GM/CC.KM/S','km/s x g/cm3'
    'GM/CM3',   'g/cm3'
    'GM/CM^3',  'g/cm3'
    'IN'    ,   'inches'
    'K/M3'  ,   'kg/m3'
    'KG/M3' ,   'kg/m3'
    'KG/M^3',   'kg/m3'
    'KG/M^3*M/S','m/s x kg/m3'
    'KM/SEC'    'km/s'
    'KM/S*G/CC','km/s x g/cm3'
    'M'     ,   'm'
    'MM'    ,   'mm'
    'MD'    ,   'mD'
    'MT'    ,   'm'
    'M/S'   ,   'm/s'
    'M/S*G/CC',  'm/s x g/cm3'
    'M/SEC' ,   'm/s'
    'MSEC'  ,   'ms'
    'MPA'   ,   'MPa'
    'MV'    ,   'mV'
    'NA'    ,   'n/a'
    'OHMM'  ,   'Ohm-m'
    'PA'    ,   'Pa'
    'PERC'  ,   '%'
    'PERCENT',  '%'
    'PPG'   ,   'ppg'
    'PSI'   ,   'psi'
    'S'     ,   's'
    'S/M'   ,   's/m'
    'SEC'   ,   's'
    'SEC/M' ,   's/m'
    'SHOTS/F',  'shots/ft'
    'SHOTS/FT', 'shots/ft'
    'SHOTS/M',  'shots/m'
    'USEC/F',   'us/ft'
    'USEC/FT',  'us/ft'
    'US/FT' ,   'us/ft'
    'US/F'  ,   'us/ft'
    'US/M'  ,   'us/m'
};

idx=find(ismember(lower(io(:,1)),lower(ui)));
if isempty(idx)
   uo=ui;
else
   if length(idx) > 1
      show(io(idx,  1)')
      error(['More than one match for ',  ui])
   else
      if iscell(ui)
         uo=io(idx,2);
      else
         uo=io{idx,2};
      end
   end
   
end
