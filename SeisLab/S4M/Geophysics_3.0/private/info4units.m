function info=info4units(units)
% Create a standard cell vector with info derived from units of measurement
% Replacement for "info4time".
%
% Written by: E. Rietsch: September 19, 2009
% Last updated:
%
%          info=info4units(units)
% INPUT
% units    units of measurement
% OUTPUT
% info     three-element cell row vector with mnemonic, units of measurement,
%          and description of the units (intended for axis annotation)
%
% EXAMPLE
%          info4units('kHz')


info=cell(1,3);
info{2}=units;

switch lower(units)
    case {'ms','msec','s','sec','us','usec'}
        info{1}='time';
        info{3}='Time';
        
    case {'hz','khz','mhz'}
        info{1}='frequency';
        info{3}='Frequency';
        
    case {'ft','kft','m','km'}
        info{1}='depth';
        info{3}='Depth';
        
    case 'samples'
        info{1}='sample';
        info{2}='n/a';
        info{3}='Samples';
   
    case 'rows'
        info{1}='rows';
        info{2}='n/a';
        info{3}='Rows';
     
    case 'amplitude'
        info{1}='amplitude';
        info{2}='n/a';
        info{3}='Binned amplitude';
        
    case 'bin-index'
        info{1}='binindex';
        info{2}='unequal bins';
        info{3}='Bin index';
  
    otherwise
        info{1}=info{2};
        info{2}='n/a';
        info{3}=info{1};
       
end

