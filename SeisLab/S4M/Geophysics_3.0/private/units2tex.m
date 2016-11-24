function uo=units2tex(ui)
% Function substitutes TeX-style units for input units that imply exponents or 
% greek letters (for axis annotation of plots)
% e.g. kg/m3 ==> kg/m^3
%
% Written by E. Rietsch, May, 6, 2000
% Last updated: November 12, 2005: Add "us" (for microseconds) as units to 
%                                  be converted to TeX-style
%
%        uo=units2tex(ui)
% INPUT
% ui     string or cell with input units of measurement
% OUTPUT
% uo     string with output units of measurement
%
% EXAMPLES
%        units4tex=units2tex('g/cm3')
%        units4tex=units2tex('us/ft')


if strcmp(ui,'n/a')
   uo='';

else
   if strcmp(ui,'us')
      uo='\mus';
   else
      uo=strrep(ui,'g/cm3','g/cm^3');
      uo=strrep(uo,'kg/m3','kg/m^3');
      uo=strrep(uo,'us/ft','\mus/ft');
      uo=strrep(uo,'us/m' ,'\mus/m');
   end
end
