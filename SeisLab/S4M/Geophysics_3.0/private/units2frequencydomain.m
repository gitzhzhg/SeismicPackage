function units=units2frequencydomain(units)
% Create units in the frequency domain from those in the time domain
%
% Written by: E. Rietsch: March 22, 2006
% Last updated:
%
%         units=units2frequencydomain(units)
% INPUT
% units   string with units of measurement; recognized units are: 
%        's','ms','us','m','ft','samples'
% OUTPUT
% units   string with units of measurements after Fourier transform (e.g. 'Hz','Wavelength per 1000 m')
%
% EXAMPLE
%         units2frequencydomain('samples')

switch units

case 's'
   units='mHz';

case 'ms'
   units='Hz';

case 's'
   units='mHz';

case 'us'
   units='kHz';

case 'm'
   units='Wavelengths per 1000 m';

case 'ft'
   units='Wavelengths per 1000 ft';

case {'samples','Samples'}
   units='Wavelengths per 1000 samples';

otherwise
    units=['1/1000 ',units];
     
end
