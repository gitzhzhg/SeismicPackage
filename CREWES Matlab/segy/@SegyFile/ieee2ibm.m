function ibm = ieee2ibm (ieee)
%
% function ieee2ibm(ieee)
% Where:
%   ieee = single precision ieee float(s) as uint32, single or double
%   ibm  = single precision ibm float(s) as uint32 (suitable for fwrite)
%          eg. fwrite(fid,ibm,'uint32')
%
% Example:
%   >> format hex
%   >> ieee2ibm([25.1 -50.3 75.2 100.9)
%   >> ans = 42191999   c2324ccc   424b3333   4264e666
%
% References
%   http://bytes.com/topic/c/answers/...
%        221981-c-code-converting-ibm-370-floating-point-ieee-754-a
%        => C code contributed by lawrence.jones@ugs.com
%
% Modified and tested in Matlab 2009b on Windows Vista 64-bit PC
%   Kevin Hall, CREWES, University of Calgary
%   February 2010
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

%

% Check type of input, and convert to uint32 for bitshifts
if (isa(ieee,'uint32'))
    val = ieee;
elseif(isa(ieee,'single'))
    val = typecast(ieee,'uint32');
elseif (isa(ieee,'double'))
    val = typecast(single(ieee),'uint32');
else
    error('Input must be of type ''uint32,'' '' single '' ''or double''');
end

% Get signbit, exponent and fraction
signbit  = bitshift(val,-31);      % store sign bit
fraction = bitshift(val,1);        % remove sign bit
exponent = bitshift(fraction,-24); % store exponent
fraction = bitshift(fraction,8);   % remove exponent
fraction = bitshift(fraction,-1);  % store fraction

% For every IEEE number
for i=1:numel(fraction) %For every input number
    if (exponent(i) == 0 && fraction(i) == 0)
        % Found a zero, don't need to do anything
    elseif (exponent(i) == 255)
        % Found a NaN or Inf
        % (fraction == 0) => Inf || (fraction != 0) => NaN 
        % Either way -> return IBM max
        warning('IEEE:nanorinf',...
            'Found NaN or Inf - returning IBM max')
        exponent(i) = uint32(hex2dec('0000007f')); 
        fraction(i) = uint32(hex2dec('ffffff00'));
    else
        % Found a valid number:
        % Add the IEEE implied digit (This should normally be bit 24, 
        % but we're bitshifted at the moment)
        fraction(i) = bitset(fraction(i),32);

        % Convert exponent from base 2 offset 127 (ieee) to 
        % base 64 offset 64 (ibm)
        exponent(i) = exponent(i) +130;
        shift = int32(bitand(typecast(-1*int32(exponent(i)),'uint32'),3));
        fraction(i) = bitshift(fraction(i),-1*shift);
        exponent(i) = bitshift(exponent(i)+3,-2);

        % (Re)normalize
        while (fraction(i) < uint32(hex2dec('10000000')))
           exponent(i) = exponent(i) -1;
           fraction(i) = bitshift(fraction(i),4);
        end 
    end
end

% Assemble the answer
signbit  = bitshift(signbit,31);
exponent = bitshift(exponent,24);
fraction = bitshift(fraction,-8); 
ibm = bitor(signbit,bitor(exponent,fraction));

end