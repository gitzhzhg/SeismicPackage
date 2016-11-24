function obj = guessByteOrder(obj)
%
% Guess if data in SEG-Y file are big or little endian
% Theory:
%   The data format code in the binary file header
%   will be out of range [1,8] if read using
%   the wrong byte order.
% Returns:
%   'ieee-be' (SEG-Y standard) unless the data can be proven to be
%   little-endian (PC byte order), in which case 'ieee-le' is returned
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

%Assume standard segy
obj.machineformat = 'ieee-be';

%Test for PC byte order
try
    fseek(obj.fid,3224,'bof');
    if(fread(obj.fid, 1, 'int16', 0, 'ieee-le') <255)
        obj.machineformat = 'ieee-le';
    end
catch me
    error(me.message);
end

end