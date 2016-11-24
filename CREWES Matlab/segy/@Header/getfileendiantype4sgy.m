function endi=getfileendiantype4sgy(obj,offset)
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



% read in indication variable if using sgy standard bytes 3225-3226 should
% be 1-8
fid=obj.fid;
fseek(fid,offset,'bof');
num=fread(fid,1,'*int16');

% get byte order of operating system
    [ab, ac, e] = computer;
% set change endian to false
 changeendi=false;
 
% set changeendi to true if the value of the indication variable is greater
% than 256

if abs(num)>=256
    changeendi=true;
end
 
% if changeendi is true make endi the opposite of the current computer else
% if changeendi is false make endi the same as the current computer
if changeendi
    if (strcmp(e,'L'))
        endi='B';
    else
      endi='L';
    end
else
    endi=e;
end

end