function t = isLas(obj)
%
%function t = islas(obj)
% Scans 'filename' for the first line that is not a comment,
% then checks if that line starts with '~Version'. If it does
% islas() returns true.
%
%  'filename' = character string
%  't'        = true or false
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

t=false;

fstats = dir(obj.fullFileName);

if isempty(fstats)
    return
end

if isequal(fstats.bytes,0)
    warning('crewes:las:islas',...
        'file is empty');
    return
end
   
try
    fid = fopen(obj.fullFileName,'r'); %open filename for reading
    
    % Read the filename line by line into memory, skipping comment
    % lines until we find a non-empty line
    row = textscan(fid,'%s',1,'delimiter','\n','CommentStyle','#');
    while isempty(char(row{1}))
        row = textscan(fid,'%s',1,'delimiter','\n','CommentStyle','#');
    end
    
    fclose(fid);
    
    % Check if line starts with '~Version'
    %If it does not, this is not a valid LAS filename
    v = '~v';
    t = strncmpi(strtrim([row{:}]),v,length(v));
    
    if ~t
        warning('crewes:las:islas', ...
            'file is not LAS format');
    end
catch ex
    warning('crewes:las:islas',ex.message);
    t = false;
end %end try

end %end function