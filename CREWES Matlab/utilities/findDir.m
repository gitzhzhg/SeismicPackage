function result = findDir(dirname,startdir,result)
%
% function result = finddir(dirname,startdir)
%
% Recursively looks for a directory name
%  Where: dirname  is the directoryname to be found (exact match)
%         startdir is the top level directory to start searching in        
%         result   is the result of the search
%  Usage: result = findDir(dirname)
%         result = findDir(dirname,startdir)
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

% Check for first call to findDir
if nargin <1 || nargin >3
   disp('Use ''help findDir'' for usage');
   return
elseif nargin == 1
    startdir = userpath();
    startdir = startdir(1:end-1);
    result=[];
elseif nargin == 2
    result=[];
end

%disp(startdir)

fl     = dir(startdir);   %get list of all files/dirs in userdir
n      = [fl.isdir];     %get logical indices for just the dirs
dirs   = { fl(n).name }; %cell array list of dir names


s = strcmp(dirs,dirname); %check for exact match
if any(s) %success!
    result = fullfile(startdir,dirs{s});
    return
else %failure, recurse
    for k = 1:length(dirs)
        if ~strncmp(dirs{k},'.',1) %skip directories '.' and '..'
            result = findDir(dirname,fullfile(startdir,dirs{k}),result);
            if ~isempty(result) %success!!!
                return
            end
        end
    end
end

end %end findDir()