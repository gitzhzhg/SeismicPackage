function filename2S4M(filename)
% Function extracts directory name and file name from a full filename
% and creates/replaces fields S4M.pathname and S4M.filename
% If there is no full path the pathname is set to the Working Directory
%
% Written by: E. Rietsch:
%
%    filename2S4M(filename)

global S4M

[pathname,filename,ext] = fileparts(filename);

if isempty(pathname)
   pathname=[pwd,filesep];
%   alert([' No file separator "',filesep,'" found in filename "',filename])
end

S4M.pathname=pathname;

S4M.filename=[filename,ext];
