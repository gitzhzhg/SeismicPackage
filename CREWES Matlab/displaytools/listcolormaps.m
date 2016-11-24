function cmaps = listcolormaps(maxcmaps)
%
% function cmaps = listcolormaps(maxcmaps)
%  return a cell-array list of the names of all predefined matlab color maps
%
% cmap     ... cell array containing a list of pre-defined Matlab color maps
% maxcmaps ... maximum number of cmaps to return. Used for memory
%              allocation. Default is 30.
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

% Set default maximum number of color maps to return
if  nargin ~= 1
   maxcmaps = 30; 
end

% Fudge maxcmaps to account for title line in color maps block
maxcmaps = maxcmaps+1;

% Find the path that contains Matlab pre-defined colormap bone.m, and 
% replace bone.m with Contents.m in the resulting string
c = regexprep(which('bone'),'bone.m','Contents.m');

% Open Contents.m for reading
fid = fopen(c,'r');

% Check to see if fopen failed
if fid == -1
    disp(c)
    error('crewes:displaytools:listcolormaps', ...
        'Error accessing Contents.m' );
end

% Initialize cmapflag to false. This flag flips to true if a color maps 
% block is found in Contents.m
cmapflag = false;

% Initialize index into cmaps
cmapidx = 1;

% Pre-allocate cmaps as a column cell vector
cmaps = cell(maxcmaps,1);

% Read the file
while true
    % Read a line of text from Contents.m
    tline = fgetl(fid);
    
    % Break out of while loop if fgetl failed (typically at end of file)
    if ~ischar(tline)
        break 
    end    
    
    % Set cmapflag to true if color maps block is found in Contents.m
    if strncmpi(tline,'% color map',11)
        cmapflag = true; 
    end
    
    % Break out of while loop at the end of the color maps block, assuming
    % this is denoted by a line that only contains a single '%' character
    if cmapflag && strcmp(tline,'%')       
        break 
    end
    
    % If we are reading lines from the Color maps block
    if cmapflag
       tmp = textscan(tline,'%s'); %Break up tline into a cell array of discrete strings
       cmaps(cmapidx) = tmp{1}(2); %Set cmaps cell array to the colormap name
       cmapidx = cmapidx+1;        %Increment index
       
       % Break out of while loop if we have reached maxcmaps
       if cmapidx > maxcmaps
          break 
       end
    end
end

% close Contents.m
fclose(fid);

% Remove the first line of the Color maps block (it's a title, not a
% color map name
cmaps = cmaps(2:length(cmaps));

% Trim any empty cells (default maxcmaps is likely greater than the number 
% of available map names)
cmaps(cellfun('isempty',cmaps))=[];