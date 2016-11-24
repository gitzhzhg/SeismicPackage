function obj       = checkInputArgs(obj, varargin)    
%
% function obj       = checkInputArgs(obj, varargin) 
%
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

%Check input arguments
if isempty(varargin{2}) %note: varargin{1} = obj
    [f, p] = uigetfile( ...
        {'*.las;', 'LAS Files (*.las)'; ...
        '*.txt;', 'Text Files (*.txt)'; ...
        '*.*',    'All Files (*.*)'},      ...
        'Select LAS file');
    if isequal(f,0) || isequal(p,0)
        warning('crewes:las:nofile','User canceled file selection');
        obj.fileName='';
        obj.fullFileName='';
    else
        obj.fileName=f;
        obj.fullFileName=fullfile(p,f);
    end
else
    [~, f, e] = fileparts(char(varargin{2}));
    obj.fileName=[f e];
    obj.fullFileName=char(varargin{2});
    
    if ~exist(obj.fullFileName,'file')
        warning('crewes:las:checkinputargs',...
            ['File: ' char(varargin{2}) ' does not exist']);
        obj.fileName='';
        obj.fullFileName='';
    end
end    
    
end %end function