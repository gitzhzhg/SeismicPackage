function [traces,texthead,binaryhead,extendedhead]=SEGY_read(filein, varargin)
% [traces,texthead,binaryhead,extendedhead]=SEGY_read(filein)
%
% SEGY_read reads in the entire SEG-Y file.
%
% Inputs:
%    filein= the name of the SEG-Y file that is to be read in.
%
% Outputs:
%    traces= a Trace object.  To get the trace header values use
%       SEGY_getHeader.  To get the trace data values use SEGY_getData.
%    texthead= a TextHeader object.  To get the character array use
%       SEGY_getHeader.
%    binaryhead= a BinaryHeader object. To get the binary header values use
%       SEGY_getHeader.  
%    extendedhead= a cell array of TextHeader Objects. To get the character array use
%       SEGY_getHeader.
%
% Optional Arguments 
% To only read in some traces:
%   [...]=SEGY_read(filein,'traces',{search parameters});
%      search parameters can be a series of header words and values as
%          defined in SEGY_getHeader
%
%
% Heather Lloyd 2010, Kevin Hall 2009, Chad Hogan 2004
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
if nargin<1 
    [filename, pathname, filterindex] = uigetfile( {'*.sgy;*.SEGY;*.segy;*.SGY', 'All SEG-Y Files (*.sgy, *.SGY, *.segy, *.SEGY)'; ...
        '*.*','All Files (*.*)'},'Select a SEG-Y File');
    if  filename~=0
    filein=[pathname,filename];
    else
        me=MException('SEGY_READ:InvalidFilename','Please select a valid Filename');
        throw(me)
    end
end
name={};
 for i = 1:2:length(varargin)
                name = varargin{i};
                value = varargin{i+1};
 end
if any(strcmpi(name,'traces'))
    segyobj=SegyFile(filein,'traces','1');
    segyobj.trace=segyobj.trace.getTraces(value{:}); 
else
    segyobj=SegyFile(filein);
end
 
texthead=segyobj.textheader;
binaryhead=segyobj.binaryheader;
traces=segyobj.trace;
extendedhead=segyobj.extendedheader;
end