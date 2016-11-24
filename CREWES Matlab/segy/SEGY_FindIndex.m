function segy = SEGY_FindIndex(segy, offset, type)

% segy = SEGY_FindIndex(segy, snum, offset)
%
% Finds all values at byte offset 'offset' in the trace header of a 
% SEG-Y data file, using struct 'segy'. Result is set in 
% segy.idxs. 'type' is the data type -- most likely it'll be 'int',
% 'short', or 'float'. 
%
% For example, you could run
%
% segy = SEGY_FindIndex(segy, 72, 'int');
%
% In this case, this function will go through all of the SEG-Y data and
% look for trace-header value 72 (of type 'int'). In this case, 72
% corresponds to source X location (sx). The function will add an element
% to the array segy.idxs. If this is the first header value you have
% indexed, the resulting data will be set in segy.idxs(1). To use the data
% after it has been set, you can recover the header offset with:
%
% segy.idxs(1).index
%
% and you can recover the values for each trace with
%
% segy.idxs(1).values
%
% this 'values' array contains one entry for each trace in the SEG-Y data
% with its extracted value.
%
% Chad Hogan, 2008
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

% $Id: SEGY_FindIndex.m,v 1.1 2008/03/04 22:36:36 cmhogan Exp $

disp(['Going to search ' num2str(segy.numtraces) ' traces']);

traceidxvalue = zeros(1, segy.numtraces);

indexnum = length(segy.idxs) + 1;

for idx = 1:segy.numtraces
    SEGY_TraceSeek(segy, idx);
    fseek(segy.FILE, offset, 0);    % move to offset
    thisidxval = fread(segy.FILE, 1, type);

    traceidxvalue(idx) = thisidxval;

    if(mod(idx, 1000) == 0)
        disp(['done ' num2str(idx) ' of ' num2str(segy.numtraces)]);
    end
end
disp(['Trace header offset ' num2str(offset) ' indexed']);

segy.idxs(indexnum).index = offset;
segy.idxs(indexnum).values = traceidxvalue;