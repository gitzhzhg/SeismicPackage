function wlog=l_depth2index(wlog,warn)
% Prepend a new column to the log curves. This curve is an index vector
% with values increasing from 1 to "nsamp", the number of rows of the curve
% matrix. It becomes the new depth curve. The original depth curve is now a 
% curve like any other curve. The mnemonic of the new curve is "row_index".
% If a curve with this name already exists and "warn" is set to 1 a warning 
% message pops up and the user is given the opportunity to overwrite it or 
% abort. Otherwise it is silently overwritten.
% The "units of measurement" of the new curve are "samples".
%
% Written by: E. Rietsch: August 4, 2003
% Last updated: September 16, 2003: change mnemonic name to "row_index"
%
%         wlog=l_depth2index(wlog)
% INPUT
% wlog    well log
% warn    issue warning if index already exists. Two possible values: 
%         1,true (yes) and  0,false (no).
%         Default: warn=false
% OUTPUT
% wlog    original well log with prepended curve "row_index"

if nargin == 1
   warn=false;
end

index=find(ismember(wlog.curve_info,'row_index'));
if ~isempty(index) && warn
   ButtonName=questdlg('The mnemonic "row_index" already exists. Do you want to replace it?', ...
                       'SeisLab Message from "l_depth2index"', ...
                       'yes','no','no');
   if strcmp(ButtonName,'no')
      disp(' The mnemonic "row_index" already exists.')
      error('Abnormal termination')
   end
   wlog=l_curve(wlog,'delete','row_index');

elseif ~isempty(index)
   wlog=l_curve(wlog,'delete','row_index');
end

nsamp=size(wlog.curves,1);
wlog.curves=[(1:nsamp)',wlog.curves];
wlog.curve_info=[{'row_index','samples','Row index'};wlog.curve_info];
wlog.first=1;
wlog.last=nsamp;
wlog.step=1;
wlog.units='samples';
