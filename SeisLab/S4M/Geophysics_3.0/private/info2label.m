function label=info2label(info)
% Create label for axis annotation from two-element or three-element 
% cell array such as those found in structures for seismic data, well logs, 
% p.d.f.s, etc. only the last two entries of "info" are used.
%
% Written by: E. Rietsch: June 23, 2003
% Last updated: July 28, 2006: streamlining
%
% INPUT
% info   2- or 3-element cell array
%        only the last two entries are used
% OUTPUT
% label  string with label for plot annotation

units=info{end-1};

if ~isempty(units)  &&  ~strcmpi(units,'n/a')
   label=[info{end},' (',units2tex(units),')'];

else
   label=info{end};
end
