function [vol,x,y,z,xinfo,yinfo,zinfo]=traces2volume(seismic,headerx,headery)
% Convert traces of a 3-D dataset into a three-dimensional matrix
%
% Written by: E. Rietsch: October 8, 2007
% Last updated:
%
%           [vol,x,y,z,xinfo,yinfo,zinfo]=traces2volume(seismic,headerx,headery)
% INPUT
% seismic   seismic dataset with two headers representing two surface coordinates
% headerx   mnemonic of header representing the x-coordinate 
%           (e.g. 'iline_no', 'cdp_x', 'cdp')
% headery   mnemonic of header representing the y-coordinate 
%           (e.g. 'xline_no', 'cdp_y', 'offset')
% OUTPUT
% vol       3-d matrix from the seimic traces
%           first coordinate is time/depth; second coordinate  is "x",
%           third coordinate is "y"
% x         values of header "headerx"
% y         values of header "headery"
% z         time values (seimic.first:seismic:step:seismic.end)
% xinfo     cell row vector with mnemonic, units of measurement, and description
%           for header "headerx"
% yinfo     cell row vector with mnemonic, units of measurement, and description
%           for header "headery"
% zinfo     cell row vector with strings 'time', seismic.units, 'Time'


%	Sort seismic traces so that "headerx" increases least
try
   index=ds_header_sort(seismic,{'headers',headery,headerx});
catch
   lasterr
   error(['Please check if the data set does have valid headers "', ...
   headerx,'" and "',headery,'".'])
end
seismic=s_select(seismic,{'traces',index});

[x,xinfo]=s_gh(seismic,headerx);
[y,yinfo]=s_gh(seismic,headery);
z=(seismic.first:seismic.step:seismic.last)';
zinfo=info4time(seismic);

temp=unique([x(:),y(:)],'rows');
if size(temp,1) < size(seismic.traces,2)
   error('Headers specified do not uniquely define traces.')
end

x=unique(temp(:,1));
y=unique(temp(:,2));


if length(x)*length(y) > size(seismic.traces,2)
   error('The seismic dataset does not have traces for all locations of the inline/cross-linge grid.')
end

vol=reshape(seismic.traces,length(z),length(x),length(y));
