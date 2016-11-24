function wlog=l_subtract_ekb(wlog,mnem4ekb)
% Subtract the Kelly-bushing elevation from the log-depth curve. 
% The Kelly bushing must be a parameter of teh well log.
% See also: ds_add_parameter
%
% Written by: E. Rietsch: September 11, 2007
% Last updated:
%
%           wlog=l_subtract_ekb(wlog,mnem4ekb)
% INPUT
% wlog      well log
% mnem4ekb  mnemonic for Kelly busking elevation
% OUTPUT
% wlog      input log with corrected depth curve
%
% EXAMPLE
%           wlog=l_data;
%           wlog1=l_subtract_ekb(wlog,'ekb');
%           l_curve(wlog)
%           l_curve(wlog1)


if nargin == 1
   mnem4ekb='ekb';
end

if isfield(wlog,'parameter_info')
   [ekb,info]=get_parameter(wlog,mnem4ekb,false);
   if isempty(ekb)
      alert(['Well log has no parameter "',mnem4ekb,'".']) 
      disp([' Available parameters are: ',cell2str(wlog.parameter_info(:,1),', ')])
      return
   end
else
   alert('Well log has no parameters.')
   return
end

%       Modification of depth curve with unit conversion and error checking
if strcmp(wlog.units,info{2})
   wlog.curves(:,1)=wlog.curves(:,1)-ekb;
elseif strcmp(wlog.units,'m')  &&  strcmp(info{2},'ft') 
   wlog.curves(:,1)=wlog.curves(:,1)-ekb*0.3048;  
elseif strcmp(wlog.units,'ft')  &&  strcmp(info{2},'m') 
   wlog.curves(:,1)=wlog.curves(:,1)-ekb/0.3048;
else
   error(['Unknown depth units: ',wlog.units,' or ',info{2}])
end

%       Correct start depth and end depth
wlog.first=wlog.curves(1,1);
wlog.last=wlog.curves(end,1);

%       Modify depth description
wlog.curve_info{1,3}=[wlog.curve_info{1,3},' (after subtraction of EKB)'];
