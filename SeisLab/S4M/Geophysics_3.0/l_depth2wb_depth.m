function wlog=l_depth2wb_depth(wlog,varargin)
% Change the depth coordinate to depth from water bottom; the function assumes 
% the water depth and possibly the Kelly bushing elevation are available as 
% parameters ('wd' and 'ekb', respectively) of the well log. There must also  
% be a field 'parameter_info'. If these parameters were already in the LAS file
% they would have been transferred to the log structure automatically.
%
% Written by: E. Rietsch: April 1, 2005
% Last updated:
%
%           wlog=l_depth2wb_depth(wlog,varargin)
% INPUT
% wlog      well log
% varargin    one or more cell arrays; the first element of each cell array is a keyword,
%             the other elements are parameters. Presently, keywords are:
%     'wd'    either mnemonic of the water-depth parameter or actual water depth
%             in the depth units of the log
%             Default: {'wd','wd'}
%     'ekb'   either mnemonic of the Kelly bushing parameter or actual Kelly
%             bushing elevation in the depth units of the log
%             Default: {'ekb','ekb'}
% OUTPUT
% wlog     well log
%
% EXAMPLE
%     wlog=l_data;
%     l_curve(wlog)
%     wlog1=l_depth2wb_depth(wlog,{'wd',1000});
%     l_curve(wlog1)

%       Default parameters
param.wd='wd';
param.ekb='ekb';

%       Decode and assign input arguments
[param,cm]=l_assign_input(param,varargin);

dunits=wlog.curve_info{1,2};    % Depth units of the log

if ischar(param.wd)
   try
%      wd=getfield(wlog,param.wd);
      wd=wlog.(param.wd);
   catch
      disp([' Well log has no parameter "',param.wd,'".'])
      error('Abnormal termination')
   end

   units=getParameterUnits(wlog,param.wd);
   if ~strcmpi(units,dunits)
      if strcmpi(units,'ft')  &&  strcmpi(dunits,'m')
         wd=wd*0.3048;
      elseif strcmpi(units,'m')  &&  strcmpi(dunits,'ft')
         wd=wd/0.3048;
      else
         error(['Incompatible units: ',dunits,', ',units])
      end
   end
else
   wd=param.wd;
end

if ischar(param.ekb)
   try
%      ekb=getfield(wlog,param.ekb);
      ekb=wlog.(param.ekb);
   catch
      disp([' Well log has no parameter "',param.ekb,'".'])
      error('Abnormal termination')
   end

   units=getParameterUnits(wlog,param.ekb);
   if ~strcmpi(units,dunits)
      if strcmpi(units,'ft')  &&  strcmpi(dunits,'m')
         ekb=ekb*0.3048;
      elseif strcmpi(units,'m')  &&  strcmpi(dunits,'ft')
         ekb=ekb/0.3048;
      else
         error(['Incompatible units: ',dunits,', ',units])
      end
   end
else
   ekb=param.ekb;
end

if wlog.step > 0
   total=round((wd+ekb)/wlog.step)*wlog.step;
else
   total=wd+ekb;
end

wlog.curves(:,1)=wlog.curves(:,1)-total;
wlog.first=wlog.curves(1,1);
wlog.last=wlog.curves(end,1);

wlog.curve_info{1,3}=[wlog.curve_info{1,3},' (from water bottom)'];


%       Check if there is a curve OWT and/or TWT and change it as well
if strcmp(dunits,'ft')
   wt1=wd/4888;
else
   wt1=wd/1490;
end

idx=find(ismember(wlog.curve_info,cm.twt));
if length(idx) == 1
   if strcmp(wlog.curve_info{idx,2},'ms')
      wlog.curves(:,idx)=wlog.curves(:,idx)-wt1*2000;
   elseif strcmp(wlog.curve_info{idx,2},'s')
      wlog.curves(:,idx)=wlog.curves(:,idx)-wt1*2;
   else
      error(['Well log has unknown two-way time units: "',wlog.curve_info{idx,2}])
   end
end

idx=find(ismember(wlog.curve_info,cm.owt));
if length(idx) == 1
   if strcmp(wlog.curve_info{idx,2},'ms')
      wlog.curves(:,idx)=wlog.curves(:,idx)-wt1*1000;
   elseif strcmp(wlog.curve_info{idx,2},'s')
      wlog.curves(:,idx)=wlog.curves(:,idx)-wt1;
   else
      error(['Well log has unknown one-way time units: "',wlog.curve_info{idx,2}])
   end
end
 
