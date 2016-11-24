function wlog=l_depth2time(wlog,varargin)
% Compute two-way time by integrating sonic log and adding it to the input log structure.
% By default, the function assumes that interval transit time and/or compressional velocity
% have the standard mnemonics. This can be changed by temporarily redefining the standard
% mnemonics, e.g. {'DTp','DTCO'} specifies that the mnemonic of the sonic log is 'DTCO' rather
% 'DTp'. Standard mnemonics are defined in function "presets". Likewise, the output curve has
% the standard mnemonic "TWT" (or "OWT", depending on the choice made via keyword "output').
% This too can be changed be temporarily redefining the standard mnemonics.
%
% Written by: E. Rietsch: February 16, 2001
% Last updated: August 20, 2007: bug fix
%
%          wlog=l_depth2time(wlog,varargin)
% INPUT
% wlog     log structure with sonic log (DTp) and/or velocity (Vp);
%          if both are available, the interval transit time is used
% varargin one or more cell arrays; the first element of each cell array is a keyword string,
%          the following arguments contains a parameter(s). 
%          Accepted keywords are:
%          'depth_time'  depth value and the corresponding time to establish time to top of log
%                 the depth value must satisfy wlog.first <= depth_time{1} <= wlog.last
%                 The time is expected in msec.
%                 Default:{'depth_time',first,0} : "first" is the depth of the first non-NaN 
%                      sample of the sonic or velocity log; thus first >= wlog.first
%          'action' describes how to handle new curve. Possible values are:
%                'add'     gives an error message if a curve with this mnemonic already exists
%                'replace' gives an error message if a curve with this mnemonic does not exist
%                'add_ne'  (add --- no error) adds a new curve or replaces an existing one
%                'Default: {'action','add_ne'}
%          'output'  Type of output, possible values are: 'twt' (two-way time) or 'owt' (one-way
%                time. Default: {'output','twt'}  
%          'description'  string with curve description.
%                 Default: {'description','Two-way time'}  if 'output' is 'twt'
%                          {'description','One-way time'}  if 'output' is 'owt'  
% OUTPUT
% wlog     input log structure with curve TWT (or OWT) appended (time units are "ms")
%
% EXAMPLE
%       wlog=l_data;
%       wlog=l_depth2time(wlog,{'depth_time',9000,2000});
%       l_plot(wlog,{'curves','TWT'})
%       mytitle('Two-way time as a function of depth')

global ABORTED

ABORTED=false;

%       Set defaults for input parameters
param.depth_time=[];
param.action='add_ne';
param.output='twt';
param.description='';

%       Decode and assign input arguments
[param,cm]=l_assign_input(param,varargin);

if strcmpi(param.output,'twt')
   fact=2;
   mnem_out=cm.twt;
   if isempty(param.description)
      param.description='Two-way time';
   end
else
   fact=1;
   mnem_out=cm.owt;
   if isempty(param.description)
      param.description='One-way time';
   end
end

dunits=wlog.curve_info{1,2};     % Depth units

if ~strcmpi(dunits,'m')  &&  ~strcmpi(dunits,'ft')
   myerror([' Depth units are "',dunits,'"; must be "m" or "ft"'])
   return
end

[idx,ier]=curve_index1(wlog,cm.dtp);
if ier
   if ~isempty(idx)        
       myerror([' More than one curve with mnemonic "',cm.dtp,'"'])
       return
   end
   [idx,ier]=curve_index1(wlog,cm.vp);
   if ier
      if ~isempty(idx)  
         myerror([' More than one curve with mnemonic "',cm.vp,'"'])
      else
         myerror([' No curve with mnemonic "',cm.vp,'" found'])
      end
      return
   end
   
   %     Remove leading and trailing rows for which the velocity curve has 
   %     null values
   wlog=l_rm_nulls(wlog,'any',cm.vp);
   
   vel=wlog.curves(:,idx);
   index=find(~isnan(vel));
   if isempty(index)
      myerror(' Velocity log consists of nothing but null values')
      return
   end
   if isempty(param.depth_time)
      param.depth_time=[wlog.curves(index(1),1),0];
   elseif iscell(param.depth_time)
      param.depth_time=cell2num(param.depth_time);
   end
 
   units=wlog.curve_info{idx,2};
   if strcmp(dunits,'m')
      if strcmp(units,'ft/s')
          fact=fact*1000/0.3048;
%         time=cumquad((fact*1000/0.3048)./vel(index),wlog.curves(index,1));
      elseif strcmp(units,'m/s')
          fact=fact*1000;
%         time=cumquad((fact*1000)./vel(index),wlog.curves(index,1));
      else
         myerror([' Unknown velocity units "',units,'"'])
         return
      end

   else
      if strcmp(units,'m/s')
         fact=fact*304.8;
%         time=cumquad((fact*304.8)./vel(index),wlog.curves(index,1));
      elseif strcmp(units,'ft/s')
	 fact=fact*1000;
%         time=cumquad(fact*1000./vel(index),wlog.curves(index,1));
      else
         myerror([' Unknown velocity units "',units,'"'])
         return
      end
   end
   time=cumquad(fact./vel(index),wlog.curves(index,1));

else    % Sonic log available
   %     Remove leading and trailing rows for which the sonic curve has 
   %     null values
   wlog=l_rm_nulls(wlog,'any',cm.dtp);
   itt=wlog.curves(:,idx);
   index=find(~isnan(itt));
   if isempty(index)
      myerror(' Sonic log consists of nothing but null values')
      return
   end
   if isempty(param.depth_time)
      param.depth_time=[wlog.curves(index(1),1),0];
   elseif iscell(param.depth_time)
      param.depth_time=cell2num(param.depth_time);
   end

   units=wlog.curve_info{idx,2};
   if strcmp(dunits,'m')
      if strcmp(units,'us/ft')
         fact=fact/304.8;
%         time=cumquad((fact/304.8)*itt(index),wlog.curves(index,1));
      elseif strcmp(units,'us/m')
         fact=fact/1000;
%         time=cumquad((fact*0.001)*itt(index),wlog.curves(index,1));
      else
         myerror([' Unknown velocity units "',units,'"'])
         return
      end
   else           % dunits is "ft"
      if strcmp(units,'us/m')
         fact=fact/0.0003048;
%         time=cumquad((fact*0.0003048)*itt(index),wlog.curves(index,1));
      elseif strcmp(units,'us/ft')
         fact=fact/1000;
%         time=cumquad((fact*0.001)*itt(index),wlog.curves(index,1));
      else
         myerror([' Unknown sonic units "',units,'"'])
         return
      end
   end
   time=cumquad(fact*itt(index),wlog.curves(index,1));
end

%       Find time to top of log
temp=interp1(wlog.curves(index,1),time,param.depth_time(1),'*linear');

if isnan(temp)
   myerror([' Depth defined via keyword ''depth_time'' (', ...
      num2str(param.depth_time(1)),') must be between ', ...
      num2str(wlog.curves(index(1),1)),' and ', ...
      num2str(wlog.curves(index(end),1)),' ',wlog.curve_info{1,2}])
   return
end

time=time-temp+param.depth_time(2);
temp=NaN(size(wlog.curves,1),1,class(wlog.curves));
temp(index)=time;
wlog=l_curve(wlog,param.action,mnem_out,temp,'ms',param.description);

if strcmpi(param.output,'twt')
   wlog=add_curve_type(wlog,{mnem_out,'twt','two-way time'});
else
   wlog=add_curve_type(wlog,{mnem_out,'owt','one-way time'});
end  
