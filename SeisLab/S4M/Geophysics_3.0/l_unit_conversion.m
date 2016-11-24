function wlog=l_unit_conversion(wlog,varargin)
% Function converts units of measurements of curves and parameters in
% a log structure.
%
% DEPRECATED: Use "ds_unit_conversion" instead.
%
% Written by: E. Rietsch: January 10, 2001
% Last updated: April 11, 2007: Bug fix
%
%          wlog=l_unit_conversion(wlog,varargin)
% INPUT
% wlog 	   log structure
% varargin cell arrays with at least two elements. The first element is a string representing 
%          an existing unit of measurement, the second is the desired unit of measurement. 
%          If additional elements are given they represent curve mnemonics which should be
%          changed. If no curves are found that have these units of measurements and/or
%          these mnemonics, an alert is printed (see keyword 'alert').
%          Possible pairs of units of measurement are (in alphabetical order) and
%          vice versa:
%          {'fraction','%'}
%          {'g/cm3','ppg'}, {'g/cm3','kg/m3'}         
%          {'m','ft'}    (this also converts '1/m' to '1/ft')
%          {'m/s','ft/s')
%          {'us/ft','us/m'}
%          {'s','ms'}
%                It is not an error if the two units are the same (e.g {'m','m'})
%
%          'alert' Print an alert. Possible values are 0 (false) and 1 (true).
%                This keyword has an effect only on those conversions following it. Hence,
%                it should be the first argument after the log structure
%                Default: {'alert',1}
% OUTPUT
% wlog      log structure with new units of measurement
%
% EXAMPLES
%        %       Change 'ft' to 'm' and '%'  to 'fraction'  
%        wlog=l_data;
%        wlog=l_unit_conversion(wlog,{'ft','m'},{'%','fraction'});
%        l_curve(wlog)
%
%        %       Change 'ft/s' to 'm/s' (but only for Vs)                                                                
%        wlog=l_unit_conversion(wlog,{'ft/s','m/s','Vs'});
%        l_curve(wlog)

global S4M

alert('DEPRECATED: Use "ds_unit_conversion" instead.')

ier=0;

for kk=1:length(wlog)
   for ll=1:length(varargin)
      units=varargin{ll};
      if strcmpi(units{1},'alert')
         S4M.alert=units{2};
      end
      if ~strcmpi(units{1},units{2})  &&  ~strcmpi(units{1},'alert')  % Perform conversion 
                                                     % only if the two units are different
         if size(units) < 2
            disp(units)
            error(' Input arguments: old and new units must be represented as a two-element cell')
         end

    switch units{1}

%=========================================================== 
      		case 'fraction'
if strcmp(units{2},'%')		       %  fraction ==> %
   [wlog,ier]=unit_conversion(wlog,units,'new=old*100');

else
   error([' No conversion option from "',units{1},'" to "',units{2},'" found'])
end
%=========================================================== 
      		case 'ft'
if strcmp(units{2},'m')		       % ft ==> m
   wlog=unit_conversion(wlog,units,'new=old*0.3048');
   [wlog,ier]=unit_conversion(wlog,{'1/ft','1/m'},'new=old/0.3048');

else
   error([' No conversion option from "',units{1},'" to "',units{2},'" found'])
end
%=========================================================== 
      		case 'g/cm3'
if strcmp(units{2},'ppg')		% g/cm3 ==> ppg
   [wlog,ier]=unit_conversion(wlog,units,'new=old*8.35');

elseif strcmp(units{2},'kg/m3')       	% g/cm3 ==> kg/m3
   [wlog,ier]=unit_conversion(wlog,units,'new=old*1000');

else
   error([' No conversion option from "',units{1},'" to "',units{2},'" found'])
end
%===========================================================
      		case 'kg/m3'
if strcmp(units{2},'ppg')		% kg/m3 ==> ppg
  [wlog,ier]=unit_conversion(wlog,units,'new=old*0.00835');

elseif strcmp(units{2},'g/cm3')       	% kg/m3 ==> g/cm3
  [wlog,ier]=unit_conversion(wlog,units,'new=old/1000');

else
  error([' No conversion option from "',units{1},'" to "',units{2},'" found'])
end
%=========================================================== 
      		case 'm'
if strcmp(units{2},'ft')		% m ==> ft
  wlog=unit_conversion(wlog,units,'new=old/0.3048');
  [wlog,ier]=unit_conversion(wlog,{'1/m','1/ft'},'new=old*0.3048');

else
  error([' No conversion option from "',units{1},'" to "',units{2},'" found'])
end
%=========================================================== 
      		case '%'
if strcmp(units{2},'fraction')		% % ==> fraction
   [wlog,ier]=unit_conversion(wlog,units,'new=old/100');

else
   error([' No conversion option from "',units{1},'" to "',units{2},'" found'])
end
%=========================================================== 
      		case 'us/ft'
if strcmp(units{2},'us/m')		% us/ft ==> us/m
   [wlog,ier]=unit_conversion(wlog,units,'new=old/0.3048');

else
   error([' No conversion option from "',units{1},'" to "',units{2},'" found'])
end
%=========================================================== 
      		case 'us/m'
if strcmp(units{2},'us/ft')		% us/m ==> us/ft
   [wlog,ier]=unit_conversion(wlog,units,'new=old*0.3048');

else
   error([' No conversion option from "',units{1},'" to "',units{2},'" found'])
end
%=========================================================== 
      		case 'ft/s'
if strcmp(units{2},'m/s')		% ft/s ==> m/s
   [wlog,ier]=unit_conversion(wlog,units,'new=old*0.3048');

else
   error([' No conversion option from "',units{1},'" to "',units{2},'" found'])
end
%=========================================================== 
      		case 'm/s'
if strcmp(units{2},'ft/s')		% m/s ==> ft/s
  [wlog,ier]=unit_conversion(wlog,units,'new=old/0.3048');

else
   error([' No conversion option from "',units{1},'" to "',units{2},'" found'])
end
%===========================================================
      		case 'ms'
if strcmp(units{2},'s')		% ms ==> s
  [wlog,ier]=unit_conversion(wlog,units,'new=old/1000');

else
   error([' No conversion option from "',units{1},'" to "',units{2},'" found'])
end
%=========================================================== 
      		case 's'
if strcmp(units{2},'ms')		% s ==> ms
  [wlog,ier]=unit_conversion(wlog,units,'new=old*1000');

else
  error([' No conversion option from "',units{1},'" to "',units{2},'" found'])
end
%===========================================================

            	otherwise
    error([' No conversion option from "',units{1},'" available'])

    end			% End of "switch' block
%===========================================================
      end		% End of if statement checking if the two units are the same
   end
end

if ier > 0
   % Only insignificant warnings
end


%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%

function [wlog,ier]=unit_conversion(wlog,units,expression)
% Function converts values of one or more curves with the same units of 
% measurement using the relationship in input argument "expression"
% INPUT
% wlog   log structure
% units  cell array with at least two elements (strings) representing original and 
%        new units of measurement; additional elements (if given) represent curve 
%        mnemonics
% expression  matlab expression of the form 'new=function(old)'
%        Examples: 'new=old*0.3048'
%                  'new=10*log(old)'
% OUTPUT
% wlog    log structure with the updated curve(s)
% ier    error code: no error ==> ier = 0
%                    error    ==> ier = 1
%        error occur if no curve with specified units of measurement are found or
%        if curve mnemonics are specified ("units" has more than 2 elements)
%        but are not fund in wlog
%        An ALERT message is printed if ier == 1 and global variable S4M.alert == 1

global S4M

ier=0;

%	Check log curves
index=find(ismember(wlog.curve_info(:,2),units{1}));
if isempty(index)
  if S4M.alert
     disp([' Alert from "l_unit_conversion": no curve with units "',units{1},'" found'])
  end
  ier=1;
elseif length(units) > 2      % Are there specific curve mnemonics for which to convert units
  idx=find(ismember(lower(wlog.curve_info(index,1)),lower(units(3:end))));
  if isempty(idx)
     if S4M.alert
        disp([' Alert from "l_unit_conversion": no specified curve with units "',units{1},'" found'])
        disp(['                               curves with these units: ', ...
            cell2str(wlog.curve_info(index,1),', ')])
     end
     ier=1;
  else
    index=index(idx);
  end
end

if ier == 0
   old=wlog.curves(:,index);  %#ok  "old is used in "eval"
   eval([expression,';']);
   wlog.curves(:,index)=new;
   alert([' Log curve(s) ',cell2str(wlog.curve_info(index,1),', '),' changed to units "',units{2},'"'])
   wlog.curve_info(index,2)=units(2);
end 

%	Check parameters for units that need to be converted      
if isfield(wlog,'parameter_info')
   params=wlog.parameter_info(:,1);
   for ii=1:length(params)
      temp=param_info(wlog,params{ii});
      if strcmp(temp{2},units{1})
%         old=getfield(wlog,params{ii});    
         old=wlog.(params{ii});  %#ok  "old is used in "eval"
         eval([expression,';']);
         wlog=ds_add_parameter(wlog,new,{params{ii},units{2},temp{3}});
         alert([' Parameter "',params{ii},'" changed'])
      end
   end    
end

%	Handle change in the units for depth
if ~isempty(index) && index(1) == 1
   wlog.first=wlog.curves(1,1);
   wlog.last=wlog.curves(end,1);
   old=wlog.step;     %#ok  "old is used in "eval"
   eval([expression,';']);
   wlog.step=new;
   wlog.units=units{2};
end
