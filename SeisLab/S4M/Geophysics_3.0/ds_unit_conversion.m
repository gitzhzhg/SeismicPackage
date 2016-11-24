function dataset=ds_unit_conversion(dataset,varargin)
% Function converts units of measurements of columns/curves/headers/panels and 
% parameters in a dataset structure.
% Datasets supported are: seismic data, well logs, tables,and pseudo-wells.  
% Units of measurement for the time/depth axis are also modified for well logs
% and pseudo-wells.
%
% Written by: E. Rietsch: December 10, 2007
% Last updated:
%
%          dataset=ds_unit_conversion(dataset,varargin)
% INPUT
% dataset  log structure or table structure
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
% dataset  dataset structure with new units of measurement
%
% EXAMPLES
%        %       Change 'ft' to 'm' and '%'  to 'fraction'  
%        wlog=l_data;
%        wlog=ds_unit_conversion(wlog,{'ft','m'},{'%','fraction'});
%        l_curve(wlog)
%
%        %       Change 'ft/s' to 'm/s' (but only for Vs)                                                                
%        wlog=ds_unit_conversion(wlog,{'ft/s','m/s','Vs'});  
%        l_curve(wlog)
%
%        %	Change units of measurement in a seismic header
%        seismic=ds_add_header(s_data,100,{'offset','m','Offset'});
%        ds_header(seismic)
%        seismic=ds_unit_conversion(seismic,{'m','ft'});
%        ds_header(seismic)


global S4M

ier=0;

for kk=1:length(dataset)
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
   [dataset,ier]=unit_conversion(dataset,units,'new=old*100');

else
   error([' No conversion option from "',units{1},'" to "',units{2},'" found.'])
end
%=========================================================== 
      		case 'ft'
if strcmp(units{2},'m')		       % ft ==> m
   dataset=unit_conversion(dataset,units,'new=old*0.3048');
   [dataset,ier]=unit_conversion(dataset,{'1/ft','1/m'},'new=old/0.3048');

else
   error([' No conversion option from "',units{1},'" to "',units{2},'" found.'])
end
%=========================================================== 
      		case 'g/cm3'
if strcmp(units{2},'ppg')		% g/cm3 ==> ppg
   [dataset,ier]=unit_conversion(dataset,units,'new=old*8.35');

elseif strcmp(units{2},'kg/m3')       	% g/cm3 ==> kg/m3
   [dataset,ier]=unit_conversion(dataset,units,'new=old*1000');

else
   error([' No conversion option from "',units{1},'" to "',units{2},'" found.'])
end
%===========================================================
      		case 'kg/m3'
if strcmp(units{2},'ppg')		% kg/m3 ==> ppg
  [dataset,ier]=unit_conversion(dataset,units,'new=old*0.00835');

elseif strcmp(units{2},'g/cm3')       	% kg/m3 ==> g/cm3
  [dataset,ier]=unit_conversion(dataset,units,'new=old/1000');

else
  error([' No conversion option from "',units{1},'" to "',units{2},'" found.'])
end
%=========================================================== 
      		case 'm'
if strcmp(units{2},'ft')		% m ==> ft
  dataset=unit_conversion(dataset,units,'new=old/0.3048');
  [dataset,ier]=unit_conversion(dataset,{'1/m','1/ft'},'new=old*0.3048');

else
  error([' No conversion option from "',units{1},'" to "',units{2},'" found.'])
end
%=========================================================== 
      		case '%'
if strcmp(units{2},'fraction')		% % ==> fraction
   [dataset,ier]=unit_conversion(dataset,units,'new=old/100');

else
   error([' No conversion option from "',units{1},'" to "',units{2},'" found.'])
end
%=========================================================== 
      		case 'us/ft'
if strcmp(units{2},'us/m')		% us/ft ==> us/m
   [dataset,ier]=unit_conversion(dataset,units,'new=old/0.3048');

else
   error([' No conversion option from "',units{1},'" to "',units{2},'" found.'])
end
%=========================================================== 
      		case 'us/m'
if strcmp(units{2},'us/ft')		% us/m ==> us/ft
   [dataset,ier]=unit_conversion(dataset,units,'new=old*0.3048');

else
   error([' No conversion option from "',units{1},'" to "',units{2},'" found.'])
end
%=========================================================== 
      		case 'ft/s'
if strcmp(units{2},'m/s')		% ft/s ==> m/s
   [dataset,ier]=unit_conversion(dataset,units,'new=old*0.3048');

else
   error([' No conversion option from "',units{1},'" to "',units{2},'" found.'])
end
%=========================================================== 
      		case 'm/s'
if strcmp(units{2},'ft/s')		% m/s ==> ft/s
  [dataset,ier]=unit_conversion(dataset,units,'new=old/0.3048');

else
   error([' No conversion option from "',units{1},'" to "',units{2},'" found.'])
end
%===========================================================
      		case 'ms'
if strcmp(units{2},'s')		% ms ==> s
  [dataset,ier]=unit_conversion(dataset,units,'new=old/1000');

else
   error([' No conversion option from "',units{1},'" to "',units{2},'" found.'])
end
%=========================================================== 
      		case 's'
if strcmp(units{2},'ms')		% s ==> ms
  [dataset,ier]=unit_conversion(dataset,units,'new=old*1000');

else
  error([' No conversion option from "',units{1},'" to "',units{2},'" found.'])
end
%===========================================================

            	otherwise
    error([' No conversion option from "',units{1},'" available'])

    end			% End of "switch' block
%===========================================================
      end		% End of if statement checking if the two units are the same
   end
end

if ~ier
   % Only insignificant warnings
end


%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%

function [dataset,ier]=unit_conversion(dataset,units,expression)
% Function converts values of one or more curves with the same units of 
% measurement using the relationship in input argument "expression"
% INPUT
% dataset    log structure
% units  cell array with at least two elements (strings) representing original and 
%        new units of measurement; additional elements (if given) represent curve 
%        mnemonics
% expression  Matlab expression of the form 'new=function(old)'
%        Examples: 'new=old*0.3048'
%                  'new=10*log(old)'
% OUTPUT
% dataset    log structure with the updated curve(s)
% ier    error code: no error ==> ier = false
%                    error    ==> ier = true
%        An "error" occurs if no column/curve/panel/header/ with specified 
%        units of measurement is found or if curve mnemonics are specified
%        ("units" has more than 2 elements) but are not fund in dataset       
%        An ALERT message is printed if ier is true and global variable 
%        S4M.alert == 1


switch dataset.type
case 'well_log'
   [dataset,ier]=convert4log_and_table_no2(dataset,units,expression,'curve_info','curves');

case 'table'
%   dataset=t_loose2compact(dataset);
   [dataset,ier]=convert4log_and_table_no2(t_loose2compact(dataset),units, ...
                 expression,'column_info','columns');
case 'seismic'
   if isfield(dataset,'header_info')
      [dataset,ier]=convert4header_no3(dataset,units,expression);
   end

case 'pseudo-wells'
   [dataset,ier]=convert4panels_no4(dataset,units,expression);
   if isfield(dataset,'header_info')
      [dataset,ier1]=convert4header_no3(dataset,units,expression);
      ier=max(ier,ier1);
   end

otherwise
   error(['Unsupported type of dataset: "',dataset.type,'".'])
end

%       Convert units of parameters listed in field "parameter_info" of a dataset
dataset=convert4parameters_no5(dataset,units,expression);


%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%

function [dataset,ier]=convert4log_and_table_no2(dataset,units,expression,info_field,data_field)


%	Check dataset fields
[index,ier]=get_indices_no6(dataset,units,info_field);

if ~ier
   old=dataset.(data_field)(:,index);  %#ok  Matrix "old" is used in "eval"
   eval([expression,';']);
   dataset.(data_field)(:,index)=new;  % Matrix "new" is created in "eval" 
   alert([' Log curve(s) "',cell2str(dataset.(info_field)(index,1),', '), ...
          '" changed to units "',units{2},'".'],'ds_unit_conversion')
   dataset.(info_field)(index,2)=units(2);
end 

%	Handle change in the units for depth
if ~isempty(index) && index(1) == 1  &&  istype(dataset,'well_log')
   dataset.first=dataset.curves(1,1);
   dataset.last=dataset.curves(end,1);
   old=dataset.step;        %#ok  Matrix "old" is used in "eval"
   eval([expression,';']);  % Matrix "new" is created in "eval" 
   dataset.step=new;
   dataset.units=units{2};
end


%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%

function [dataset,ier]=convert4header_no3(dataset,units,expression)

%	Check dataset fields
[index,ier]=get_indices_no6(dataset,units,'header_info');

if ~ier
   old=dataset.headers(index,:);  %#ok  Matrix "old is used in "eval"
   eval([expression,';']);
   dataset.headers(index,:)=new;  % Matrix "new" is created in "eval" 
   alert([' Header(s) "',cell2str(dataset.header_info(index,1),', '), ...
          '" changed to units "',units{2},'".'],'ds_unit_conversion')
   dataset.header_info(index,2)=units(2);
end 


%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%

function [dataset,ier]=convert4panels_no4(dataset,units,expression)

%	Check dataset fields
[index,ier]=get_indices_no6(dataset,units,'panel_info');


if ~ier
   for ii=1:length(index)
      panel=dataset.panel_info{index(ii),1};
      old=dataset.(panel);    %#ok  Matrix "old" is used in "eval" 
      eval([expression,';']); % Matrix "new" is created in "eval" 
      dataset.(panel)=new;  
      alert([' Panel "',panel,'" changed to units "',units{2},'".'],'ds_unit_conversion')
      dataset.panel_info(index(ii),2)=units(2);
   end
end 

%	Handle change in the units for depth
if strcmp(dataset.units,units{1})
   old=[dataset.first,dataset.last,dataset.step]; %#ok Matrix "old" is used in "eval"
   eval([expression,';']);    % Matrix "new" is created in "eval" 
   dataset.first=new(1);   
   dataset.last=new(2);
   dataset.step=new(3);
   dataset.units=units{2};
end

%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%

function dataset=convert4parameters_no5(dataset,units,expression)

%	Check parameters for units that need to be converted      
if isfield(dataset,'parameter_info')
   params=dataset.parameter_info(:,1);
   for ii=1:length(params)
      temp=param_info(dataset,params{ii});
      if strcmp(temp{2},units{1})
         old=dataset.(params{ii});  %#ok  "old is used in "eval"
         eval([expression,';']);
         %    Matrix "new" is created in "eval" 
         dataset=ds_add_parameter(dataset,new,{params{ii},units{2},temp{3}});
         alert([' Parameter "',params{ii},'" changed.'],'ds_unit_conversion')
      end
   end    
end


%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%

%	Check dataset fields
function [index,ier]=get_indices_no6(dataset,units,info_field)

global S4M

ier=false;

index=find(ismember(dataset.(info_field)(:,2),units{1}));
if isempty(index)
   if S4M.alert
      disp([' Alert from "ds_unit_conversion": no curve/column/header/panel with units "',units{1},'" found.'])
   end
   ier=true;

elseif length(units) > 2      % Are there specific mnemonics for which to convert units
   idx=find(ismember(lower(dataset.(info_field)(index,1)),lower(units(3:end))));
   if isempty(idx)
      if S4M.alert
         disp([' Alert from "ds_unit_conversion": no specified curve/column/header/panel with units "',units{1},'" found.'])
         disp(['                               curves with these units: ', ...
            cell2str(dataset.(info_field)(index,1),', ')])
      end
      ier=true;
   else
      index=index(idx);
   end
end
