function seismic=s_new_time(seismic,old_new,varargin)
% Function creates new "time" scale (e.g. for time-to-depth conversion: in
% this case the first column would be times and the second columns the 
% corresponding depths).
% Trace samples with times earlier than old_new(1,1) or later than 
% old_new(end,1) are discarded.
%
% Written by: E. Rietsch, July 24, 2001
% Last updated: December 13, 2006; use new convention for 'null' field
%
%          seismic=s_new_time(seismic,old_new,varargin)
% INPUT
% seismic  seismic structure
% old_new  two-column matrix; the first column represents old (original) times,
%          the second column the corresponding new times
%          New times corresponding to old times that not in array "old_new" are
%          determined by linear interpolation 
% varargin one or more cell arrays; the first element of each cell array is 
%          a keyword string, the following arguments contains a parameter(s). 
%          Accepted keywords are:
%     'interpolation'  type of interpolation used to convert to the new
%          "time" scale. Possible values are those strings allowed
%          for input argument "method" of MATLAB function "interp1"
%          (e.g. 'linear', 'cubic', 'spline')
%          Default: {'interpolation','cubic'}
%     'step'   sample interval of output data set
%          Default: {'step',seismic.step)
%     'units'  units of measurement of the new "time" scale
%          Default: {'units',seismic.units}
% OUTPUT
% seismic  seismic structure with new "time" scale

global S4M

if ~isstruct(seismic)
   error(' First input dataset must be a seismic structure')
end

%	Set defaults for input parameters
param.interpolation='cubic';
param.step=seismic.step;
param.units=seismic.units;

%       Decode input arguments, modify defaults if necessary
param=assign_input(param,varargin);

newt=interp1q(old_new(:,1),old_new(:,2),(seismic.first:seismic.step:seismic.last)');
idx=find(~isnan(newt));
newt=newt(idx);
seismic.first=ceil(newt(1)/param.step)*param.step;
seismic.last=floor(newt(end)/param.step)*param.step;
seismic.step=param.step;
seismic.units=param.units;
seismic.traces=interp1(newt,seismic.traces(idx,:), ...
    (seismic.first:seismic.step:seismic.last)',param.interpolation);

%	 Check for null values
if ~any(isnan(seismic.traces))
   seismic.null=[];
else
   seismic.null=NaN;
end

%	Append history field
if S4M.history
   seismic=s_history(seismic,'append',param.interpolation);
end
