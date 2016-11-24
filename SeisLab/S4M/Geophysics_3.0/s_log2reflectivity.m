function reflect=s_log2reflectivity(wlog,step,varargin)
% Create reflection coefficient series in time from well log in 
% depth or time. A specific impedance curve to use can be specified via the
% "redefine curve name" facility: {'aimp',impedance_curve}  where 
% "impedance_curve" is the mnemonic of the impedance curve to use.
%
% Written by: E. Rietsch: December 17, 2003
% Last updated: October 26, 2005: Create history field
%
%           reflect=s_log2reflectivity(wlog,step,varargin)
% INPUT
% wlog      log structure with impedance curve (default is "aImp"),  or "DTp" and "rho", 
%           or "Vp" and "rho".
%           Mnemonics can be changed: For example {'dtp','dt'} implies the the 
%           sonic curve is represented by "dt" (by default, curve mnemonics 
%           are not case sensitive).
% step      seismic sample interval
% varargin  one or more cell arrays; the first element of each cell array is a keyword,
%           the other elements are parameters. Presently, keywords are:
%     'depth_time'  depth value and the corresponding time to establish time to top of log
%           the depth value must satisfy wlog.first <= depth_time{1} <= wlog.last
%           Default:{'depth_time',first,0} : "first" is the depth of the first
%                                  non-NaN  sample of the sonic or velocity log;
%                                  thus first >= wlog.first. Only necessary if 
%                                  there is no curve with mnemonic "TWT".
%
% OUTPUT
% reflect   seismic structure representing reflection coefficients
%
% EXAMPLE
%           wlog=l_data;
%           reflect=s_log2reflectivity(wlog,4,{'depth_time',wlog.first,1000})
%           s_wplot(reflect,{'interpol','linear'})

global S4M

%	Set default parameters
param.depth_time=[];
 
[param,cm]=l_assign_input(param,varargin);

%       Input checking
if ~isempty(param.depth_time)
   if length(param.depth_time) ~= 2
      error('Parameter "depth_time" must have two elements: a depth and the associated time.')
   elseif iscell(param.depth_time)
      param.depth_time=cell2num(param.depth_time);
   end
end

[dummy,ierr]=curve_index1(wlog,cm.aimp);   %#ok  First output argument is a place holder.
if ierr
   disp(' Acoustic impedance computed.')
   wlog1=l_seismic_acoustic(wlog,{'Vp',cm.vp},{'DTp',cm.dtp},{'rho',cm.rho}, ...
                        {'aImp',cm.aimp},{'twt',cm.twt});
else
   disp([' Reflectivity computed from curve "',cm.aimp,'".'])
   wlog1=wlog;
end

%       Create TWT curve --- if necessary or required
if ~iscurve(wlog1,cm.twt)
   if isempty(param.depth_time)
      wlog1=l_depth2time(wlog1,{'output',cm.twt});
   else
      wlog1=l_depth2time(wlog1,{'depth_time',param.depth_time}, ...
         {'output','twt'});
   end

elseif ~isempty(param.depth_time)
   wlog1=l_depth2time(wlog1,{'action','replace'},{'output',cm.twt});
end


%       Make TWT curve the "official" depth curve and resample if necessary
index=curve_index1(wlog1,cm.twt);
if index > 1
   wlog1=l_switch_depth(wlog1,cm.twt);
end
if wlog1.step ~= step
   wlog1=l_resample(wlog1,step);
end

%	Convert impedance to seismic dataset and compute reflection coefficients
impedance=s_rm_trace_nulls(s_convert(l_gc(wlog1,cm.aimp),wlog1.first,step),false);
                        
reflect=s_reflcoeff(impedance);
reflect.name=['Reflectivity (',wlog.name,')'];

if S4M.history
   reflect=s_history(reflect,'add',['Created from log "',wlog.name,'", curve "',cm.aimp,'"']);
end
