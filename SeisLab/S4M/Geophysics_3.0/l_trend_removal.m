function [wlog,aux]=l_trend_removal(wlog,mnem,refdepth,varargin)
% Correct values of a particular curve to a particular depth, called 
% reference depth (remove a linear trend of the form a+b*depth)
%               new_curve = (old_curve/trend)*trend_at_reference_depth
%
% Written by: E. Rietsch: August 8, 2003
% Last updated: July 31, 2008: Fix logical if
%
%         [wlog,aux]=l_trend_removal(wlog,mnem,refdepth,varargin)
% INPUT
% wlog     well log
% mnem     mnemonic of curve whose trend should be removed
% refdepth  reference depth; depth at which the original and the trend-removed
%          curve should be the same
% varargin one or more cell arrays; the first element of each cell array 
%          is a keyword string, the following arguments contains a parameter(s). 
%          Accepted keywords are:        
%      'mnemonic'  mnemonic of the new curve;
%             Default: {'mnemonic',[mnem,'_normal']
%      'depths'    first and last depth of the interval over which the 
%             trend should be computed; it is then applied to the whole log, 
%             not just that interval
%             Default: {'depths',wlog.first,wlog.last}
% OUTPUT
% wlog     input well log with new curve. 
% aux      structure with intercept "a" and slope "b" of the trend
%
% EXAMPLE
%          wlog=l_data;
%          [wlog,aux]=l_trend_removal(wlog,'vp',10000);
%          l_plot1(wlog,{'curves','vp','vp_normal'})
%          aux

%       Defaults of input arguments
param.alpha=0.45;
param.mnemonic=[mnem,'_normal'];
param.depths=[wlog.first,wlog.last];

%       Use input parameters to change defaults
param=assign_input(param,varargin);

if iscell(param.depths)
   param.depths=cell2mat(param.depths);
end

% dummy=l_select(wlog,[{'depth'},param.depths]);

[curve,info]=l_gc(wlog,mnem);
depth=wlog.curves(:,1);
idx=find(depth >= param.depths(1) & depth <= param.depths(2));
if isempty(idx)
   disp(' No log values in depth range requested')
   error('Abnormal termination')
end

[a,b]=l1_slope_intercept(depth(idx),curve(idx),param.alpha);

trend=a+b*depth;
curve=(curve./trend)*(a+b*refdepth);
wlog=l_curve(wlog,'add_ne',param.mnemonic,curve,info{2},[info{3}, ...
            ' (corrected for depth of ',num2str(refdepth),' ', ...
            wlog.curve_info{1,2},')']);
if nargout > 1;
   aux.a=a;
   aux.b=b;
end
