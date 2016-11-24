function logout=l_resample(wlog,step,varargin)
% Function resamples all curves of a log. Gaps in log curves (NaNs) are 
% interpolated prior to resampling (l_fill_gaps). Gaps are filled even 
% if "step" is equal to wlog.step.
% Leading and/or trailing null values are ignored.
%
% Written by: E. Rietsch:
% Last updated: July 22, 2009: Keep field "null" even if there are no nulls.
%
%         logout=l_resample(wlog,step,varargin)
% INPUT
% wlog    input log
% step    New step size. No default
% varargin  cell array; the first element of the cell array is a keyword string,
%         the following argument is a parameter. 
%         Accepted keywords are:
%     'option'  Type of interpolation performed to avoid aliasing if the new 
%               step size is larger than the original step size. 
%               Possible values are: 'nearest','mean', and 'median'.
%         'median' can be used only if the new step size is an integer multiple of
%               the step size of the input data. ('median' not yet implemented)
%         'nearest' is actually unrelated to the aliasing problem; the value 
%               assigned to a new depth is that of the nearest depth in the 
%               original log. This option is of particular interest for 
%               blocked logs as it preserves the blocky character.
%         Default: {'option','mean'}
% OUTPUT
% logout   resampled log

% UPDATE HISTORY
%         December 23, 2005: Turn off warnings in "interp1" if NaN's are
%         encountered


global S4M

%       Set defaults for input parameters
param.option='mean';

%       Decode and assign input arguments
param=assign_input(param,varargin);

if isfield(wlog,'null')        % Fill gaps in log curves
   wlog=l_fill_gaps(wlog);
end

if wlog.step == step               
   logout=wlog;
   return
end

[nsamp,ncurves]=size(wlog.curves);
if nsamp <= 1
   logout=wlog;
   return
end

logout.first=ceil(wlog.first/step)*step;
logout.step=step;
depth=(logout.first:step:wlog.last)';
logout.last=depth(end);
logout.curves=zeros(length(depth),ncurves);
logout.curves(:,1)=depth;

if isfield(wlog,'null')  &&  S4M.matlab_version >= 0  % Turn off warnings caused by NaN's in curves
   warning('off','MATLAB:interp1:NaNinY')
end

switch param.option

        case 'nearest'
logout.curves(:,2:end)=interp1(wlog.curves(:,1),wlog.curves(:,2:end),depth,'nearest');

        case {'mean','median'}

if wlog.step > 0                       % Handle uniformly sampled data
   if wlog.step < step                 % New step size larger than original one
      ratio=logout.step/wlog.step;
      if strcmpi(param.option,'median')
         disp(' Option "median" not yet implemented')
      elseif strcmpi(param.option,'mean')
         for ii=2:ncurves
            temp=wlog.curves(:,ii);
            idx=find(~isnan(temp));
            if ~isempty(idx)
               temp(idx)=mysmooth(temp(idx),ratio);
               logout.curves(:,ii)=interp1(wlog.curves(:,1),temp,depth,'*linear');
            else
               logout.curves(:,ii)=NaN;
            end
         end

      else
         error([' Unknown option "',param.option,'"; check input arguments'])
      end

   else                                 % New step size is smaller than the old one
      for ii=2:ncurves
         logout.curves(:,ii)=interp1(wlog.curves(:,1),wlog.curves(:,ii),depth,'*linear');
      end
   end

else                                    % Handle non-uniformly sampled data
   if max(diff(wlog.curves(:,1))) <= step  % New step size larger than original one
      if isfield(wlog,'null')
         no_nan=0;
      else
         no_nan=1;
      end
      for ii=2:ncurves
         logout.curves(:,ii)=interp_av(wlog.curves(:,1),wlog.curves(:,ii),depth,no_nan);
      end

   else                                 % New step size is smaller than the original one
      for ii=2:ncurves
         logout.curves(:,ii)=interp1q(wlog.curves(:,1),wlog.curves(:,ii),depth);
      end
   end
end  

        otherwise
        error(' Unknown option')
       
end

%	Turn on warnings regarding NaN's in "interp1" that had been turned off before
if S4M.matlab_version >= 7 
   warning('on','MATLAB:interp1:NaNinY')
end


%       Copy rest of fields
logout=copy_fields(wlog,logout);

%       Handle logicals (if they exist)
index=find(ismember(lower(wlog.curve_info(:,2)),'logical'));
if ~isempty(index)
   temp=logout.curves(:,index);
   temp(temp > 0.33  &  temp < 0.67)=NaN;
   temp=round(temp);
   logout.curves(:,index)=temp;
   alert({'Interpolation of curves with units "logical" (such as "sand", "shale", etc.)';...
         'is not reliable. Such curves should be recomputed.'})
end

%       Check for NaNs
if any(any(isnan(logout.curves(:,2:end))))
   logout.null=NaN;
end

%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%

function yy=interp_av(x,y,xi,no_nan)
% Function interpolates x,y pairs to new sampling; the interpolation 
% is performed in such a way that the output sample for abscissa xi(k)
% is the average of the function y in the interval xi(k-1)+xi(k))/2, (xi(k)+xi(k+1))/2
%         yy=interp_av(x,y,xi)
% INPUT
% x,y           abscissa and associated ordinate of given function
% xi            desired abscissa values
% no_nan        logical; no_nan is true if there are no NaNs in y, it is false otherwise
% OUTPUT
% yy  interpolated ordinates

if no_nan
   u=cumquad(y,x);
   xih=[xi(1);(xi(1:end-1)+xi(2:end))/2;xi(end)];
   temp=interp1q(x,u,xih);
   yy=diff(temp)./diff(xih);

else
   idx=find(~isnan(y));
   if isempty(idx)
      yy=NaN*zeros(size(xi));
      return
   end
   y1=y(idx); x1=x(idx);
   u=cumquad(y1,x1);
   xih=[xi(1);(xi(1:end-1)+xi(2:end))/2;xi(end)];
   temp=interp1q(x1,u,xih);
   yy=diff(temp)./diff(xih);
end



