function log_out=l_eqdepth(log)
% Funtion checks if log is equidistantly sampled in depth (log.step ~= 0)
% If this is the case it is copied to output.
% If this is not the case the curves are resampled with the median sample interval (step)
% rounded to 1 ft, 0.25 m, 1 ms, or 0.001 sec. The first depth is an integer multiple of
% the sample interval.
% Written by: E. Rietsch
% Last updated: January 5, 2001; new rules for sample interval selection
%
%              log_out=l_eqdepth(log)
% INPUT
% log       log structure
% OUTPUT
% log_out   log structure equidistantly sampled
%              log_out=l_eqdepth(log)
%
% See also: L_RESAMPLE

if log.step ~= 0
  log_out=log;
  return

else
  [nsamp,ncurves]=size(log.curves);
  if nsamp <= 1
    log_out=log;
    return
  end
  dd=diff(log.curves(:,1));
  step=median(dd);
  units=log.curve_info{1,2};
  if strcmpi(units,'m')
    step=round(step*4)*0.25;
  elseif strcmpi(units,'ft') || strcmpi(units,'ms')
    step=round(step);
  elseif strcmpi(units,'s')
    step=round(step*1000)/1000;
  else
    error([' Unknown depth units: ',units])
  end

%       Determine new first and last depth
  log_out.first=ceil(log.first/step)*step;
  log_out.last=floor(log.last/step)*step;
  log_out.step=step;
  temp=(log_out.first:step:log_out.last)';
  log_out.curves=zeros(length(temp),ncurves);

  for ii=2:ncurves
     log_out.curves(:,ii)=interp1Q(log.curves(:,1),log.curves(:,ii),temp);
  end
  log_out.curves(:,1)=temp;
%  end
end

idx=find(isnan(log_out.curves(:,2:ncurves)),1);
if ~isempty(idx)
   log_out.null=NaN;
end

%       Copy rest of fields
log_out=copy_fields(log,log_out);
