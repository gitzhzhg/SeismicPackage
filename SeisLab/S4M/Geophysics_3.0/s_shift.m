function seismic=s_shift(seismic,varargin)
% Function applies user-specified time shifts to seismic traces
%
% Written by: E. Rietsch: July 2001
% Last updated: March 23, 2008:  bug fix for constant shift; output
%                                precision is input precision
%
%            seismic=s_shift(seismic,varargin)
% INPUT
% seismic    seismic structure
% varargin   one or more cell arrays; the first element of each cell array 
%            is a keyword, the other elements are parameters. 
%            Presently, keywords are:
%    'shifts'  the second cell is either a constant time shift applied to 
%            all traces, cell an array with a shift for each trace of 
%            "seismic", or a header mnemonic.
%            Shifts are rounded to the nearest integer multiples of the  
%            sample interval "seismic.step"
%    'header'  header mnemonic to be used if shifts should be put into header (if they
%            are not already there). It must not yet exist in the input data set.
%            Default: {'header',[]}; i.e. not used                
%    'interpol'  interpolate trace values if the shifts are not integer
%            multiples of the sample interval; default is 'no' (shifts are 
%            rounded to the nearest multiple of the sample interval.
%            NOT YET IMPLEMENTED
%    'scale'   factor to apply to the shifts before they are applied to 
%            the traces this is generally meant for shifts stored in a 
%            header (e.g. multiplication by -1).
%    'option'  this keyword determines what should be done with data that 
%            are shifted outside the time range of "seismic". Possible values are:
%            'extend'   extend the time range to accommodate all shifts. It means that
%                       seisout.first=seismic.first+min(shifts)
%                       seisout.last=seismic.last+max(shifts)                          
%            'truncate' trace values that fall outside the time range of 
%                       seismic are discarded  
%            'circular' a circular time shift is applied.
%            Default: {'option','extend'}
%    'null'  value to use for samples that have been shifted into the time range of
%            "seisout". This will happen whenever different traces are shifted by 
%            different amounts AND 'option' is not 'circular'. 
%            Default: {'null',0}.
% OUTPUT                          
% seisout   seismic input dataset with shifts applied   
%
% EXAMPLE
%           seis=s_data;
%           ntr=size(seis.traces,2);
%           seis_shifted=s_shift(seis,{'shifts',linspace(-72,60,ntr)},{'header','shifts'});
%           s_compare(seis,seis_shifted)

% UPDATE HISTORY
%            March 30, 2006:  Apply scale before computing sample indices
%            November 6, 2007:  same input and output dataset

%     Set defaults of input parameters
param.shifts=0;
param.header=[];
param.interpol='no';
param.scale=1;
param.option='extend';
param.null=0;
param.precision=class(seismic.traces);

%       Replace defauls by actual input arguments
param=assign_input(param,varargin);
if isyes(param.interpol)
   alert(' Interpolation not yet implemented; shifts rounded to nearest sample instead.')
end

ntr=size(seismic.traces,2);

%       Get the shifts
if ~ischar(param.shifts)
   if iscell(param.shifts)
      error(' Cell array with keyword "shifts" can have only two elements (including ''shifts'')')
   end
   nsh=length(param.shifts);
   shifts=round(param.shifts*(param.scale/seismic.step));
   if nsh ~= ntr  &&  nsh ~= 1
      error([' Number of shifts (',num2str(nsh), ...
            ') differs from number of traces (',num2str(ntr),')']) 
   end

else         % Get shifts from headers
   shifts=s_gh(seismic,param.shifts)*(param.scale/seismic.step);
   shifts=round(shifts);
   nsh=ntr;
end

if nsh == 1
   seismic=constant_shift_no1(seismic,shifts,param);
else
   if isconstant(shifts,eps)      % all shifts are the same
      seismic=constant_shift_no1(seismic,shifts(1),param);
   else
      [seismic,shmin,shmax]=tracewise_shift_no2(seismic,shifts,param);
   end
end

%     Add header (if requested)
if ~isempty(param.header)
   seismic=s_header(seismic,'add_ne',param.header,shifts*seismic.step,seismic.units,'Shifts applied');
end


if isnan(param.null)
   seismic.null=NaN;
end

%    Append history field
if isfield(seismic,'history')
   try
      htext=[param.option,': Minimum shift: ',num2str(seismic.step*shmin), ...
                          ', maximum shift: ',num2str(seismic.step*shmax)];
   catch
      htext='';
   end
   seismic=s_history(seismic,'append',htext);
end 

%    Update dataset name
seismic.name=[seismic.name,' - shifted'];


%    Change precision to that of input data
if strcmpi(param.precision,'single')
   seismic=single(seismic);
else
   seismic=double(seismic);
end


%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%

function seismic=constant_shift_no1(seismic,shift,param)


switch param.option
case 'extend'
   shift=shift*seismic.step;
   seismic.first=seismic.first+shift;
   seismic.last=seismic.last+shift;     

case 'truncate'
   shift=shift*seismic.step;
   first=seismic.first;
   last=seismic.last;
   seismic.first=seismic.first+shift;
   seismic.last=seismic.last+shift;
   seismic=s_select(seismic,{'times',first,last});

case 'circular'
   seismic.traces=circshift(seismic.traces,double(shift));

otherwise
   error(['Unknown parameter for keyword "option": ',param.option])

end


%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%

function [seismic,shmin,shmax]=tracewise_shift_no2(seismic,shifts,param)
% Each trace has its own amount of shift

[nsamp,ntr]=size(seismic.traces);
first=seismic.first;
last=seismic.last;

shmin=min(shifts);
shmax=max(shifts);

switch param.option
case {'extend','truncate'}
   if isnan(param.null)
      traces=NaN(nsamp+shmax-shmin,ntr,param.precision);
   else
      traces=repmat(param.null,nsamp+shmax-shmin,ntr);
   end

   for ii=1:ntr
      ish=shifts(ii)-shmin;
      if ~isnan(ish) && ~isempty(ish)
         traces(ish+1:nsamp+ish,ii)=seismic.traces(:,ii);
      end         
   end

case 'circular'
   for ii=1:ntr
      seismic.traces(:,ii)=circshift(seismic.traces(:,ii),double(shift));
   end

otherwise
   error(['Unknown parameter for keyword "option": ',param.option])

end

seismic.first=first+shmin*seismic.step;
seismic.last=last+shmax*seismic.step;
seismic.traces=traces;

switch param.option
case 'extend'
   if any(isnan(traces(:)))
      seismic.null=NaN;
   else
      seismic.null=[];
   end

case 'truncate'
   seismic=s_select(seismic,{'times',first,last});
   if any(isnan(seismic.traces(:)))
      seismic.null=NaN;
   else
      seismic.null=[];
   end

otherwise
   %  do nothing

end
