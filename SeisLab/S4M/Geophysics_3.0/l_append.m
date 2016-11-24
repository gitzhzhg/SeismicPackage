function wlog=l_append(wlog1,wlog2,varargin)
% Function appends one log structure to another. 
% The following cases are handled (or planned).
%    1. Both logs have the same depth values: 
%       curves on the second log which are identical to curves on the first 
%       log are not copied.
%       Only those parameters of the second log with names different from those 
%       in the first log are copied to the output log.  
%    2. If this is not the case both logs are uniformly resampled. Unless
%       given explicitly, the sample interval is taken from the first log. 
%       If the first log is not sampled uniformly, it is taken from the second
%       log. If the second log is not sampled uniformly either, an error 
%       message is issued.
%       The string 'no_1' is appended to mnemonics of curves of the second log
%       structure which equal those of the first log structure. The same is
%       true for parameters. All other fields (such as wall name, country) 
%       are taken from the first log. However, fields of the second log that
%       do not exist in the first log are also copied to the output log.
%       
% Written by: E. Rietsch: December 26, 2000
% Last updated: August 20, 2007: set precision of output data set
%
%         wlog=l_append(wlog1,wlog2,varargin)
% INPUT
% wlog1    first input log 
% wlog2    log to be appended
% varargin  one or more cell arrays; the first element of each cell array is a
%          keyword, the other elements are parameters. Presently, keywords are:
%    'step'  step size of output log.
%          Default: {'step',wlog1.step}  
%                   i.e. depth step of first log (must not be 0)
%    'units' depth units. 
%          Default: {'units',wlog1.curve_info{1,2}} 
%                    i.e. units of first input log
%     
% OUTPUT 
% wlog     combination of the two input logs; precision is the same as the 
%          precision of the first input data set

global S4M

[nsamp1,ncurves1]=size(wlog1.curves);
[nsamp2,ncurves2]=size(wlog2.curves);

wlog.type='well_log';
wlog.tag='unspecified';
wlog.name=wlog1.name;

% 	First handle the case where the depth values and units of measurement of the two logs are identical

if nsamp1 == nsamp2 && ...
   strcmp(wlog1.curve_info{1,2},wlog1.curve_info{1,2}) &&  ...
   max(abs(wlog1.curves(:,1)-wlog2.curves(:,2))) < 1.0e-5*(wlog1.last-wlog1.first)/nsamp1;        

   index=curve_indices(wlog2,wlog1.curve_info(2:end,1));
   if isempty(index)
      wlog.curves=[wlog1.curves,wlog2.curves];
      wlog.curve_info=[wlog1.curve_info;wlog2.curve_info];

   else
      nindex=find(~ismember(1:ncurves2,index));
      nindex=nindex(2:end);     
      wlog.curves=[wlog1.curves,wlog2.curves(:,nindex)];
      wlog.curve_info=[wlog1.curve_info;wlog2.curve_info(nindex,:)];
      for ii=index;
         mnem=wlog2.curve_info{ii,1};
         curve1=l_gc(wlog1,mnem);
         curve2=l_gc(wlog2,mnem);
         if max(abs(curve1-curve2)) <= 1.0e-3*max(curve1)
            alert([' Alert from "l_append": curve "',mnem,'" of second data set disregarded'])
            alert(['                        (same as curve "',mnem,'" of first data set'])
         else
            ds_header(wlog,'add',[mnem,'_no_1'],curve2,wlog2.curve_info{ii,2},wlog2.curve_info{ii,3})
         end
      end
   end

%       Copy rest of fields
   wlog=copy_fields(wlog1,wlog);
   wlog=copy_fields(wlog2,wlog);
   return
end

% 	If depth values are different resample at least second data set

%     Set default values
append.step=wlog1.step;
append.units=wlog1.curve_info{1,2};

%       Decode and assign input arguments
append=assign_input(append,varargin);
if append.step == 0
   if wlog2.step == 0
      error(' Sample interval "step" must not be zero')
   end
   append.step=wlog2.step;
end

%       Check depth units
if ~strcmp(append.units,'m') && ~strcmp(append.units,'ft') && ~strcmp(append.units,'ms')
   error([' Unknown depth units: ',append.units])
end

%       Convert depth if necessary
if ~strcmp(append.units,wlog1.curve_info{1,2})
   if strcmp(wlog1.curve_info{1,2},'m')
      wlog1.curves(:,1)=wlog1.curves(:,1)/0.3048;
      wlog1.first=wlog1.first/0.3048;
      wlog1.last=wlog1.last/0.3048;
      wlog1.step=wlog1.step/0.3048;
   elseif strcmp(wlog1.curve_info{1,2},'ft')
      wlog1.curves(:,1)=wlog1.curves(:,1)*0.3048;
      wlog1.first=wlog1.first*0.3048;
      wlog1.last=wlog1.last*0.3048;
      wlog1.step=wlog1.step*0.3048;
   else
      error([' First input log has unknown depth units: ',wlog1.curve_info{1,2}])
   end
end

if ~strcmp(append.units,wlog2.curve_info{1,2})
   if strcmp(wlog2.curve_info{1,2},'m')
      wlog2.curves(:,1)=wlog2.curves(:,1)/0.3048;
      wlog2.first=wlog2.first/0.3048;
      wlog2.last=wlog2.last/0.3048;
      wlog2.step=wlog2.step/0.3048;
   elseif strcmp(wlog2.curve_info{1,2},'ft')
      wlog2.curves(:,1)=wlog2.curves(:,1)*0.3048;
      wlog2.first=wlog2.first*0.3048;
      wlog2.last=wlog2.last*0.3048;
      wlog2.step=wlog2.step*0.3048;
   else
      error([' Second input log has unknown depth units: ',wlog2.curve_info{1,2}])
   end
end

% 	Define new start and end depth
first=min([wlog1.first,wlog2.first]);
last=max([wlog1.last,wlog2.last]);
wlog.first=0.5*ceil(2*first/append.step)*append.step;
wlog.last=0.5*floor(2*last/append.step)*append.step;
wlog.step=append.step;
nsamp=round((wlog.last-wlog.first)/wlog.step)+1;
wlog.curves=zeros(nsamp,ncurves1+ncurves2-1);
depth=(0:nsamp-1)'*(wlog.last-wlog.first)/(nsamp-1)+wlog.first;

if S4M.matlab_version >= 7 
   warning('off','MATLAB:interp1:NaNinY')
end
if wlog1.step > 0
   wlog.curves(:,2:ncurves1)=interp1(wlog1.curves(:,1),wlog1.curves(:,2:ncurves1),depth,'*linear');
else
   wlog.curves(:,2:ncurves1)=interp1q(wlog1.curves(:,1),wlog1.curves(:,2:ncurves1),depth);
end
if S4M.matlab_version >= 7 
   warning('on','MATLAB:interp1:NaNinY')
end

if wlog2.step > 0
   wlog.curves(:,ncurves1+1:ncurves1+ncurves2-1) = ...
                       interp1(wlog2.curves(:,1),wlog2.curves(:,2:ncurves2),depth,'*linear');
else
   wlog.curves(:,ncurves1+1:ncurves1+ncurves2-1) = ...
                       interp1q(wlog2.curves(:,1),wlog2.curves(:,2:ncurves2),depth);
end
wlog.curves(:,1)=depth;

%       Handle curve information field
curve_info=wlog2.curve_info(2:end,:);

if S4M.case_sensitive
   idx=find(ismember(curve_info(:,1),wlog1.curve_info(:,1)));
else
   idx=find(ismember(lower(curve_info(:,1)),lower(wlog1.curve_info(:,1))));
end

if ~isempty(idx)
   for ii=idx(:)'
     curve_info{ii,1}=[curve_info{ii,1},'_no_1'];
   end
end

%       Combine 'curve_info'
wlog.curve_info=[wlog1.curve_info;curve_info];
wlog.curve_info{1,2}=append.units;


%       Handle 'curve_types'
if isfield(wlog1,'curve_types')
   if isfield(wlog2,'curve_types')
      curve_types=wlog2.curve_types;
      if S4M.case_sensitive
         idx=find(ismember(curve_types(:,1),wlog1.curve_types(:,1)));
      else
         idx=find(ismember(lower(curve_types(:,1)),lower(wlog1.curve_types(:,1))));
      end

      if ~isempty(idx)
         curve_types(idx,:)=[];
      end

%               Combine 'curve_types'
      wlog.curve_types=[wlog1.curve_types;curve_types];
      wlog.curve_info{1,2}=append.units;

   else
      wlog.curve_types=wlog1.curve_types;
   end

elseif  isfield(wlog2,'curve_types')
   wlog.curve_types=wlog2.curve_types;
end


%       Handle parameters
wlog=combine_parameters(wlog1,wlog2,wlog);

%       Copy rest of fields
wlog=copy_fields(wlog1,wlog);
wlog=copy_fields(wlog2,wlog);

%       Check for NaNs in the combined structure
if ~isfield(wlog,'null') || ~isempty(wlog.null)
% index=find(isnan(wlog.curves(:,2:end)));
%  if ~isempty(index)
  if any(any(isnan(wlog.curves(:,2:end))))
     wlog.null=NaN;
  else
     wlog.null=[];
  end
end  

wlog=l_rm_nulls(wlog,'all');

%       Precision of output should be the same as precision of first input data set
if strcmp(class(wlog1.curves),'single')
   wlog=single(wlog);
else
   wlog=double(wlog);
end
