function seisout=s_append(seis1,seis2,varargin)
% Function combines two seismic data sets. On output the traces of the 
% first input data set are followed by the traces of the second data set.
% The two data sets must satisfy the following two conditions:
%           seis1.step=seis2.step;
%           (seis1.first-seis2.first)/seis1.step is integer
% Unless keyword 'times' is {'min','max'} and keyword 'headers' is 'i' (intersect)
% some seismic samples and/or header values of "seisout" may not be defined by the 
% input data.
% The precision of the output dataset is single-precision if at least one
% of the input datasets is single-precision.
%
% See also:   s_select
%
% Written by: E. Rietsch: January 28, 2000
% Last updated: January 1, 2007: Ascertain that the precision of the output
%                                dataset is single-precision if one of the 
%                                input datasets is single-precision
%
%         seisout=s_append(seis1,seis2,varargin)
%
% INPUT 
% seis1   seismic structure or empty; if it is empty the second input 
%         argument (second seismic dataset) is output (any other arguments 
%         are ignored). This is intended to make it more convenient to 
%         build a multi-trace dataset in a loop.
% seis2   seismic structure; can also be empty.
% varargin    one or more cell arrays; the first element of each cell array is a 
%             keyword, the other elements are parameters. Presently, keywords are:
%    'header_null' Null value to use for non-existing header values in the output data set
%    'null'    Null value to use for non-existing trace samples in the output data set
%              Only used if there are no null values in the input data
%    'times'   This two-cell string defines what should be done if the two data sets have 
%         differing first-sample and/or last-sample times
%         times{1} = 'min': seisout.start=min([seis1.first,seis2.first]);
%         times{1} = 'max': seisout.start=max([seis1.first,seis2.first]);
%         times{2} = 'min': seisout.start=min([seis1.last,seis2.last]);
%         times{2} = 'max': seisout.start=max([seis1.last,seis2.last]);
%         Default: {'times','min','max'}
%    'headers'   This string parameter defines what should be done if the two data sets have
%         different headers
%         {'headers','u'} : Use the union of the headers (default)
%         {'headers','i'} ; Use the intersection of the headers (the headers they have in common)
%
% OUTPUT
% seisout  Combination of the two datasets

if ~istype(seis1,'seismic')
   if isempty(seis1)
      seisout=seis2;
      return
   else
      error('First input argument must be a seismic dataset or empty.')
   end
end

if ~istype(seis2,'seismic')
   error('Second input argument must be a seismic dataset.')
end

%       Set defaults parameters
param.header_null=NaN;
param.null=NaN;
param.times={'min','max'};
param.headers='u';

%       Decode and assign input arguments
param=assign_input(param,varargin);

if isnull(seis1)  || isnull(seis2)
   param.null=NaN;
end

ttype=param.times;
htype=param.headers;

%       Check compatibility of data sets
if ~strcmpi(seis1.units,seis2.units)
   error(['Input data sets have different depth/time/frequncy units ("', ...
           seis1.units,'" vs. "',seis2.units,'")'])
end

if seis1.step ~= seis2.step
  error([' Input data sets have different sample intervals: ', ...
     num2str(seis1.step),' ',seis1.units,' vs. ', num2str(seis2.step), ...
     ' ',seis2.units])
end

seisout.type=seis1.type;
seisout.tag=seis1.tag;
seisout.name='';
step=seis1.step;
seisout.step=step;
seisout.units=seis1.units;
                   
null_value=param.null;
header_null_value=param.header_null;

fdiff=(seis1.first-seis2.first)/seis1.step;
ldiff=(seis1.last-seis2.last)/seis1.step;
[nsamp1,ntr1]=size(seis1.traces);
[nsamp2,ntr2]=size(seis2.traces);


if fdiff == 0 && ldiff == 0       % Simple case where both data sets have the same start and end times
   seisout.traces=[seis1.traces,seis2.traces];
   seisout.first=seis1.first;
   seisout.last=seis1.last;

else
   if strcmpi(ttype(1),'max')
      first=max([seis1.first,seis2.first]);
   elseif strcmpi(ttype(1),'min')
      first=min([seis1.first,seis2.first]);
   else
      error(['Unknown input parameter ttype(1) (',ttype(1),')']) 
   end

   if strcmpi(ttype(2),'max')
      last=max([seis1.last,seis2.last]);
   elseif strcmpi(ttype(2),'min')
      last=min([seis1.last,seis2.last]);
   else
     error(['Unknown input parameter ttype(1) (',ttype(1),')']) 
   end

   seisout.first=first;
   seisout.last=last;
   nsamp=round((last-first)/step)+1;

   seisout.traces=null_value*zeros(nsamp,ntr1+ntr2);

   ia=round((first-seis1.first)/step);
   if ia >= 0
      seisout.traces(1:min([nsamp1-ia,nsamp]),1:ntr1)=seis1.traces(ia+1:min([nsamp1,nsamp+ia]),:);
   else
      seisout.traces(1-ia:min([nsamp1-ia,nsamp]),1:ntr1)=seis1.traces(1:min([nsamp1,nsamp+ia]),:);  
   end

   ia=round((first-seis2.first)/step);
   if ia >= 0
      seisout.traces(1:min([nsamp2-ia,nsamp]),ntr1+1:ntr1+ntr2)= ...
               seis2.traces(ia+1:min([nsamp2,nsamp+ia]),:);
   else
      seisout.traces(1-ia:min([nsamp2-ia,nsamp]),ntr1+1:ntr1+ntr2)= ...
               seis2.traces(1:min([nsamp2,nsamp+ia]),:);  
   end
end
        
%      Set headers of output data set
if isfield(seis1,'headers') && isfield(seis2,'headers')   % Both data sets have headers
   seisout=merge_headers(seisout,seis1,seis2,htype,header_null_value);

elseif isfield(seis1,'headers')     % Only the first data set has headers
   seisout.header_info=seis1.header_info;
   if header_null_value == 0
      seisout.headers=[seis1.headers,zeros(size(seis1.headers,1),ntr2)];
   else
      seisout.headers=[seis1.headers,header_null_value*zeros(size(seis1.headers,1),ntr2)];
      if isnan(header_null_value)
         seisout.header_null=NaN;
     end
   end

elseif isfield(seis2,'headers')     % Only the second data set has headers
   seisout.header_info=seis2.header_info;
   if header_null_value == 0
      seisout.headers=[zeros(size(seis2.headers,1),ntr1),seis2.headers];
   else
      seisout.headers=[header_null_value*zeros(size(seis2.headers,1),ntr1),seis2.headers];
      if isnan(header_null_value)
         seisout.header_null=NaN;
      end
   end

else                                % Neither input data set has headers
   % Do nothing
end   
 
%     Copy common fields that have not yet been set and that have the same value 
%     in both input data sets (string or number, no arrays)
set_fields={'traces','headers','header_info','first','last','step', ...
            'units','history'};
fields1=fieldnames(seis1);
fields2=fieldnames(seis2);
fields=intersect(fields1,fields2);
fields=fields(~ismember(fields,set_fields));
for ii=1:length(fields);
   fval1=seis1.(fields{ii});
   fval2=seis2.(fields{ii});

%   fval1=getfield(seis1,fields{ii});
%   fval2=getfield(seis2,fields{ii});
        try
   if strcmpi(fval1,fval2)
%      seisout=setfield(seisout,fields{ii},fval1);
      seisout.(fields{ii})=fval1;
   end
        catch
        end

        try
   if fval1 == fval2
      seisout.(fields{ii})=fval1;
%      seisout=setfield(seisout,fields{ii},fval1);
   end
        catch
        end
end

%{
%	Set null field, if necessary
% if isfield(seis1,'null') && isfield(seis2,'null')
   if ~isnull(seis1) && ~isnull(seis2)
      if seis1.null == seis2.null
         param.null=seis1.null;
      end
   end
% end
if any(isnan(seisout.traces(:)))
   if ~isfield(seisout,'null')
      seisout.null=param.null;
   end
else
   if isfield(seisout,'null')
     seisout=rmfield(seisout,'null');
   end
end
%}

if any(isnan(seisout.traces(:)))
   seisout.null=NaN;
else
   seisout.null=[];
end

%    Append history field
if isfield(seis1,'history') && isfield(seis2,'history')
   seisout.history=seis1.history;
   seisout=s_history(seisout,'append',['[',ttype{1},' ',ttype{2},'], ',htype]);
   seisout=s_history(seisout,'merge',seis2.history);
end

if strcmp(class(seis1.traces),'single')  ||  strcmp(class(seis2.traces),'single')
   seisout=single(seisout);
end


%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%

function seisout=merge_headers(seisout,seis1,seis2,htype,header_null_value)
% Function merges headers of the two seismic input data sets
% INPUT
% seisout seismic output data set with fields that have already been set
% seis1, seis2    seismic input data sets
% htype   This string parameter defines what should be done if the two data sets have
%         different headers
%         htype='u': Use the union of the headers (default)
%         htype='i'; Use the intersection of the headers (the headers they have in common)
% header_null_value   Null value for missing header values
% OUTPUT
% seisout seismic output data set
%     seisout=merge_headers(seis1,seis2,htype)

ntr1=size(seis1.headers,2);
ntr2=size(seis2.headers,2);

headers1=seis1.header_info(:,1);
headers2=seis2.header_info(:,1);
iheaders=intersect(headers1,headers2);
hidx1=find(ismember(headers1,iheaders));
hi1=seis1.header_info(hidx1,:);   
hidx2=find(ismember(headers2,iheaders));
hi2=seis2.header_info(hidx2,:);

%        Check that units of measurement are the same
lh1=length({hi1{:,2}});
if lh1 ~= sum(ismember({hi1{:,2}}',{hi2{:,2}}))
   spaces=blanks(lh1)';
   commas=char(44*ones(lh1,1));
   units_of_measurement=[char({hi1{:,1}}),spaces,char({hi1{:,2}}),commas, ...
                        spaces,char({hi2{:,2}}),spaces,char({hi2{:,1}})];
   disp(units_of_measurement)
   error('Units of measurement in common headers of the two input data sets differ')
end
seisout.header_info=hi1;
seisout.headers=[seis1.headers(hidx1,:),seis2.headers(hidx2,:)];

if strcmpi(htype,'u')
   if isempty(iheaders)
      idx1=1:length(headers1(:,1));
      idx2=1:length(headers2(:,1));
   else
      [dummy,idx1]=setdiff(headers1(:,1),iheaders(:,1));  %#ok  Only second output argument required
      [dummy,idx2]=setdiff(headers2(:,1),iheaders(:,1));  %#ok  Only second output argument required
   end
   if ~isempty(idx1) || ~isempty(idx2)
      seisout.header_null=header_null_value;
   end
   seisout.header_info=[seisout.header_info; ...
      seis1.header_info(idx1,:);seis2.header_info(idx2,:)];
   seisout.headers=[seisout.headers;
            seis1.headers(idx1,:),header_null_value*zeros(length(idx1),ntr2);
            header_null_value*zeros(length(idx2),ntr1),seis2.headers(idx2,:)];
elseif ~strcmpi(htype,'i')
   error(['Unknown parameter htype (',htype,')'])
end
