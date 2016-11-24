function dataset=ds_add_header(dataset,values,info,replace)
% ADD_HEADER adds one or more headers to a seismic or a pseudo-well dataset.
% See also: ds_header
%
% Written by: E. Rietsch: April 28, 2006
% Last updated: February 2, 2008: bug fix
%
%          dataset=add_header(dataset,values,info,replace)
% INPUT
% dataset  seismic or pseudo-well dataset
% values   row vector or matrix of header values One row per header.
%          There must be as many columns as there are traces in the seismic
%          dataset or pseudo-wells in the pseudo-well dataset.
%          However, if all the header values are constant, only this constant
%          need to be specified (see example below).
% info     three-column cell array; one row per header
%          The first column contains the header mnemonic, the second the
%          units of measurement, the third a description of the header.
% replace  logical variable; if true an existing header with the same mnemonic
%          will be replaced; otherwise, if a header with the same name already
%          exists, an error will be thrown.        
%          Default: replace=true
%
% EXAMPLE
%          seismic=s_data;
%          seismic=add_header(seismic,330,{'offset','ft','Source-receiver offset'});
%          ds_header(seismic)


global S4M

if istype(dataset,'seismic')
   ntr=size(dataset.traces,2);
elseif istype(dataset,'pseudo-wells')
   ntr=panelsize(dataset,2);
else
   error('The first input argument must be a seismic dataset or a pseudo-well dataset.')
end

%     Check input arguments
[nh,ntr1]=size(values);
if size(info,1) ~= nh
   error(' Input arguments "values" and "info" have different number of rows.')
end
if size(info,2) ~= 3
   error('Cell array with header info must have three columns.')
end
if ntr1 == 1
   values=values(:,ones(ntr,1));
elseif ntr1 ~= ntr
   disp({'The number columns of input argument "values" must be either equal to 1'; ...
        ['or must be equal to ',num2str(ntr)]})
   error('Abnormal termination')
end

if isfield(dataset,'header_info')
   existing_headers=dataset.header_info(:,1);
   new_headers=info(:,1);
   if S4M.case_sensitive
      [dummy,idx1,idx2]=intersect(existing_headers,new_headers);   %#ok  First output argument not required
   else
      [dummy,idx1,idx2]=intersect(lower(existing_headers),lower(new_headers)); %#ok  First output argument not required
   end

   if isempty(idx1)
      dataset.header_info=[dataset.header_info;info];
      dataset.headers=[dataset.headers;values];
      
   elseif nargin > 3  &&  ~replace
      disp({['Over-writing of header values is not permissible; yet ', ...
             'the following new headers']; ...
            ['are already present in dataset "',dataset.name,'".']})
      error('Abnormal termination.')

   else
      dataset.header_info(idx1,:)=info(idx2,:);
      dataset.headers(idx1,:)=values(idx2,:);
      idx=find(~ismember(1:nh,idx2));
      if ~isempty(idx)
         dataset.header_info=[dataset.header_info;info(idx,:)];
         dataset.headers=[dataset.headers;values(idx,:)];
      end
   end

else
   dataset.header_info=info;
   dataset.headers=values;
end
