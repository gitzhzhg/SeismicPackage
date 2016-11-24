function index=ds_header_sort(dataset,varargin)
% Function sorts the headers of the dataset structure "dataset" and creates an index 
% vector. Intended for used with seismic data and pseudo-wells.
% For seismic data this index vector can be used in "s_select" to sort the traces
% For pseudo-wells this index vector can be used in "pw_select" to sort the pseudo-wells
%
% Written by: E. Rietsch: July 8, 2001
% Last updated: January 28, 2008: bug fix
%
%            index=ds_header_sort(dataset,varargin)
% INPUT
% dataset    dataset structure
% varargin   one or more cell arrays; the first element of each cell array is a keyword,
%            the other elements are parameters. Presently, keywords are:
%            'headers'  List of headers to be sorted
%                       General form: {'headers',{header1,sort_dir1},{header2,sort_dir2}, ...
%                       {header3,sort_dir3}}
%                       First the traces are are sorted by trace header "header1", then by
%                       trace header "header2", ...
%                       "sort_dir1", "sort_dir2", ... denote sort directions. Possible
%                       values are 'increasing' or 'decreasing.  Default is 'increasing'.
%                       No default.
%            'sortdir'  Default sort direction ('increasing' or 'decreasing'); 
%                       used if sort direction is not given explicitly with each header
%                       Default: 'increasing'
%                       This value is overruled by any explicitly given sort direction:
%                       ds_header_sort(seismic,{'headers',{'cdp','decreasing'}}) is the same as
%                       ds_header_sort(seismic,{'headers','cdp'},'{'sortdir','decreasing'})
%                       Default: {'sortdir','increasing'}
% OUTPUT
% index      column vector with trace indices
%
% EXAMPLES
%            seismic=s_data;
%            %      Select the five traces with the highest CDP numbers in decreasing order
%            index=ds_header_sort(seismic,{'headers',{'cdp','decreasing'}})
%            seismic1=s_select(seismic,{'traces',index(1:5)})
%
%            seismic3=s_data3d;
%            %      Select the five traces closest to the well location (1010,2606)
%            seismic3=s_header_math(seismic3,'add','offset=sqrt((iline_no-1010)^2+(xline_no-2606)^2)', ...
%                     'n/a','Distance to well location');
%            index3a=ds_header_sort(seismic3,{'headers',{'offset','increasing'}});
%            seismic3a=s_select(seismic3,{'traces',index3a(1:5)});
%            ds_header(seismic3a)
%
%            %          Sort the traces (increasing inline, decreasing cross-line)
%            index3b=ds_header_sort(seismic3,{'headers',{'xline_no','increasing'}, ...
%                    {'iline_no','decreasing'}});
%            seismic3b=s_select(seismic3,{'traces',index3b});
%            disp('    iline_no    xline_no')
%            disp([seismic3b.headers(1,1:25)',seismic3b.headers(2,1:25)'])
%
%            %          Sort the traces (decreasing both inline and cross-line)
%            index3c=ds_header_sort(seismic3,{'headers',{'xline_no'},{'iline_no'}}, ...
%                    {'sortdir','decreasing'});
%            seismic3c=s_select(seismic3,{'traces',index3c});
%            disp('    iline_no    xline_no')
%            disp([seismic3c.headers(1,1:25)',seismic3c.headers(2,1:25)'])

% UPDATE HISTORY
%            January 2, 2001; compact/new header structure; use of "sortrows"
%            December 10, 2007: Generalize for use with pseudo-wells; bug fix

%       Check input dataset
switch dataset.type
case 'seismic'
   ntr=size(dataset.traces,2);
case 'pseudo-wells'
   ntr=panelsize(dataset,2);
otherwise
   error(' First input argument must be a seismic or a pseudo-well dataset.')
end

%     	Set default values for input arguments
param.headers=[];
param.sortdir='increasing';

%       Replace defaults by actual input arguments
param=assign_input(param,varargin);

%       Check input arguments
if isempty(param.headers)
   error(' At lest one header for sorting must be specified.')
end

%       If there is only one header specified param.headers{1} is a string;
if ~iscell(param.headers{1})
   param.headers={param.headers};  
end

nk=length(param.headers);

%       Reserve room for arrays
headers=zeros(ntr,nk);

% 	Get header values to be sorted
for ii=1:nk
   if iscell(param.headers{ii}) && length(param.headers{ii}) == 2
      if strcmpi(param.headers{ii}(2),'increasing')
         headers(:,ii)=ds_gh(dataset,param.headers{ii}(1))';
      else
         headers(:,ii)=-ds_gh(dataset,param.headers{ii}(1))';
      end

   else
      if strcmpi(param.sortdir,'increasing')
         headers(:,ii)=ds_gh(dataset,param.headers{ii})';
      else
         headers(:,ii)=-ds_gh(dataset,param.headers{ii})';
      end  
   end
end

%   Sort headers
[dummy,index]=sortrows(headers); %#ok The first output argument is not required
index=index';
