function [seismic,ebcdic_header,binary_header]=read_segy_file_legacy(filename,varargin)
% Function reads a SEG-Y file and outputs a seismic structure
%
% Written by: E. Rietsch: March 5, 2000
% Last updated: April 6, 2007: fix bug in reading of user-specified headers
%
%             [seismic,ebcdic_header,binary_header]=read_segy_file_legacy(filename,varargin)
%
% INPUT       (all input arguments are optional)
% filename    name of the  file to read
%             if the name is omitted, empty, or a file with this name is not found
%             a file selector box will pop up to allow interactive file selection
% varargin    Variable number of arguments. Each argument is a cell array whose first
%             element is a keyword and whose other elements can be strings, numeric
%             values, or other cell arrays
%             Possible keywords are:
%     format  floating point format to use; this cell array has the form
%             {'format',fp-format} where fp_format is one of the three strings:
%             'ibm'      IBM floating point format (standard and default)
%             'ieee'     IEEE format, big endian  (Sun, SGI, etc.)
%             'header'   The binary header bytes 25-26 are used to determine the format
%             Default: {'format','header'}; 
%     headers  header values to be read from binary trace header; this cell array has
%             the form {'headers',{mnem1,first,bytes,units,description}, ...
%             {mnem2,first,bytes,units,description},...} where
%             "mnem1", "mnem2", ... denote header mnemonics (such as CDP, OFFSET), 
%             "first" denotes the first byte in the binary header, 
%             "bytes" denotes the number of bytes occupied by the mnemonic (2 or 4), 
%             "units" denotes the units of measurement for the header value, and 
%             "description" is a description of the header value. 
%             Example:
%             {'headers',{'ILINE_NO',181,4,'n/a','CDP number'}, ...
%                        {'OFFSET',37,4,'m','Source-receiver distance'}}
%             See below for a list of headers retrieved by default.
%             Default: {'headers',{}}
%     times   times to output; this cell array has the form
%             {'times',first,last}; all samples with times between (and including)
%             first and last (in ms) are output.
%             Default: {'times',[]}  i.e. all times
%     traces  select traces to output; this cell array has the form
%             {'traces',criterion}; the variable "criterion" is used to 
%             determine which traces to output. "criterion" can be an index
%             vector specifying the traces to output
%             Examples: {'traces',1:2:100}
%                       {'traces',[2,5,7:10,22]}
%             Alternatively, it can be a string with a logical expression involving 
%             trace headers such as '10 <= cdp && 100 >= cdp'
%             Examples: {'traces','cdp == 100 && offset > 100'}
%                       {'traces','14000 < cdp  && (14660 >= cdp || 14680 <= cdp)'};
%             The variables in the logical relationships must be headers of the 
%             data set; Use of functions "fix", "mod", and "round" are permitted; all
%             other function names will be interpreted as headers and likely cause an
%             error; the case of the headers in an expression does not matter.
%             Default: {'traces',[]}
%    ignoreshift  By default this function reads byte locations 109-110 (see 
%             header "lag" below) and applies the shift to the seismic data;
%             This behavior can be overwritten by setting this parameter to true;
%             Default: {'ignoreshift',false}
%
% Headers retrieved by default are (any one of these headers is removed if it turns out to 
% be identically zero):
%        ds_seqno      Trace sequence number within line (1-4)
%        ffid          Original Field record number (9-12)
%        o_trace_no    Trace sequence number within original field record (13-16)
%        source        Energy source point number (17-20)
%        cdp           CDP ensemble number (21-24)
%        seq_cdp       Trace sequence number within CDP ensemble (25-28)
%        trc_type      Trace ID (1=live,2=dead,3=dummy,4=time break,...) (29-30)
%        offset        Distance from source point to receiver group (37-40)
%        rec_elev      Receiver elevation (41-44);
%        sou_elev      Surface elevation at source (45-48)
%        depth         Source depth below surface (49-52)
%        sou_h2od      Water depth at source (61-64)
%        rec_h2od      Water depth at receiver group (65-68)
%        sou_x         X coordinate of source (73-76)
%        sou_y         Y coordinate of source (77-80)
%        rec_x         X coordinate of receiver (81-84)
%        rec_y         Y coordinate of receiver (85-88)
%        lag           Lag time between shot and recording start in ms (109-110)
%                      (the value of lag is added to the start time of the 
%                      seismic; hence it can be used to simulate non-zero start
%                      time of the data)
%                      see also parameter "ignoreshift", above.
%        cdp_x         X coordinate of CDP (181-184)
%        cdp_y         Y coordinate of CDP (185-189)
%        iline_no      In-line number (189-192)
%        xline_no      Cross-line number (193-196)
%               The numbers in parentheses at the end of the line denote the location 
%               of the corresponding bytes in the SEG-Y trace header
%    
% OUTPUT
% seismic       Seismic structure
%       seismic.type               'seismic' (type of structure)
%       seismic.traces             Array of seismic traces
%       seismic.first              Start time of seismic (in ms)
%       seismic.last               End time of seismic (in ms)
%       seismic.step               Sample interval of seismic (in ms)
%       seismic.units              Time units used (ms)
%       seismic headers            Matrix with header mnemonics (one row 
%                                  per header)
%       seismic.header_info        Three-column cell array with header info 
%                                  (one row per header)
%       seismic.line_number        Line number (5-8)
%       seismic.reel_number        Reel number (9-12)
%       seismic.cdp_fold           CDP fold
%       seismic.traces_per_record  Data traces per record (13-14)
%       seismic.aux_per_record     Auxiliary traces per record (15-16)
%       seismic.history            A four element cell array. The first element
%                                  is the start date/time of the program that 
%                                  invoked this function; the second element is
%                                  the start date/time this function was executed;
%                                  and the last cell contains the name if the file 
%                                  that was read
%            Example:
%               seismic.offset     contains the offset for each trace
%               seismic.header_info.cdp two-element cell array {'m','Offset'}
%                                  the first element represents the units of measurement, the 
%                                  second is a description of the header
%                 
% ebcdic_header         EBCDIC reel header converted to ASCII
% binary_header         Binary reel header
%              
% 	[seismic,ebcdic_header,binary_header]=read_segy_file(filename,varargin)

global S4M ABORTED

ABORTED=false;
seismic=[];
ebcdic_header=[];
binary_header=[];

run_presets_if_needed

%	Set default parameters
param.format='header';
param.headers={};
param.ignoreshift=false;
param.times=[];
param.traces=[];

if ~isempty(varargin)
   param=assign_input(param,varargin);   % Interpret/check input data
end

if strcmpi(param.format,'ieee')
   param.format='ieee-be';
end
o_format='ieee-be';

if nargin < 1
   filename='';
end
if nargout < 2
   parameters=initiate_segy_file_reading_no1(filename,param,o_format);
elseif nargout == 2
   [parameters,ebcdic_header]=initiate_segy_file_reading_no1(filename,param,o_format);
else
   [parameters,ebcdic_header,binary_header]=initiate_segy_file_reading_no1(filename,param,o_format);
end

if ABORTED
   return
end

param.format=parameters.format;
fid=parameters.fid;
seismic=parameters.seismic;
indices2=parameters.indices2;
indices4=parameters.indices4;
idx_time=parameters.idx_time;
filename=parameters.filename;
no_samples=parameters.no_samples;
n_default_headers=parameters.n_default_headers;
nindices2=parameters.nindices2;
nindices4=parameters.nindices4;

[header,nh_select,max_traces,idx_trace,h_index,trace_select,param] = ...
        select_traces(seismic,fid,param); %#ok h_index used in "eval"

ier=0;

%       Reserve room for header and seismic
if max_traces <= 1000
  grab=max_traces;
else
  grab=1000;
  grab_inc=grab;
end
txt=['Make room for ',num2str(grab),' traces'];
ltext=length(txt);
%fprintf('\n')

headers=zeros(size(seismic.header_info,1),grab);
seismic.traces=zeros(length(idx_time),grab);
ntraces=0;
ntraces_read=0;

%       Read seismic trace header
four_bytes=zeros(60,1);
two_bytes=zeros(90,1);

test=fread(fid,1,'int32');

				try  % Catch errors due to premature end of SEG-Y file

		while ~isempty(test) && ntraces < max_traces   % read trace
ntraces_read=ntraces_read+1;
% trace_no=ntraces_read;          % For use in trace selection

if ntraces >= grab
   grab=grab+grab_inc;
   headers=[headers,zeros(size(seismic.header_info,1),grab_inc)];      %#ok
   seismic.traces=[seismic.traces,zeros(length(idx_time),grab_inc)];   %#ok
   if S4M.alert
      txt=['Make room for ',num2str(grab),' traces'];
      if grab == 2*grab_inc
         fprintf(txt)
      else
         fprintf([char(8*ones(1,ltext)),txt])
      end
      ltext=length(txt);
   end
end 
four_bytes(1)=test;
four_bytes(2:7)=fread(fid,6,'int32');
two_bytes(15:18)=fread(fid,4,'int16');
four_bytes(10:17)=fread(fid,8,'int32');
two_bytes(35:36)=fread(fid,2,'int16');
four_bytes(19:22)=fread(fid,4,'int32');
two_bytes(45:90)=fread(fid,46,'int16');
four_bytes(46:60)=fread(fid,15,'int32');

depth_scalar=two_bytes(35);
if depth_scalar > 0
  four_bytes(11:17)=four_bytes(11:17)*depth_scalar;
elseif depth_scalar < 0
  four_bytes(11:17)=four_bytes(11:17)/abs(depth_scalar);
else
  if ier < 2, disp('Alert from "read_segy_file": Scalar for elevations and depths (bytes 69-70) is zero'); end
  ier=ier+1;
end

coord_scalar=two_bytes(36);
if coord_scalar > 0
   four_bytes(19:22)=four_bytes(19:22)*coord_scalar;
elseif coord_scalar < 0
   four_bytes(19:22)=four_bytes(19:22)/abs(coord_scalar);
else
   if ier < 2, disp('Alert from "read_segy_file": Scalar for coordinates (bytes 71-72) is zero'); end
   ier=ier+1;
end

headers(:,ntraces+1)=[four_bytes(indices4(1:nindices4)); ...
                               two_bytes(indices2(1:nindices2)); ...
                              four_bytes(indices4(nindices4+1:end)); ...
                               two_bytes(indices2(nindices2+1:end))];

if trace_select == 0
   boolean=ntraces < max_traces;
elseif trace_select == 1 
   for jj=1:nh_select
      eval([header{jj},'=headers(h_index(jj),ntraces+1);']);
   end
                         try
   boolean=ntraces < max_traces && eval(param.traces);
                        catch
   error([' The input argument  ',param.traces, ...
         '  of read_segy_file is probably not a valid logical expression.'])
                         end
else
                        try
   boolean=ntraces < max_traces && sum(ismember(idx_trace,ntraces_read));
                       catch
   error([' The input argument  ',param.traces, ...
         '  of read_segy_file is probably not a valid logical expression'])
                        end
end

%       Read seismic trace
if strcmpi(param.format,'ibm')    % IBM format (requires conversion)
   %if ntraces < 10; disp(param.format); end %test
   temp=fread(fid,no_samples,'uint');
   if boolean
      ntraces=ntraces+1;
      seismic.traces(:,ntraces)=ibm2ieee(temp(idx_time ));
   end
else                                       % Other format (no conversion required)
   % if ntraces < 10; disp('ieee'); end%test
   temp=fread(fid,no_samples,'float32');
   if boolean
      ntraces=ntraces+1;
      try
      seismic.traces(:,ntraces)=temp(idx_time);
      catch
      % keyboard
      end
   end
end

test=fread(fid,1,'int32');                 % Check if there is one more trace

		end

				catch
 msgdlg('Prematude end of SEG-Y file or other file reading error.')

				end

fclose(fid);
fprintf('\n')

if S4M.history
   seismic=s_history(seismic,'add',filename);
end

%       Remove unused reserved space
%seismic.traces=seismic.traces(:,1:ntraces);
seismic.traces(:,ntraces+1:end)=[];
headers(:,ntraces+1:end)=[];

%       Remove headers with duplicate header mnemonics
if isfield(seismic,'header_info')
   mnems=seismic.header_info(:,1);
   lmnems=length(mnems);
%   bool=logical(zeros(lmnems,1));
   bool=false(lmnems,1);

   if lmnems > 1
      try
      if length(unique(mnems)) < lmnems
         for ii=lmnems-1:-1:2
            bool1=ismember(mnems(1:ii),mnems(ii+1));
            bool(1:ii)= bool1(1:ii) | bool(1:ii);
         end
         seismic.header_info(bool,:)=[];
         headers(bool,:)=[];
      end
      catch
         dbstack
         ple
         alert('An error of unknown orign has been cought.')
         keyboard
      end
   end
end


%       Remove header mnemonics that contain only zeros
nh=size(headers,1);
index=1:nh;
for ii=n_default_headers:-1:1;
%  idx=find(headers(ii,:) ~= 0);
%  if isempty(idx)
   if all(headers(ii,:) == 0)
      index(ii)=[];
   end
end

if ~isempty(index)
   seismic.header_info=seismic.header_info(index,:);
   seismic.headers=headers(index,:);

%       Check if Header "lag" still exists
   [index,ier]=header_index1(seismic,'lag');


   if ier == 0  &&  ~param.ignoreshift
      disp('Seismic data shifted since header "lag" is not identically zero.')
      disp(['Lag varies from ',num2str(min(seismic.headers(index,:))),' to ',num2str(max(seismic.headers(index,:)))])
      seismic=s_shift(seismic,{'shifts',seismic.headers(index,:)});
   end
else
   seismic=rmfield(seismic,'header_info');
end

if seismic.aux_per_record == 0
   seismic=rmfield(seismic,'aux_per_record');
end

if isempty(seismic.traces)
   msgdlg([' Alert from "read_segy_file": No seismic traces read from file ',filename])
end

seismic.fp_format_of_segy_file=param.format;


%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%

function [parameters,ebcdic_header,binary_header]=initiate_segy_file_reading_no1(filename,param,o_format)

global S4M ABORTED

parameters=[];

%       Open the file
if ~isempty(filename)
   fid=fopen(filename,'r',o_format);
   if fid == -1 
      if ~S4M.deployed
         fprintf(['... unable to find file ',filename,'\n']);
      end
   else
      filename2S4M(filename)
   end
else
   fid=-1;
end

if fid==-1     % Open file selector window
   [filename,ierr]=get_filename4r('sgy');
   if ierr
      return
   end
   fid=fopen(filename,'r',o_format);
   if fid < 0
      error(['File "',filename,'" could not be opened.'])
   end
end


%  Read EBCDIC reel header 1
header_ebcdic=fread(fid,3200,'uchar');
if isempty(header_ebcdic)
   errordlg('EBCDIC header is empty; requested file is either empty or not an SEG-Y file.', ...
      S4M.name)
   ABORTED=true;
end

if nargout > 1
   ebcdic_header=char(ebcdic2ascii(reshape(header_ebcdic,80,40)'));
end

% Read binary real header
bh=fread(fid,400,'uchar');

two_bytes=bh(1:2:399)*256+bh(2:2:400);
four_bytes=((bh(1:4:9)*256+bh(2:4:10))*256+bh(3:4:11))*256+bh(4:4:12);

if nargout > 2
   binary_header=two_bytes;
end

seismic.type='seismic';
seismic.tag='unspecified';
[dummy,name]=fileparts(S4M.filename);  %#ok First output variable is not required
seismic.name=name;
seismic.from=fullfile(S4M.pathname,S4M.filename);
seismic.line_number=four_bytes(2);
seismic.reel_number=four_bytes(3);
seismic.traces_per_record=two_bytes(7);
seismic.aux_per_record=two_bytes(8);
seismic.first=0;
seismic.step=two_bytes(9)/1000;
no_samples=two_bytes(11);
seismic.last=seismic.step*(no_samples-1);
seismic.units='ms';

if two_bytes(13) == 1 
   if strcmpi(param.format,'header')  || strcmpi(param.format,'ibm')
      param.format='ibm';
 
   else
      display('IEEE format requested')
      warning(warnid,['Data apparently stored as 32-bit IBM floating point numbers; ' ...
         'data sample format code = ',num2str(two_bytes(13))]);
      
   end

elseif two_bytes(13) == 5
   if strcmpi(param.format,'header')  || strcmpi(param.format,'ieee-be')
      param.format='ieee-be';
 
   else
      display('IBM format requested')
      warning(warnid,['Data apparently stored as 32-bit IEEE big-endian floating point numbers; ' ...
         'data sample format code = ',num2str(two_bytes(13))]);
      
   end

else

   myerror(['Data in unsupported format; ' ...
            'data sample format code = ',num2str(two_bytes(13))]);
   if ABORTED
      fclose(fid)
      return
   end
end


if two_bytes(14) ~= 0
  seismic.cdp_fold=two_bytes(14);
end

if two_bytes(28) == 1
  units='m';
elseif two_bytes(28) == 2
  units='ft';
else
  units='unknown';
end

[seismic,idx_time]=select_times(seismic,no_samples,fid,param);

%       Compute indices of two-byte and four-byte header words which are to be saved
%       by default
indices2=([29;109]-1)/2+1;
indices4=([1;9;13;17;21;25;37;49;61;65;73;77;45;81;85;41;181;185;189;193]-1)/4+1;
nindices2=length(indices2);
nindices4=length(indices4);
i_lag=nindices4+2;       % Index of lag
[seismic,indices2,indices4,n_default_headers] = ...
             select_headers(seismic,units,indices2,indices4,i_lag,param);


parameters.fid=fid;                     % File ID
parameters.seismic=seismic;             % Seismic structure without traces and headers
parameters.indices2=indices2;           % Indices of 2-byte headers
parameters.indices4=indices4;           % Indices of 4-byte headers
parameters.idx_time=idx_time;           % indices of trace samples to output
parameters.format=param.format;         % Data format to read
parameters.no_samples=no_samples;       % Number of samples per trace input
parameters.filename=filename;           % Name of SEG-Y file
parameters.traces=param.traces;         % Parameters controlling trace selection
parameters.n_default_headers=n_default_headers;   % Number of headers read by default
parameters.i_lag=i_lag;                 % Description index of lag
parameters.nindices2=nindices2;
parameters.nindices4=nindices4;        


%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
function [header,nh,max_trace,idx_trace,h_index,trace_select,param] = ...
   select_traces(seismic,fid,param)
% Function creates parameters which  control which traces are output

if isempty(param.traces)
  max_trace=inf;
  idx_trace=[];
  h_index=[];
  trace_select=0;
  header=[];
  nh=[];

elseif ischar(param.traces)  % Compute criteria for outputting/discarding traces
  trace_select=1;
  max_trace=inf;
  idx_trace=[];
  param.traces=lower(param.traces);
  header=unique(extract_words(param.traces));

  nh=length(header);
  if nh == 0
    fclose(fid);
    error([' No header in trace constraint ',param.traces])
  end

  h_index=zeros(nh,1);
  counter=0;
  for ii=1:nh
    temp=find(ismember(seismic.header_info(:,1),lower(header(ii))));
    if ~isempty(temp)
      if length(temp) > 1
         disp([' More than one header with mnemonic "',header{ii},'" found when '])
         disp(' analyzing the trace-selection constraints.')
         disp(' Probably caused by explicitely requesting a header that is already ')
         disp(' read by default and then using it in the constraint.')
         pause(0)
         error(' Abnormal termination')
      end
      counter=counter+1;
      header(counter)=header(ii);
      h_index(counter)=temp;
    end
  end
  nh=counter;

else
  trace_select=2;
  idx_trace=param.traces;
  h_index=[];

     try
  max_trace=max(idx_trace);
     catch
  disp(' Apparently wrong syntax using keyword "traces"')
  error(' Abnormal termination')
     end

  header=[];
  nh=[];
end

%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
function [seismic,idx_time]=select_times(seismic,no_samples,fid,param)
% Function sets parameter that determine the time range to output

if iscell(param.times)
   param.times=cat(2,param.times{:});
end

%       Selection criteria for traces and times
if ~isempty(param.times)    % Compute index vector for time samples

   param.times(1)=ceil(param.times(1)/seismic.step)*seismic.step;
   if param.times(1) > 0
      seismic.first=param.times(1);
   end
   if param.times(2) < seismic.first
      fclose(fid);
      error([' Selected end time (',num2str(param.times(2)), ...
          ') smaller than start time (',num2str(seismic.first),')']);
   end
   if param.times(2) < seismic.last
      seismic.last=fix(param.times(2)/seismic.step)*seismic.step;
   end
    
   ita=ceil(seismic.first/seismic.step)+1;
   ite=floor(seismic.last/seismic.step)+1;
   idx_time=(max([1,ita]):min([ite,no_samples]))';
   if isempty(idx_time)
      fclose(fid);
      error(' No time samples selected. Check parameter "times"')
   end
else
  idx_time=(1:no_samples)';   % Keep all time samples
end

%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
function [seismic,indices2,indices4,n_default_headers]= ...
       select_headers(seismic,units,indices2,indices4,i_lag,param)
% Function sets parameters which contol what headers to output

%global param

%         Select standard headers and add user-defined headers
% Descriptions for standard binary trace headers
seismic.header_info(:,3)=[...
      {'Trace sequence number within line'}; ...        % 4-byte headers 
      {'Original Field record number'}; ...
      {'Trace sequence number within original field record'}; ...
      {'Energy source point number'}; ...
      {'CDP number'}; ...
      {'Trace sequence number within CDP ensemble'}; ...
      {'Offset'}; ...
      {'Source depth below surface'}; ...
      {'Water depth at source'}; ...
      {'Water depth at receiver group'}; ...
      {'X coordinate of source'}; ...
      {'Y coordinate of source'}; ...
      {'Sourface elevation at source'}; ...
      {'X coordinate of receiver'}; ...
      {'Y coordinate of receiver'}; ...
      {'Receiver elevation'}; ...
      {'X-coordinate of CDP'}; ...
      {'Y-coordinate of CDP'}; ...
      {'In-line number'}; ...
      {'Cross-line number'}; ...
      {'Trace type (1=live,2=dead,3=dummy,...)'}; ...   % 2-byte headers
      {'Lag time between shot and recording start'}; ...
];

seismic.header_info(:,1)=[ ...  % 4-byte headers
      {'ds_seqno'}; ... 
      {'ffid'}; ...
      {'o_trace_no'}; ...  
      {'source'}; ...   
      {'cdp'}; ...      
      {'seq_cdp'}; ... 
      {'offset'}; ...     
      {'depth'}; ...  
      {'sou_h2od'}; ...  
      {'rec_h2od'}; ... 
      {'sou_x'}; ...     
      {'sou_y'}; ...      
      {'sou_elev'}; ...      
      {'rec_x'}; ...       
      {'rec_y'}; ...     
      {'rec_elev'}; ...
      {'cdp_x'};...
      {'cdp_y'}; ...
      {'iline_no'}; ...
      {'xline_no'}; ...
      {'trc_type'}; ...         % 2-byte headers
      {'lag'}; ...
];

n_default_headers=size(seismic.header_info,1);

seismic.header_info(1:n_default_headers,2)=deal({'n/a'});
seismic.header_info(7:16,2)={units};
seismic.header_info(i_lag,2)={'ms'};

%      Defaults for some commonly requested headers
defaults.iline_no={'n/a','In-line number'};
defaults.xline_no={'n/a','Cross-line number'};
defaults.cdp_x={units,'X-coordinate of CDP'}; 
defaults.cdp_y={units,'Y-coordinate of CDP'}; 

if isempty(param.headers)
   nh=0;
elseif ~iscell(param.headers{1})
   param.headers={param.headers};
   nh=1;
else
   nh=length(param.headers);
end

add=cell(nh,5);
for ii=1:nh
   nh1=length(param.headers{ii});
   add(ii,1:nh1)=param.headers{ii};
   if nh1 == 5
     if isempty(add{ii,4}); add(ii,4)={'n/a'}; end
     if isempty(add{ii,5}); add(ii,5)={'not available'}; end
   else
     if sum(ismember(fieldnames(defaults),lower(add{ii,1})))
%        add(ii,4:5)=getfield(defaults,lower(add{ii,1}));
        add(ii,4:5)=defaults.(lower(add{ii,1}));
     else
        add(ii,4:5)={'n/a','not available'};
     end
   end
end
get.mnem=add(:,1);
get.first=add(:,2);
get.bytes=add(:,3);
get.units=add(:,4);
get.descriptions=add(:,5);
nheader=size(get.mnem,1); 

for jj=1:nheader
   if get.bytes{jj} == 2
      strt=(get.first{jj}-1)/2+1;
      if fix(strt) ~= strt
         error(['First byte for header ',get.mnem{jj}, ' is wrong'])
      end
      indices2=[indices2;strt];  %#ok
   else
      strt=(get.first{jj}-1)/4+1;
      if fix(strt) ~= strt
         error(['First byte for header ',get.mnem{jj}, ' is wrong'])
      end
      indices4=[indices4;strt];  %#ok
   end
end

seismic.header_info=cat(1,seismic.header_info, ...
   cat(2,lower(get.mnem(:)),get.units(:),get.descriptions(:)));

%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
function ascii=ebcdic2ascii(ebcdic)
% Function converts EBCDIC string to ASCII
% see http://www.room42.com/store/computer_center/code_tables.shtml
% Date Feb. 20, 2000;  written by E. Rietsch
% INPUT
% ebcdic	EBCDIC string
% OUTPUT
% ascii		ASCII string
%		  ascii=ebcdic2ascii(ebcdic)

pointer= ...
[ 0    16    32    46    32    38    45    46    46    46    46    46   123   125    92    48
  1    17    33    46    46    46    47    46    97   106   126    46    65    74    46    49
  2    18    34    50    46    46    46    46    98   107   115    46    66    75    83    50
  3    19    35    51    46    46    46    46    99   108   116    46    67    76    84    51
  4    20    36    52    46    46    46    46   100   109   117    46    68    77    85    52
  5    21    37    53    46    46    46    46   101   110   118    46    69    78    86    53
  6    22    38    54    46    46    46    46   102   111   119    46    70    79    87    54
  7    23    39    55    46    46    46    46   103   112   120    46    71    80    88    55
  8    24    40    56    46    46    46    46   104   113   121    46    72    81    89    56
  9    25    41    57    46    46    46    46   105   114   122    46    73    82    90    57
 10    26    42    58    46    33   124    58    46    46    46    46    46    46    46    46
 11    27    43    59    46    36    44    35    46    46    46    46    46    46    46    46
 12    28    44    60    60    42    37    64    46    46    46    46    46    46    46    46
 13    29    45    61    40    41    95    39    46    46    91    93    46    46    46    46
 14    30    46    46    43    59    62    61    46    46    46    46    46    46    46    46
 15    31    47    63   124    94    63    34    46    46    46    46    46    46    46    46];
pointer=reshape(pointer,1,256);

ascii=pointer(ebcdic+1);

%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
function d = ibm2ieee (ibmf)

% Name:         ibm2ieee
% Abstract:     convert a matrix of IBM/360 32-bit floats
%               to IEEE doubles.
%
%               IBMF is the matrix of IBM/360 32-bit floats each
%               stored as a 32 bit unsigned big-endian integer
%               in a MATLAB double.
%
%               The format of a IBM/360 32-bit float is:
%
%                  sign 7-bit exponent  24 bit fraction
%                  The base is 16. The decimal point is to
%                  the left of the fraction. The exponent is
%                  biased by +64.
%
%               The basic idea is that you use floating point on
%               the various fields.
%
%               ieee = sign * 16 ^ (exponent - 64) * fraction / 16 ^ 6
%
% By:           Martin Knapp-Cordes
%               The MathWorks, Inc.
%
% Date(s):      Jun 95 - 28, 29

% $Revision: 1.2 $  $Date: 1995/06/29 14:50:03 $
% This M-file is not officially supported by The MathWorks, Inc.  Please use
% it as is, or modify it to your specific need
%
%Assuming you have a file, which contains IBM float 32 format binary data, called
%5702.seg, then you must use the following FOPEN and FREAD call the read the
%file:
%
%   fid = fopen('5702.seg','r','b');
%
%%
%% Read first data record - IBM/360 32-bit floating format
%%                          Read them as unsigned (32-bit) integers.
%% Convert to IEEE doubles using ibm2ieee.
%%
%% size - number of elements to read
%
%    ibm1 = fread(fid,size,'uint');
%    ieee1 = ibm2ieee(ibm1);
%----------------------------------------------------------------------------
%
    if (nargin ~= 1)
        error ('Wrong number of arguments.');
    elseif (isempty(ibmf))
        error ('Argument is an empty matrix');
    end
%
    aibmf = sprintf('%08x',ibmf);
%
% hexd(1) - 1st hex digit - first bit is sign, next 3 bits high order exponent
% hexd(2) - 2nd hex digit - bits of low order exponent
% hexd(3) - 3rd-8th hex digit - bits of fraction
%
    hexd = sscanf(aibmf,'%1x%1x%6x',[3,inf]);
    d = (1 - (hexd(1,:) >= 8) .* 2) .* ...
        16 .^ ((hexd(1,:) - (hexd(1,:) >= 8) .* 8) .* 16 + hexd(2,:) ...
        - 70).* hexd(3,:);
    d = reshape(d,size(ibmf));
