function [fid,seismic,param,parameters,text_header,binary_header] = ...
                               open_segy_file4reading(filename,varargin)
% Function opens an SEG-Y file, reads the textual header and the binary file 
% header and outputs the file ID, a preliminary seismic data structure 
% with empty fields "header" and "traces".
% In addition, it outputs parameters that allow function "read_segy_file_traces"
% to read consecutive traces.
%
% Written by: E. Rietsch: January 6, 2007
% Last updated: February 27, 2008: bug fix in error message
%
%             [fid,seismic,param,parameters,text_header,binary_header] = ...
%                               open_segy_file4reading(filename,varargin)
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
%             the form {'headers',{mnem1,first,bytes,units,dimension,description}, ...
%             {mnem2,first,bytes,units,dimension,description},...} where
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
%             {'times',first,last}, {'times',[first,last]}, or {'times',[]} 
%             In the first two forma all samples with times between (and 
%             including) first and last (in ms) are output. In the last case
%             all samples are output.
%             Default: {'times',[]}
%     traces  select traces to output; this cell array has the form
%             {'traces',expression}; the variable "expression" is used to 
%             determine which traces to output. "expression" can be an index
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
%    max_mem   maximum amount of contiguous memory in megabytes (MB) bytes available 
%             to store seismic traces;
%             Default: {'max_mem',[]}
%             This means the maximum size is determined internally. 
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
%       seismic.name               file name without exyension
%       seismic.from               Full name of the SEG-Y file
%       seismic.traces             Array of seismic traces
%       seismic.first              Start time of seismic (in ms)
%       seismic.last               End time of seismic (in ms)
%       seismic.step               Sample interval of seismic (in ms)
%       seismic.units              Time units used (ms)
%       seismic headers            Matrix with header mnemonics (one row 
%                                  per header)
%       seismic.header_info        Three-column cell array with header info 
%                                  (one row per header)
%       seismic.null               []
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


% UPDARE HISTORY
%          July 15, 2007: bug fix in trace constraints

global ABORTED

%	Set default output arguments
run_presets_if_needed

%	Set default for input parameters
param.format='header';
param.headers={};
param.ignoreshift=false;
param.debug=false;
%param.precision='single';
param.times=[];
param.traces=[];
param.header_precision='single';
param.max_mem=[];   

if nargin == 0
   filename='';

else
%	Replace defaults by actual input arguments
   param=assign_input(param,varargin,'open_segy_file4reading');
   if ~ismember(lower(param.format),{'ieee','ibm','header'})
      disp([' Unknown trace format: ',param.format])
      disp(' Allowed formats are ''header'', ''ieee'', and ''ibm''.')
      drawnow
      error('Abnormal termination.')
   end
   if ~isempty(param.times)
      param=check_time_range_no0(param);  % Check if the time range is specified correctly
   end
end


%	Open SEG-Y file and get file ID
fid=open_segy_file_no1(filename);

if fid < 0
    seismic=[];
    text_header=[];
    binary_header=[];
    parameters=[];
   return
end
    
%	Read textual file header
text_header=read_textual_file_header_no2(fid);

%	Read binary file header
binary_header=read_binary_file_header_no3(fid);

%	Check FP format and save it and units of measurement for distance in 
%       structure "param"
param=check_file_headers_no4(binary_header,param);

if ABORTED
   seismic=[];
   return
end

%	Create seismic structure and index of the time samples to retain
[seismic,parameters.idx4times]=create_seismic_structure_no5(binary_header,param);

%	Collect info about the headers to read from binary trace header block
[parameters.header_info,parameters.indices,parameters.true4four, ...
       parameters.constraint_info,param]=select_trace_headers2read_no6(param);
if ABORTED
   seismic=[];
   return
end

param.nsamp=length(parameters.idx4times);
param.nheaders=length(parameters.indices);
param.no_samples=binary_header(8);

%	Get number of traces
param.ntraces=get_no_of_traces_no7(seismic.from,binary_header(8));


%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%

function param=check_time_range_no0(param)
% Check the compatibility of the time range specified

if ~isempty(param.times)
   if iscell(param.times)
      param.times=cell2num(param.times);
   end
   if length(param.times) == 1
      param.times=[param.times,param.times];
   elseif length(param.times) == 2
      if param.times(2) < param.times(1)
         error('Start time of time range is greater than end time.')
      end
      if param.times(1) < 0
         error('Start time of time range is less than 0.')
      end
   else
      error('Range of times to read must be specified by a start time and an end time.')
   end
end


%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%

function fid=open_segy_file_no1(filename)
%	Open SEG=Y file and return file ID
% INPUT
% filenane filename; can be empty
% mformat  machine format (see Matlab function "fopen"
% OUPTUT
% fid      file ID

mformat='ieee-be';

%       Open the file
if ~isempty(filename)
   fid=fopen(filename,'r',mformat);
   if fid == -1 
      if ~isdeployed
         disp(['... unable to find file ',filename])
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
   fid=fopen(filename,'r',mformat);
   if fid < 0
      error(['File "',filename,'" could not be opened.'])
   end
end

%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%

function text_header=read_textual_file_header_no2(fid)
% Read EBCDIC header and output it as ASCII
% INPUT
% fid          File ID
% OUTPUT
% text_header  EBCDIC header as ASCII

global ABORTED S4M

%	Read EBCDIC reel header 1
% text_header=fread(fid,3200,'uchar');  % Linux problem
text_header=fread(fid,3200,'uchar');
if isempty(text_header)
   if S4M.deployed
      errordlg('EBCDIC header is empty; requested file is either empty or not an SEG-Y file.', ...
      S4M.name)
      ABORTED=true;
      return
   else
      error('EBCDIC header is empty; requested file is either empty or not an SEG-Y file.')
   end
end

ABORTED=false;
text_header=char(ebcdic2ascii(reshape(text_header,80,40)'));


%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%

function binary_header=read_binary_file_header_no3(fid)
% Read binary header and initiate the seismic dataset
% INPUT
% fid   file ID
% OUTPUT
% binary_header   binary header (two-byte variables and four-byte variables
%                 are stored together in one vector

%	Read binary file header
bh=fread(fid,400,'uchar');

two_bytes=bh(1:2:399)*256+bh(2:2:400);
four_bytes=((bh(1:4:9)*256+bh(2:4:10))*256+bh(3:4:11))*256+bh(4:4:12);

binary_header=[four_bytes(1:3);two_bytes(7:200)];


%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%

function param=check_file_headers_no4(binary_header,param)
%	Check binary-format code and output format and units of measurement in
%       structure param
% INPUT
% binary_header  binary header
% param      structure with input parameters for "read_segy_file"
% OUTPUT
% param      input structure "param" with theo new fields: "format" and "units"   

global ABORTED

%	Check format
if binary_header(10) == 1 
   if strcmpi(param.format,'header')  ||  strcmpi(param.format,'ibm')
%      param.fmt='ibm';
      param.format='ibm';
   else
      disp('IEEE format requested')
      mywarning(['Data apparently stored as 32-bit IBM floating point numbers; ' ...
         'data sample format code = ',num2str(binary_header(10))]);      
   end
   param.precision='uint32=>uint32';

elseif binary_header(10) == 5
   if strcmpi(param.format,'header')  || strcmpi(param.format,'ieee')
%      param.fmt='ieee-be'; 
      param.format='ieee-be';
   else
      display('IBM format requested')
      mywarning(['Data apparently stored as 32-bit IEEE big-endian floating point numbers; ' ...
         'data sample format code = ',num2str(binary_header(10))]);      
   end
   param.precision='single';

else
   if param.debug
      disp(['Data in unsupported format; ' ...
            'data sample format code = ',num2str(binary_header(10))]);
      param.precision='single';

   else
      myerror(['Data in unsupported format; ' ...
            'data sample format code = ',num2str(binary_header(10))]);
      ABORTED=true;
      fclose(fid)
      return
   end
end

%	Extract units of measurement for distances
if binary_header(25) == 1
   param.units='m';
elseif binary_header(25) == 2
   param.units='ft';
else
   param.units='unknown';
end

ABORTED=false;


%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%

function [seismic,idx4times]=create_seismic_structure_no5(binary_header,param)
% Create the fields of the seismic structure and populate them as far as possible
% INPUT
% binary_header   binary header
% param           structure with input parameters for "read_segy_file"
% OUTPUT
% seismic         seismic structure
% idx4time        index of time samples to keep

global S4M

step=binary_header(6)/1000;
no_samples=binary_header(8);
last=step*(no_samples-1);


%	Compute indices of time range 
if ~isempty(param.times)    % Compute index vector for time samples
   ita=ceil(param.times(1)/step);
   param.times(1)=ita*step;
   
   param.times(2)=min(last,param.times(2));
   ite=fix(param.times(2)/step);
   param.times(2)=ite*step;

   idx4times=ita+1:ite+1;

else
  param.times=[0,last];
  idx4times=(1:no_samples)';   % Keep all time samples

end

%	Create seismic structure
seismic.type='seismic';
seismic.tag='unspecified';
[dummy,seismic.name]=fileparts(S4M.filename);  %#ok First output variable is not required
seismic.from=fullfile(S4M.pathname,S4M.filename);
seismic.line_number=binary_header(2);
seismic.reel_number=binary_header(3);
seismic.traces_per_record=binary_header(4);
seismic.aux_per_record=binary_header(5);
if binary_header(11) ~= 0
  seismic.cdp_fold=binary_header(11);
end

seismic.first=param.times(1);
seismic.last=param.times(2);
seismic.step=step;
seismic.units='ms';


%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%

function [header_info,indices,true4four,constraint_info,param]=select_trace_headers2read_no6(param)
% Create a cell array with ibformation about the headers that are to be read
% their bit location and length
% INPUT
% param   input parameters of read_segy_file (plus ome parameters 
%         acquired along the way
% OUTPUT
% header_info
%	The 7 columns of header_info are:
%       mnemonic, units, description, first byte, No. of bytes,
%       bool ("true" if user-requested), and index.
%       If the 240-byte header is read into a 60  element array then the index 
%       defines in which element a 4-byte header is: for a 4-byte header word
%       starting at byte 13 index=4 (index=(start_byte-1)/4+1)
%       The index for a two-byte header is similarly defined.
% indices    vector of header indices
% true4four  boolean vector true if index is for a four-byte header.

global S4M

constraint_info=[];

if isempty(param.headers)
   nh1=0;
elseif iscell(param.headers{1})
   nh1=length(param.headers);  % User-requested headers
else
   param.headers={param.headers};
   nh1=1;
end

%     Select standard headers and add user-defined headers
%     Descriptions for standard binary trace headers
nh0=22;		    % Default headers
nh=nh0+nh1;     % Total number of headers
header_info=cell(nh,7);

header_info(1:nh0,6)=deal({false});

header_info(1:nh0,3)=[...
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
      {'Surface elevation at source'}; ...
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

header_info(1:nh0,[1,4,5]) =  { ...  % 4-byte headers
      'ds_seqno',      1,4; ... 
      'ffid',          9,4; ...
      'o_trace_no',   13,4; ...  
      'source',       17,4; ...   
      'cdp',          21,4; ...      
      'seq_cdp',      25,4; ... 
      'offset',       37,4; ...     
      'depth',        49,4; ...  
      'sou_h2od',     61,4; ...  
      'rec_h2od',     65,4; ... 
      'sou_x',        73,4; ...     
      'sou_y',        77,4; ...      
      'sou_elev',     45,4; ...      
      'rec_x',        81,4; ...       
      'rec_y',        85,4; ...     
      'rec_elev',     41,4; ...
      'cdp_x',       181,4; ...
      'cdp_y',       185,4; ...
      'iline_no',    189,4; ...
      'xline_no',    193,4; ...
      'trc_type',     29,2; ...  % 2-byte headers
      'lag',         109,2};

header_info(1:nh0,2)=deal({'n/a'});
header_info(7:16,2)={param.units};
header_info(22,2)={'ms'};

if nh1 > 0

%   nh=length(param.headers);
%   nh=nh1;

%      Defaults for some commonly requested headers
   defaults.iline_no={'n/a','In-line number'};
   defaults.xline_no={'n/a','Cross-line number'};
   defaults.cdp_x={param.units,'X-coordinate of CDP'}; 
   defaults.cdp_y={param.units,'Y-coordinate of CDP'}; 
   defaults.rec_x={param.units,'X-coordinate of receiver'}; 
   defaults.rec_y={param.units,'Y-coordinate of receiver'}; 
   defaults.sou_x={param.units,'X-coordinate of source'}; 
   defaults.sou_y={param.units,'Y-coordinate of source'}; 

%	Handle header supplided by users

   header_info(nh0+1:nh,6)=deal({true});

   for ii=nh0+1:nh
      nhii=length(param.headers{ii-nh0});
      if nhii == 5
         header_info(ii,[1,4,5,2,3])=param.headers{ii-nh0};
         if isempty(header_info{ii,2}); 
            header_info{ii,2}='n/a';
         end
         if isempty(header_info{ii,3}); 
            header_info{ii,3}=header_info{ii,1};
         end

      elseif nhii == 3
         header_info(ii,[1,4,5])=param.headers{ii-nh0};
         if any(ismember(fieldnames(defaults),lower(header_info{ii,1})))
            header_info(ii,2:3)=defaults.(lower(header_info{ii,1}));
         else
            header_info{ii,2}='n/a';
       	    header_info{ii,3}=header_info{ii,1};
         end

      else
         error('The description of an additional header to be read must be a cell vector with 3 or 5 entries.') 
      end
   end
   ierr=false;
   for jj=nh0+1:nh
      if mod(header_info{jj,4},header_info{jj,5}) ~= 1
         disp(['First byte for header ',header_info{jj,1}, ' is wrong.'])
         ierr=true;
      end
   end

   if ierr
      error('Abnormal termination.')
   end
end

%	Keep unique headers
[dummy,index]=myunique(header_info(:,1)); %#ok First output argument is not required
header_info=header_info(index,:);
nh=length(index);

indices=zeros(nh,1);
true4four=true(nh,1);
for ii=1:nh
   indices(ii)=(header_info{ii,4}-1)/header_info{ii,5}+1;
   header_info{ii,7}=indices(ii); 
   true4four(ii)=header_info{ii,5} == 4;
end

%	If traces are selected via constraints on headers then 
%	create a cell array with header names and associate locations
%       in the "headers" matrix
if ~isempty(param.traces)  &&  ischar(param.traces)
%	Find headers in constraint string
   expression=param.traces;
   words=symvar(expression);

   if ~S4M.case_sensitive
     %		Change expression
     for ii=1:length(words)
         expression=strrep(expression,words{ii},lower(words{ii}));
      end
      param.traces=expression;
      words=lower(words);
      headers=lower(header_info(:,1));
    else
      headers=header_info(:,1);
   end
   words=unique(words);
   index=find(ismember(headers,words));
   bool=ismember('trace_no',words);
   if ~bool  && isempty(index)
      disp([' No headers found in trace constraint "',expression,'".'])
      myerror(' Reading of SEG-Y file terminated abnormally.')
   else
      constraint_info=cell(length(index)+bool,2);
      for ii=1:length(index)
         constraint_info(ii,:)=[headers(index(ii)),{num2str(index(ii))}];
      end
      if bool
         constraint_info(end,:)={'trace_no',0};
      end
   end
end


%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%

function ntr=get_no_of_traces_no7(filename,nsamples)
% Use number of bytes to compute number of traces

ll=dir(filename);
nbytes=ll.bytes;
ntr=0.25*(nbytes-3600)/(nsamples+60);

if ~isnearinteger(ntr)
   mywarning(['Number of bytes in file "',filename,'", (',num2str(nbytes), ...
      '), is not compatible with constant-length traces'])
   ntr=fix(ntr);
end


%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%

function ascii=ebcdic2ascii(ebcdic)
% Function converts EBCDIC string to ASCII
% see http://www.room42.com/store/computer_center/code_tables.shtml
%
% Written by: E. Rietsch: Feb. 20, 2000
% Last updated:
%
%           ascii=ebcdic2ascii(ebcdic)
% INPUT
% ebcdic    EBCDIC string
% OUTPUT
% ascii	   ASCII string


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
