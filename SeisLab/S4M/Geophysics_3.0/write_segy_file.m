function write_segy_file(seismic,filename,varargin)
% Function writes seismic data to disk in SEG-Y file format. 
% If the start time is greater than zero, zeros are prepended. If it is less 
% than zero, a warning message is printed. 
% The only floating-point format supported is IEEE big-endian, which is one of 
% the official SEG-Y standard formats. 
% ProMAX recognizes the numeric code associated with this format and reads 
% SeisLab-generated SEG-Y files without need for for any special settings.
%
% Written by: E. Rietsch: March 12, 2000
% Last updated: July 10, 2009: If datset has negative start time shift it to
%                              zero and set "lag" header tp start time.
%
%        write_segy_file(seismic,filename,varargin)
% INPUT
% seismic     structure; the following fields are required
%        traces    seismic traces
%        first
%        last
%        step
%        units
% filename    Full filename; if empty (or invalid) a file-selection window will
%             pop up to allow interactive file selection
% varargin    Variable number of arguments. Each argument is a cell array whose
%             first element is a keyword and whose other elements can be
%             strings, numeric values, or other cell arrays. Possible keywords  
%             are:  
%        'headers' header values to be written to the binary trace header in 
%             addition to those written by default (if available); 
%             Headers stored by default are:
%               ds_seqno      Trace sequence number within line (1-4)
%               ffid          Original Field record number (9-12)
%               o_trace_no    Trace sequence number within original field record (13-16)
%               source        Energy source point number (17-20)
%               cdp           CDP ensemble number (21-24)
%               seq_cdp       Trace sequence number within CDP ensemble (25-28)
%               trc_type      Trace ID (1=live,2=dead,3=dummy,4=time break,...) (29-30)
%               offset        Distance from source point to receiver group (37-40)
%               depth         Source depth below surface (49-52)
%               sou_h2od      Water depth at source (61-64)
%               rec_h2od      Water depth at receiver group (65-68)
%               sou_x         X coordinate of source (73-76)
%               sou_y         Y coordinate of source (77-80)
%               sou_elev      surface elevation at source (45-48)
%               rec_x         X coordinate of receiver (81-84)
%               rec_y         Y coordinate of receiver (85-88)
%               rec_elev      receiver elevation (41-44);
%               lag           Lag time between shot and recording start in ms (109-110)
%                  (the value of lag is added to the start time of the 
%                  seismic; hence it can be used to simulate non-zero start
%                  time of the data)
%               cdp_x         X coordinate of CDP (181-184)
%               cdp_y         Y coordinate of CDP (185-189)
%               iline_no      In-line number (189-192)
%               xline_no      Cross-line number (193-196)
%               The numbers in parentheses at the end of the line denote the location 
%               of the corresponding bytes in the SEG-Y trace header
%
%               The cell array for the user-specified headers has the form 
%               {'headers',{mnem1,first,bytes},{mnem2,first,bytes},...} where                
%               "mnem1", "mnem2", ... denote header mnemonics (such as ILINE_NO, XLINE_NO), 
%               "first" denotes the first byte in the binary header, 
%               "bytes" denotes the number of bytes occupied by the mnemonic (2 or 4), 
%               Example: {'headers',{'ILINE_NO',181,4},{'XLINE_NO',185,4}}
%                         these headers must, of course, be present in the input data set.
%               Default: no headers other than the standard SEG-Y headers listed above
%                         will be saved.
%        'print'   printout of messages. possible values 0 or 1.
%                Default: {'print',1}   this means that messages are printed.
%        'ascii_header'  ASCII version of the 40-line, 3200 byte EBCDIC header
%                a default EBCDIC header will be inserted if this header is not supplied
% EXAMPLE
%        seismic=s_data3d;
%        write_segy_file(seismic,filename,{'headers',{'iline_no',189,4},{'xline_no',193,4}});              

% UPDATE HISTORY
%        July 23, 2006: Replaced header mnemonics "field_rec_no" by "ffid" 
%                              and  "trc_id" by "trc_type";
%                              write field "sort" with trace-sort code (if it 
%                              exists) to bytes 29-30 of the binary tape
%                              header
%        November 5. 2008: Fixed bug that caused a problem under Linux

global ABORTED

if ~istype(seismic,'seismic');
   error(' The first input argument must be a seismic data set.')
end

%       Set default parameters
param.format='ieee';
param.headers={};
param.ascii_header='';
param.print=1;

%       Decode and assign input arguments
param=assign_input(param,varargin);

if nargin < 2
   filename='';
end

%       Add zeros to make start time zero
if seismic.first > 0
   seismic=s_select(seismic,{'times',0,seismic.last});
elseif seismic.first < 0
   display([' WARNING! Start time of seismic < 0. Seismic data will be shifted by ', ...
                num2str(-seismic.first),' ms']);
   if isheader(seismic,'lag');
       display('Existing header "LAG" will be written to the SEG-Y file')
   else
       seismic=add_header(seismic,seismic.first,{'lag','ms','Lag'});
       display('A header "LAG" will be written to the SEG-Y file')
   end

end

%       Define file format
if strcmpi(param.format,'ieee')
   param.format='ieee-be';
   o_format='ieee-be';
   datafmt=5;	% Data-format code
   fidx=0;
end

%      Create additional headers for output
[nsamp,ntr]=size(seismic.traces);
dt=seismic.step*1000;
seismic=ds_header(seismic,'add','gen_scale_factor',1,'','Scale factor');
seismic=ds_header(seismic,'add','number_of_samples',nsamp,'','Number of samples in this trace');
seismic=ds_header(seismic,'add','samp_int',dt,'us','Sample interval of this trace in us');
  
 
%   Handle nulls in traces
if isnull(seismic)
   seismic.traces(isnan(seismic.traces))=0;
end

%   Handle nulls in headers
if isfield(seismic,'header_null')
   if isnan(seismic.header_null)
      seismic.headers(isnan(seismic.headers))=0;
   end
end   

%  Open file
if ~isempty(filename)
   fid=fopen(filename,'w',o_format);
   if fid < 0
      disp(['... unable to create requested file "', filename,'"']);
   end
else
   fid=-1;
   filename=[seismic.name,'.sgy'];
end
if fid == -1
   [selected_file,ierr]=get_filename4w('sgy',filename);
   if ierr
      return;  
   end
   fid=fopen(selected_file,'w',o_format);
   if fid < 0
      disp(['... unable to create requested file "', filename,'"']);
      ABORTED=true;
      return
   end
%   [pathname,filename]=fileparts(selected_file);
%   S4M.seismic_path=pathname;
end 

%    Write EBCDI reel header
if isempty(param.ascii_header)
   ascii_header=make_header; 
else
   ascii_header=param.ascii_header;
   [nh1,mh1]=size(ascii_header);
   if nh1*mh1 ~= 3200
      error('ASCII/EBCDIC header nust have 3200 bytes')
   else
      if nh1 > mh1
         ascii_header=ascii_header';
      end
   end
end
% fwrite(fid,ascii2ebcdic(ascii_header),'char');    % Linux problem?
fwrite(fid,ascii2ebcdic(ascii_header),'uchar');
if param.print
   disp('EBCDIC reel header written')
end;

%   Write binary reel header
two_bytes=zeros(194,1);

if isfield(seismic,'job_id')
   jobid=seismic.job_id;
else
   jobid=1;
end 

if isfield(seismic,'line_number')
   lineid=seismic.line_number;
else
   lineid=1;
end 

if  isfield(seismic,'reel_number')
  reelid=seismic.reel_number;
else
   reelid=1;
end 

if isfield(seismic,'traces_per_record')
   two_bytes(1)=seismic.traces_per_record;
else
   two_bytes(1)=ntr;
end 

if isfield(seismic,'aux_per_record')
   two_bytes(2)=seismic.aux_per_record;
else
   two_bytes(2)=0;
end 

two_bytes(3:7)=[dt,dt,nsamp,nsamp,datafmt]';

if isfield(seismic,'cdp_fold')
   two_bytes(8)=seismic.cdp_fold;
end

if isfield(seismic,'sort')
   two_bytes(9)=seismic.sort;
end

if ~isfield(seismic,'headers')
   two_bytes(22)=0;
else 
   idx=find(ismember(seismic.header_info(:,1), ...
        {'offset','sou_x','rec_x','cdp_x','sou_y','rec_y','cdp_z'}));
   if isempty(idx)
      two_bytes(22)=0;
   else
      units=unique(seismic.header_info(idx,2));
      if length(units) > 1
         disp(' Units of measurement for distance:')
         disp(units)
         warning(warnid,'Inconsistent distance units in headers; units in SEG-Y file are set to unknown');
      elseif strcmpi(char(units),'m')
         two_bytes(22)=1;
      elseif strcmpi(char(units),'ft')
         two_bytes(22)=2;
      else
         warning(warnid,'Distance units in headers are neither ft nor m; units in SEG-Y file are set to unknown');
         two_bytes(22)=0;
      end
   end
end

fwrite(fid,[jobid lineid reelid],'int32');
fwrite(fid,two_bytes,'int16');
if param.print, disp('Binary reel header written'), end;

%       Write headers and traces
nh=size(seismic.header_info,1);
start=zeros(nh,1);
bytes=zeros(nh,1);
index=zeros(nh,1);

kk=1;
[start,bytes,index,kk]=set_parameters(seismic,'ds_seqno',1,4,start,bytes,index,kk,param.print);
[start,bytes,index,kk]=set_parameters(seismic,'ffid',9,4,start,bytes,index,kk,param.print);
[start,bytes,index,kk]=set_parameters(seismic,'o_trace_no',13,4,start,bytes,index,kk,param.print);
[start,bytes,index,kk]=set_parameters(seismic,'source',17,4,start,bytes,index,kk,param.print);
[start,bytes,index,kk]=set_parameters(seismic,'cdp',21,4,start,bytes,index,kk,param.print);
[start,bytes,index,kk]=set_parameters(seismic,'seq_cdp',25,4,start,bytes,index,kk,param.print);
[start,bytes,index,kk]=set_parameters(seismic,'trc_type',29,2,start,bytes,index,kk,param.print);
[start,bytes,index,kk]=set_parameters(seismic,'offset',37,4,start,bytes,index,kk,param.print);
[start,bytes,index,kk]=set_parameters(seismic,'depth',49,4,start,bytes,index,kk,param.print);
[start,bytes,index,kk]=set_parameters(seismic,'sou_h2od',61,4,start,bytes,index,kk,param.print);
[start,bytes,index,kk]=set_parameters(seismic,'rec_h2od',65,4,start,bytes,index,kk,param.print);
[start,bytes,index,kk]=set_parameters(seismic,'sou_x',73,4,start,bytes,index,kk,param.print);
[start,bytes,index,kk]=set_parameters(seismic,'sou_y',77,4,start,bytes,index,kk,param.print);
[start,bytes,index,kk]=set_parameters(seismic,'sou_elev',45,4,start,bytes,index,kk,param.print);
[start,bytes,index,kk]=set_parameters(seismic,'rec_x',81,4,start,bytes,index,kk,param.print);
[start,bytes,index,kk]=set_parameters(seismic,'rec_y',85,4,start,bytes,index,kk,param.print);
[start,bytes,index,kk]=set_parameters(seismic,'rec_elev',41,4,start,bytes,index,kk,param.print);
[start,bytes,index,kk]=set_parameters(seismic,'gen_scale_factor',69,2,start,bytes,index,kk,param.print);
[start,bytes,index,kk]=set_parameters(seismic,'gen_scale_factor',71,2,start,bytes,index,kk,param.print);
[start,bytes,index,kk]=set_parameters(seismic,'gen_scale_factor',89,2,start,bytes,index,kk,param.print);
[start,bytes,index,kk]=set_parameters(seismic,'number_of_samples',115,2,start,bytes,index,kk,param.print);
[start,bytes,index,kk]=set_parameters(seismic,'samp_int',117,2,start,bytes,index,kk,param.print);
[start,bytes,index,kk]=set_parameters(seismic,'lag',109,2,start,bytes,index,kk,param.print);
[start,bytes,index,kk]=set_parameters(seismic,'cdp_x',181,4,start,bytes,index,kk,param.print);
[start,bytes,index,kk]=set_parameters(seismic,'cdp_y',185,4,start,bytes,index,kk,param.print);
[start,bytes,index,kk]=set_parameters(seismic,'iline_no',189,4,start,bytes,index,kk,param.print);
[start,bytes,index,kk]=set_parameters(seismic,'xline_no',193,4,start,bytes,index,kk,param.print);

%       Add user-specified headers
for ii=1:length(param.headers)
   if length(param.headers{ii}) ~= 3
      disp([char(13),'  Error in specification of headers to be written to file: ', ...
          cell2str(param.headers{ii}(1))])
      error(' Probably insufficient number of parameters')
   end
   [start,bytes,index,kk]=set_parameters(seismic,param.headers{ii}{1}, ...
        param.headers{ii}{2},param.headers{ii}{3},start,bytes,index,kk,param.print);
end

start(kk:end)=[];    % Remove unneeded, previously reserved array elements
bytes(kk:end)=[];    % Remove unneeded, previously reserved array elements
index(kk:end)=[];    % Remove unneeded, previously reserved array elements

for ii=1:ntr
  write_trace(fid,seismic.traces(:,ii),seismic.headers(index,ii),start,bytes,fidx);
end
fclose(fid);

%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%

function [start,bytes,index,kk]=set_parameters(seismic,mnem,sb,nb,start,bytes,index,kk,iprint)
% Function sets values start(index),bytes(index) to the starting location
% of the header mnemonic and increments index
% seismic    seismic structure
% mnem    header mnemonic whose starting location in the four-byte representation 
%         (nb=4) or the two-byte representation (nb=2) needs to be set
% sb      starting byte
% nb      number of bytes
% start   array where the index for the two-byte and four-byte header is stored
% bytes   array where the number of bytes required by this header is stored
% index   index into row of header which contains the header values of 
%         the mnemonic mnem
% kk      next location in arrays start and bytes
% OUTPUT
% start   updated start array
% bytes   updated bytes array
% index   updated index
% kk      input index incremented by 1
% iprint  print-out control parameter: 1 printout, 0 no printout

global S4M

if S4M.case_sensitive
   idx=find(ismember(seismic.header_info(:,1),mnem));
else
   idx=find(ismember(lower(seismic.header_info(:,1)),mnem));
end
if isempty(idx)
   if iprint, disp(['Header "',mnem,'" not found in dataset and assumed to be zero']), end
   return, 
end
index(kk)=idx;
bytes(kk)=nb;

if nb == 2
   temp=fix((sb-1)/2);
   if temp*2 ~= sb-1
      error(['Starting byte location for header ',mnem,'(',num2str(sb),' is not odd'])
   else
      start(kk)=temp+1;
   end
else
   temp=fix((sb-1)/4);
   if temp*4 ~= sb-1
      error(['Starting byte location for header ',mnem,'(',num2str(sb),' is not 1 plus a multiple of 4'])
   else
      start(kk)=temp+1;
   end  
end
kk=kk+1;

%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%

function write_trace(fid,trace,headers,start,bytes,fidx)
% Function writes one seismic trace together with its header to file
% fid     file identification number
% trace   seismic trace
% headers header values associated with the trace
% start   starting four-byte or two-byte index in the 240-byte header 
%         for each header value
% bytes   number of bytes for each header value
% fidx    format index;
%         fidx=0; no conversion
%         fidx=1; conversion to IMB floating point format

% nsamp=length(trace);
nh=length(headers);

fbytes=zeros(60,1);
tbytes=zeros(120,1);
for ii=1:nh
   if bytes(ii) == 4
      fbytes(start(ii))=headers(ii);
   else
      tbytes(start(ii))=headers(ii);
   end
end
fwrite(fid,fbytes( 1: 7),'int32');
fwrite(fid,tbytes(15:18),'int16');
fwrite(fid,fbytes(10:17),'int32');
fwrite(fid,tbytes(35:36),'int16');
fwrite(fid,fbytes(19:22),'int32');
fwrite(fid,tbytes(45:90),'int16');
fwrite(fid,fbytes(46:60),'int32');

% Write trace
if fidx == 0
   fwrite(fid,trace,'float32');
else
   error('IBM floating point not yet implemented')
end

%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%

function ascii_header=make_header
% Function creates ASCII version of standard EBCIC header of SEG-Y format

ascii_header=char(...
'C 1 CLIENT                        COMPANY                       CREW NO', ...         
'C 2 LINE            AREA                        MAP ID ', ...                          
'C 3 REEL NO           DAY-START OF REEL     YEAR      OBSERVER', ...                   
'C 4 INSTRUMENT: MFG            MODEL            SERIAL NO', ...                       
'C 5 DATA TRACES/RECORD        AUXILIARY TRACES/RECORD         CDP FOLD', ...           
'C 6 SAMPLE INTERNAL         SAMPLES/TRACE       BITS/IN      BYTES/SAMPLE', ...        
'C 7 RECORDING FORMAT        FORMAT THIS REEL        MEASUREMENT SYSTEM', ...           
'C 8 SAMPLE CODE: FLOATING PT     FIXED PT     FIXED PT-GAIN     CORRELATED ', ...      
'C 9 GAIN  TYPE: FIXED     BINARY     FLOATING POINT     OTHER ', ...                   
'C10 FILTERS: ALIAS     HZ  NOTCH     HZ  BAND    -     HZ  SLOPE    -    DB/OCT ', ...  
'C11 SOURCE: TYPE            NUMBER/POINT        POINT INTERVAL', ...                   
'C12     PATTERN:                           LENGTH        WIDTH', ...                   
'C13 SWEEP: START     HZ  END     HZ  LENGTH      MS  CHANNEL NO     TYPE', ...         
'C14 TAPER: START LENGTH       MS  END LENGTH       MS  TYPE', ...                      
'C15 SPREAD: OFFSET        MAX DISTANCE        GROUP INTERVAL', ...                    
'C16 GEOPHONES: PER GROUP     SPACING     FREQUENCY     MFG          MODEL', ...        
'C17     PATTERN:                           LENGTH        WIDTH', ...                   
'C18 TRACES SORTED BY: RECORD     CDP     OTHER', ...                                  
'C19 AMPLITUDE RECOVEY: NONE      SPHERICAL DIV       AGC    OTHER', ...                
'C20 MAP PROJECTION                      ZONE ID       COORDINATE UNITS', ...           
'C21 PROCESSING:', ...                                                                  
'C22 PROCESSING:', ...                                                                  
'C23 ', ...                                                                             
'C24 ', ...                                                                             
'C25 ', ...                                                                             
'C26 ', ...                                                                             
'C27 ', ...                                                                             
'C28 ', ...                                                                             
'C29 ', ...                                                                             
'C30 ', ...                                                                             
'C31 ', ...                                                                             
'C32 ', ...                                                                             
'C33 ', ...                                                                             
'C34 ', ...                                                                             
'C35 ', ...                                                                             
'C36 ', ...                                                                             
'C37 ', ...                                                                             
'C38 ', ...                                                                             
'C39 ', ...                                                                             
'C40 END EBCDIC')';

%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%

function ebcdic=ascii2ebcdic(ascii)
% Function converts ASCII string to EBCDIC
% see http://www.room42.com/store/computer_center/code_tables.shtml
% Date Feb. 20, 2000;  written by E. Rietsch
% INPUT
% ascii         ASCII string
% OUTPUT
% ebcdic	EBCDIC string
%   		  ebcdic=ascii2ebcdic(ascii)

pointer =  ...
  [0  16  64 240 124 215 125 151  75  75  75  75  75  75  75  75
   1  17  90 241 193 216 129 152  75  75  75  75  75  75  75  75
   2  18 127 242 194 217 130 153  75  75  75  75  75  75  75  75
   3  19 123 243 195 226 131 162  75  75  75  75  75  75  75  75
   4  20  91 244 196 227 132 163  75  75  75  75  75  75  75  75
   5  21 108 245 197 228 133 164  75  75  75  75  75  75  75  75
   6  22  80 246 198 229 134 165  75  75  75  75  75  75  75  75
   7  23 125 247 199 230 135 166  75  75  75  75  75  75  75  75
   8  24  77 248 200 231 136 167  75  75  75  75  75  75  75  75
   9  25  93 249 201 232 137 168  75  75  75  75  75  75  75  75
  10  26  92 122 209 233 145 169  75  75  75  75  75  75  75  75
  11  27  78  94 210 173 146 192  75  75  75  75  75  75  75  75
  12  28 107  76 211 224 147 106  75  75  75  75  75  75  75  75
  13  29  96 126 212 189 148 208  75  75  75  75  75  75  75  75
  14  30  75 110 213  95 149 161  75  75  75  75  75  75  75  75
  15  31  97 111 214 109 150  75  75  75  75  75  75  75  75  75];
pointer=pointer(:);

ebcdic=pointer(ascii+1);
