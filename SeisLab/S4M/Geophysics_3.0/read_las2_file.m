function [wlog,las_header]=read_las2_file(filename,iprint)
% Function reads well logs from a disk file in LAS format (Log ASCII Standard),
% Version 2.0, as specified by the Canadian Well Logging Society.
% Generally somewhat faster than "read_las_file" but may be deprecated in 
% the future.
% See also: read_las_file
%
% Written by: E. Rietsch: February 6, 2000;  
% Last updated: October 10, 2006: Replace "strim" by "strtrim"
%
%          [wlog,las_header]=read_las2_file(filename,iprint)
% INPUT
% filename  string with the file name (and full path if desired);
%         if the file is not found a file selection box will pop up to
%         allow interactive file selection
% iprint  Control diagnostic output (optional)
%         iprint = 0  ==> no output; DEFAULT
%         iprint ~= 0 ==> print out progress of file reading
% OUTPUT
% wlog    structure containing the log curves and ancillary information such as:
%    wlog.type   type of structure; set to 'well_log'
%    wlog.name   name of the file (without extension) from which the log was red 
%    wlog.curves   Matrix of curve values
%         Each log curve is in a column and the first column is usually the depth.
%         Thus, if wlog.curves has n columns, then there are n-1 log curves.
%         If necessary, the rows of curve data are resorted so that
%         depth increases from one row to the next
%         
%    wlog.curve_info Cell array (3 x number of curves) with curve
%         mnemonics, curve units of measurement, and curve descriptions
%    wlog.first	 Start of log (first depth in file)
%    wlog.last   	 End of log (last depth in file)
%    wlog.step	 Depth increment (0 if unequal)
%    wlog.units  Units of measurement for depth
%    wlog.null	 Null value (set to NaN) if there are no-data values
%                otherwise, this field is not set
%    wlog.wellname  Name of well
%    wlog.location  Location of well
%    wlog.company  Oil/gas company which drilled the well
%    wlog.field   Field in which well is located
% 
%         The following components, while desirable, may or may not be present 
%         (depending on LAS file), and others may be present. 
%         They include well identification such as
%    wlog.country Country in which well is located            
%    wlog.wellid	 Well Identifier
%    wlog.api     API number of the well
%    wlog.service Service company which logged the well
%
%          Also included are all parameters in the Parameter Section of the
%          LAS header which have numerical values other than the null value. 
%          Each parameter is stored in a field of the log structure; its 
%          units of measurement and a description are stored in a row of
%          the cell array "wlog.parameter_info" 
%          Examples illustrating such parameters are 
%    wlog.ekb=84	Kelly bushing elevation     
%    wlog.egl=0		Ground level elevation
%    wlog.parameter_info={'ekb','ft','Kelly bushing elevation';
%                         'egl','ft','Ground level elevation'}
% las_header  LAS header of log (provides additional information about the well)

global S4M

run_presets_if_needed

if nargin  <= 1;
   iprint=0;
   if nargin == 0
      filename='';
   end
end  

%  Open the file
if ~isempty(filename)
   fid=fopen(filename,'rt');
   if fid > 0
      filename2S4M(filename)
   end
else
   fid=-1;
end

if fid == -1 
   [filename,ierr]=get_filename4r('las');
   if ierr
      error('No file selected')
   end

   fid=fopen(filename,'rt');
   if fid < 0
      error(['File "',filename,'" could not be opened.'])
   end
end 

lashead=repmat(32,3,100);
nhead=1;

buffer=get_buffer(fid);   
lashead(1,1:length(buffer))=buffer;

while strcmpi(buffer(1),'#')
   buffer=get_buffer(fid);
   nhead=nhead+1;
   lashead(nhead,1:length(buffer))=buffer;
end

if ~strcmpi(buffer(1:2),'~v')
   error('File is not a LAS file');
end

% 	Determine the version of LAS
buffer=get_buffer(fid);
nhead=nhead+1;
lashead(nhead,1:length(buffer))=buffer;
% if length(buffer) > bmax, bmax=length(buffer); end
ind=find(buffer == ':')-1;
ind1=find(buffer == '.')+1;
%vernum=sscanf(buffer(ind1(1):ind),'%*s%f');
vernum=sscanf(buffer(ind1(1):ind),'%f');

%	Check for wrapping
buffer=get_buffer(fid);
nhead=nhead+1;
lashead(nhead,1:length(buffer))=buffer;
% if length(buffer) > bmax, bmax=length(buffer); end
ind=find(buffer == ':')-1;
wrapflag=sscanf(buffer(1:ind),'%*s%s');
if strcmpi(wrapflag,'YES')
   disp(' LAS file is wrapped')
%  disp([mfilename,' cannot read wrapped LAS files'])
%  error('  use REFORMAT.EXE from http://www.cwls.org to unwrap file');
end

if vernum ~= 2.0
   disp([mfilename,' can only read Version 2.0 LAS files'])
   error('  use REFORMAT.EXE from http://www.cwls.org to convert from version 1.2 to 2.0');
else
   [wlog,las_header] = read_las_v20(fid,iprint);
end

las_header=char(strtrim(char(lashead)),las_header);

%       Check log start and end time and step size
if isempty(wlog.curves)
   disp(' No log curve values read; there may be non-numeric characters in the data block')
   error(' Check data section of LAS file')
end

wlog.units=wlog.curve_info{1,2};

if wlog.first ~= wlog.curves(1,1)
   fprintf(['Log start depth (',num2str(wlog.curves(1,1)), ...
      ') differs from header information (',num2str(wlog.first), ...
      '); now corrected\n'])
   wlog.first=wlog.curves(1,1);
end
if wlog.last ~= wlog.curves(end,1)
  fprintf(['Log end depth (',num2str(wlog.curves(end,1)), ...
    ') differs from header information (',num2str(wlog.last), ...
    '); now corrected\n']);
  wlog.last=wlog.curves(end,1);
end
if wlog.step ~= 0
   if ~isconstant(diff(wlog.curves(:,1)),S4M.log_step_error)
      fprintf('Log step size not uniform enough; step size changed to 0\n')
   end
end

%   Make sure that log depths are ascending
if wlog.first > wlog.last
   wlog.curves=flipud(wlog.curves);
   temp=wlog.first;
   wlog.first=wlog.last;
   wlog.last=temp;
   wlog.step=-wlog.step;
end

%  Replace null values by NaNs
idx=find(wlog.curves == wlog.null);
if ~isempty(idx)
   wlog.curves(idx)=NaN;
   wlog.null=NaN;
else
   wlog.null=[];
end

%	Replade 'UNDEFINED' in curve description when it is obvious from the mnemonic
wlog.curve_info=description_substitution(wlog.curve_info);

%       Replace unadmissible curve mnemonics
wlog.curve_info(:,1)=fix_mnemonics(wlog.curve_info(:,1));


%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%

function [wlog,las_header]=read_las_v20(fid,iprint)
% Function reads LAS file version 2.0
% Date Feb. 12, 2000;  written by E. Rietsch
%      Feb. 19, 2000;  updated to handle better missing key words 
%
% INPUT
% fid    File id of LAS file
% iprint  Control diagnostic output (optional)
%        iprint = 0  ==> No output; DEFAULT
%        iprint ~= 0 ==> diagnostic output
%     
% OUTPUT
% wlog      structure contining the log curves and ancillary information
%  wlog.curves   	Matrix of curve values
%		 Each log is in a column and the first column is usually the depth.
%                Thus, if wlog.curves has n columns, then there are n-1 log curves.
%                If necessary, the rows of curve data are resorted so that
%                depth increases from one line to the next
%         
%  wlog.curve_info Cell array (3 x number of curves) with curve mnemonics, 
%                curve units of measurement, and curve descriptions
%  wlog.first	 Start of log (first depth in file)
%  wlog.last	 End of log (last depth in file)
%  wlog.step	 Depth increment (0 if unequal)
%  wlog.null	 Null value (set to NaN)
%  wlog.las_header  LAS header of log (provides additional information about the well)
%  wlog.wellname	 Name of well
%  wlog.location	 Location of well
%  wlog.company   Oil/gas company which drilled the well
%  wlog.field     Field in which well is located
% 
%                The following components, while desirable, may or may not be present 
%                (depending on LAS file), and others may be present. 
%                They include well identification such as
%  wlog.country   Country in which well is located
%  wlog.county    County in which well is located
%  wlog.province  Province in which well is located
%  wlog.state     State in whichwell is located
%  wlog.uwi       Universal well identifier (Canada)            
%  wlog.wellid    Well Identifier
%  wlog.api       API number of the well (USA)
%  wlog.service   Service company which logged the well
%
%                Also included are all parameters in the Parameter Section of the
%                LAS header which have numerical values other than the null value. 
%                Each parameter is stored in a field of the log structure; its 
%                units of measurement and a description are stored in a row of
%                the cell array "wlog.parameter_info" 
%                Examples illustrating such parameters are 
%    wlog.ekb=84	Kelly bushing elevation     
%    wlog.egl=0		Ground level elevation
%    wlog.parameter_info={'ekb','ft','Kelly bushing elevation';
%                         'egl','ft','Ground level elevation'}

global S4M

wlog.type='well_log';
wlog.tag='unspecified';
[dummy,wlog.name]=fileparts(S4M.filename);  %#ok The first output argument 
                                            %    is not required
wlog.from=fullfile(S4M.pathname,S4M.filename);

% 	Allocate space for a 200x100-byte header
nchar=100;
% las_header=32*ones(200,nchar);
las_header=repmat(32,200,nchar);

mnem_length=25;	 	% Maximum length of curve mnemonic

%       Accomodate up to 100 curves and up to 100 parameters
%n_curves=100;
%n_param=100;

if nargin == 1; iprint=0; end
nhead=0;
bmax=0;

buffer=get_buffer(fid);


		while isempty(buffer) || ~strcmpi(buffer(1:2),'~a')

unknown_block=1;  % Used to check if an unknown block is in the file

% 	Well information block
if strcmpi(buffer(1:2),'~w')
  if iprint ~= 0, disp('... reading well info'); end
  unknown_block=0;
  nhead=nhead+1;
  lb=min([length(buffer),nchar]);
  bmax=max([lb,bmax]);
  las_header(nhead,1:lb)=buffer(1:lb);
  
  if iprint ~= 0
     disp('... reading well info'); 
  end
  buffer=get_buffer(fid);
  while buffer(1:1) ~= '~' 
    nhead=nhead+1;
    lb=min([length(buffer),nchar]);
    bmax=max([lb,bmax]);
    las_header(nhead,1:lb)=buffer(1:lb);
    ia=find(buffer == '.')+1;
    if length(ia) > 1
       ia=ia(1);
    end
    ind=find(buffer==':')-1;
    kw=deblank(buffer(1:ia-2));   
		      
%  	Check for start, stop and step 
    if strcmpi(kw,'STRT')
%      if buffer(ia) ~= ' '
       if ~strcmp(buffer(ia),' ')
          temp=sscanf(buffer(ia:ind),'%s',1);
          nu=length(temp);
       else
          nu=0;
       end
       wlog.first = sscanf(buffer(ia+nu:ind),'%f',1);
    end

    if strcmpi(kw,'STOP')
      if ~strcmp(buffer(ia),' ')
%      if buffer(ia) ~= ' '
        temp=sscanf(buffer(ia:ind),'%s',1);
        nu=length(temp);
      else
        nu=0;
      end
      wlog.last = sscanf(buffer(ia+nu:ind), '%f',1);
    end 

    if strcmpi(kw,'STEP')
%      if buffer(ia) ~= ' '
      if ~strcmp(buffer(ia),' ')
        nu=length(temp);
      else
        nu=0;
      end
      wlog.step = sscanf(buffer(ia+nu:ind), '%f');
      wlog.units='';	% Place holder
    end
       
    if strcmpi(kw,'NULL')
       wlog.null = sscanf(buffer(ia:ind), '%f');  
    end

    if strcmpi(kw,'COMP')
       wlog.company = strtrim(buffer(ia:ind));  
    end	
	      
    if strcmpi(kw,'WELL')
       wlog.wellname = strtrim(buffer(ia:ind));
    end

    if strcmpi(kw,'FLD')
       wlog.field = strtrim(buffer(ia:ind));  
    end	
	      
    if strcmpi(kw,'LOC')
       temp=strtrim(buffer(ia:ind));
       if ~isempty(temp), wlog.location = temp; end 
    end	

    if strcmpi(kw,'PROV')
      temp=strtrim(buffer(ia:ind));
      if ~isempty(temp), wlog.province = temp; end 
    end	
	      		      
    if strcmpi(kw,'CTRY')
      temp=strtrim(buffer(ia:ind));
      if ~isempty(temp), wlog.country = temp; end;
    end

    if strcmpi(kw,'CNTY')
      temp=strtrim(buffer(ia:ind));
      if ~isempty(temp), wlog.county = temp; end;
    end

    if strcmpi(kw,'STAT')
      temp=strtrim(buffer(ia:ind));
      if ~isempty(temp), wlog.state = temp; end;
    end

    if strcmpi(kw,'UWI')
      temp=strtrim(buffer(ia:ind));
      if ~isempty(temp), wlog.wellid = temp; end
    end

    if strcmpi(kw,'API')
      temp=strtrim(buffer(ia:ind));
      if ~isempty(temp), wlog.api = temp; end
    end

    if strcmpi(kw,'SRVC')
      temp=strtrim(buffer(ia:ind));
      if ~isempty(temp), wlog.service = temp; end
    end

    if strcmpi(kw,'DATE')
      temp=strtrim(buffer(ia:ind));
      if ~isempty(temp), wlog.date = temp; end 
    end
		      
    buffer=get_buffer(fid);
  end
  if iprint ~= 0, disp('... finished reading well info'); end
end

% 	Parameter information block
if strcmpi(buffer(1:2),'~p') 
  unknown_block=0;
  nhead=nhead+1;
  lb=min([length(buffer),nchar]);
  bmax=max([lb,bmax]);
  las_header(nhead,1:lb)=buffer(1:lb);
  
  if iprint ~= 0, disp('... reading parameter info'); end
  buffer=get_buffer(fid);	   
  while buffer(1:1) ~= '~' 
    nhead=nhead+1; 
    lb=min([length(buffer),nchar]);
    bmax=max([lb,bmax]);
    las_header(nhead,1:lb)=buffer(1:lb);

    if ~strcmpi(buffer(1),'#') 
      ind=find(buffer(1:min([lb-1,21])) == '.')-1; 
      ind=ind(1);
      parameter=lower(strtrim(buffer(1:ind)));
      ind1=find(buffer == ':');

      if ~strcmpi(buffer(ind+2),' ') && ~strcmpi(buffer(ind+2),':')
         unit=sscanf(buffer(ind+2:ind1-1),'%s',1);
         nu=length(unit);
         unit=char(unit_substitution(unit));%  Change abbreviations for units of measurement
      else
         unit='UNDEFINED';
         nu=0;
      end

      value=strtrim(buffer(ind+nu+2:ind1-1));
      temp=strtrim(buffer(ind1+1:end));
      if ~isempty(temp)
	     n=min([60 length(temp)]);
	     descr=temp(1:n);
      else
         descr='UNDEFINED';
      end
      if isnumber(value)
         if str2double(value) ~= wlog.null
            if ~isfield(wlog,'parameter_info')
	           wlog.parameter_info={parameter,unit,descr}; 
            else
	           wlog.parameter_info=[wlog.parameter_info;
	                           {parameter,unit,descr}];
            end
 %           wlog.parameter_info=setfield(wlog.parameter_info,parameter,{unit,descr});
 %           wlog=setfield(wlog,parameter,str2double(value));
            wlog.(parameter)=str2double(value);
         end
      end
    end

    buffer=get_buffer(fid);
  end
%   if isempty(wlog.parameter_info), wlog=rmfield(wlog,'parameter_info'); end

  if iprint ~= 0, disp('... finished reading parameter info'); end

end  

% 	Curve information block
if strcmpi(buffer(1:2),'~c') 
  unknown_block=0;
  nhead=nhead+1;
  lb=min([length(buffer),nchar]);
  bmax=max([lb,bmax]);
  las_header(nhead,1:lb)=buffer(1:lb);
  
  if iprint ~= 0, disp('... reading curve info'); end
  buffer=get_buffer(fid);

  nlogs=0;
  curve_info=cell(3,100);  % Reserve sufficient room for curve information
  while isempty(buffer) || buffer(1:1) ~= '~'
    nhead=nhead+1; 
    lb=min([length(buffer),nchar]);
    bmax=max([lb,bmax]);
    las_header(nhead,1:lb)=buffer(1:lb);
    if  ~strcmp(buffer(1),'#')
      nlogs=nlogs+1;
      ind=find(buffer(1:min(length(buffer),mnem_length+1)) == '.')-1;
      if isempty(ind)
        disp(' No period (dot) found.')
        disp([' Curve mnemonic could be longer than ',num2str(mnem_length),' characters:'])
        disp(buffer)
        fclose(fid);
        error(' Abnormal termination')
      end

%		Get units of measurement
      ind1=find(buffer == ':');
      if ~strcmp(buffer(ind+2),' ')
%       curve_info(2,nlogs)={sscanf(buffer(ind+2:ind1-1),'%s',1)};
        temp=strtrim(buffer(ind+2:ind1-1));
        idx=findstr(temp,'  ');
        if ~isempty(idx)  % Are there blanks in the unit description?
          idx1=find(diff(idx)==1);
          if isempty(idx1)
             curve_info{2,nlogs}=temp;
          else
             curve_info{2,nlogs}=temp(1:idx(idx1(1))-1);
          end
        else
          curve_info{2,nlogs}=temp;
        end
      else
        curve_info{2,nlogs}='UNDEFINED';
      end

%		Get description
      ind2=find(buffer(ind1+1:end) > 64);
      if ~isempty(ind2)
        curve_info{3,nlogs}=buffer(ind1+ind2(1):end);
      else
        curve_info{3,nlogs}='UNDEFINED';
      end

      curve_info{1,nlogs}=deblank(buffer(1:ind(1)));
    end
    buffer=get_buffer(fid);
  end

  curve_info=curve_info(:,1:nlogs);  % Release unused, reserved space

  if iprint ~= 0, disp('... finished reading curve info'); end
end  

% 	Other information block
if strcmpi(buffer(1:2),'~o') 
  unknown_block=0;
  nhead=nhead+1;
  lb=min([length(buffer),nchar]);
  bmax=max([lb,bmax]);
  las_header(nhead,1:lb)=buffer(1:lb);
  if iprint ~= 0, disp('... reading other information'); end
  buffer=deblank(fgetl(fid));
  nhead=nhead+1;
  lb=min([length(buffer),nchar]);
  bmax=max([lb,bmax]);
  las_header(nhead,1:lb)=buffer(1:lb);

  while isempty(buffer) || buffer(1:1) ~= '~' 
    nhead=nhead+1;
    lb=min([length(buffer),nchar]);
    bmax=max([lb,bmax]);
    las_header(nhead,1:lb)=buffer(1:lb); 
    buffer=deblank(fgetl(fid)); 
  end

  if iprint ~= 0, disp('... finished reading other information'); end
end       

if unknown_block == 1     	% Unknown block encountered: 
				% Write it to header
   nhead=nhead+1;
   lb=min([length(buffer)-1,nchar]);
   bmax=max([lb,bmax]);
   las_header(nhead,1:lb)=buffer(1:lb); 
   buffer=deblank(fgetl(fid)); 
   while  ~isempty(buffer) && buffer(1:1) ~= '~'
      nhead=nhead+1;
      lb=min([length(buffer),nchar]);
      bmax=max([lb,bmax]);
      las_header(nhead,1:lb)=buffer(1:lb); 
      buffer=get_buffer(fid); 
   end
end                        
		end      % Last block (log curves) reached

nhead=nhead+1;
lb=min([length(buffer),nchar]);
% bmax=max([lb,bmax]);
las_header(nhead,1:lb)=buffer(1:lb);
las_header=deblank(char(las_header(1:nhead,:)));

%	ASCI log curves
if iprint ~= 0, disp('... reading ASCII curve values'); end
wlog.curves=fscanf(fid,'%g',[nlogs,inf])';
if iprint ~= 0, disp('... finished reading ASCII curve values'); end

fclose(fid);

wlog.curve_info=curve_info';

if strcmpi(curve_info{1,1},'dept')
   wlog.curve_info{1,1}='DEPTH';
end

%	Change abbreviations for units of measurement
for ii=1:nlogs
  wlog.curve_info(ii,2)=unit_substitution(wlog.curve_info(ii,2));
end

%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%

function buffer=get_buffer(fid)
% Get next line in file
% INPUT
% fid     file identifier
% OUTPUT
% buffer  line of text

buffer=strtrim(fgetl(fid));
while isempty(buffer)
   buffer=strtrim(fgetl(fid));
end

if length(buffer == 1)   %#ok
   buffer=[buffer,' '];
end
