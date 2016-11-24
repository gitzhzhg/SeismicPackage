function seismic=s_header_int2double(seismic,header,fpformat)
% Convert the bit pattern of integers in a header to floating point numbers. 
% (used in case header values in an SEG-Y file should have been read as  
% floating-point numbers)
%
% Background: By default (and following the SEG-Y standard), function 
% "read_segy_file" reads trace headers as two-byte or 4-byte integers. 
% However, some programs --- such as ProMAX --- allow users to put floating-point 
% numbers into the headers. Thus the values of these headers are read
% incorrectly. This function converts header values read as integers to the
% values they would have had they been read as floating point numbers.
% 
% Written by: E. Rietsch: June 30, 2005
% Last updated:
%
%           seismic=s_header_int2double(seismic,header,fpformat)
% INPUT
% seismic   seismic data set
% header    mnemonic of header to convert
% fpformat  string with floating-point format (possible values are 'ibm' and 'ieee')
% OUTPUT
% seismic   seismic with converted header

%       Find header to convert
index=header_index1(seismic,header);
temp=seismic.headers(index,:);

%       Write header to temporary file
tempfile=fullfile(tempdir,'tempconversion');
fid=fopen(tempfile,'w');
fwrite(fid,temp,'int32');
fclose(fid);

%       Read temporary file
fid=fopen(tempfile,'r');

if strcmpi(fpformat,'ibm')
   temp=ibm2ieee(fread(fid,inf,'uint'));

elseif strcmpi(fpformat,'ieee')
   temp=fread(fid,inf,'float32');

else
   error('Unknown floating point format')
end


seismic.headers(index,:)=temp;

%       Close temporary file
fclose(fid);

try
   if ispc
      [ier,msg]=dos(['del ',tempfile]); %#ok Only used for debugging
   else
      ier=unix(['rm -f ',tempfile]);    %#ok Only used for debugging
   end
catch
end

