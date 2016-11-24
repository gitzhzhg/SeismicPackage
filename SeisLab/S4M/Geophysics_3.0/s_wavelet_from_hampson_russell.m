function [seismic,header]=s_wavelet_from_hampson_russell(filename)
% Read wavelet in Hampson-Russell format from ASCII file
%
% Written by: E. Rietsch: October 18, 2005
% Last updated: April 23, 2007: Handle single-precision and null value
%
%           [seismic,header]=s_wavelet_from_hampson_russell(filename)
% INPUT
% filename  file name (optional)
%           the filename and the directory are saved in global variable S4M
% OUTPUT
% seismic   seismic data set read from file
% header    text header of Hampson-Russell file

global ABORTED S4M

ABORTED=true;
seismic=[];
header=[];


if nargin == 0
   [fid,filename]=open_file('rt');
else
   [fid,filename]=open_file('rt',filename);
end

if fid < 0
   return
end

header=cell(25,1);

line=deblank(fgetl(fid));

if length(line) > 3  &&  ~strcmp(line(1:3),'~SR')
%       Read header
   ik=0;

   try
      while length(line) < 15  || ~strcmp(line(1:15),'#STRATA_WPARAMS')
         ik=ik+1;
         header{ik}=line;
         line=fgetl(fid);
      end

   catch
      disp('Problem reading file')
      msgdlg({['Problem encountered when trying to read file "',selected_file,'".']; ...
      'File is probably not in Hampson-Russell format.'})
      return
   end
   header=header(1:ik);
   line=fgetl(fid);
end

%       Read wavelet parameters
ierr=0;

if strcmp(line(1:3),'~SR')
   step=str2double(line(4:end));
else
   ierr=1;
end


line=fgetl(fid);
if strcmp(line(1:3),'~TZ')
   nt0=str2double(line(4:end));
else
   ierr=1;
end

line=fgetl(fid);
if strcmp(line(1:3),'~NS')
   nsamp=str2double(line(4:end));
else
   ierr=1;
end

line=fgetl(fid);
if strcmp(line(1:3),'~PR') ||  strcmp(line(1:3),'~RP')  % Second option is a fix 
                                       % to handle an aberration from H-R format
   pr=str2double(line(4:end));         %#ok Available in file but not used
else
   ierr=1;
end

if ierr > 0
   fclose(fid)
   disp('File is not in Hampson-Russell format.')
   warndlg('File is not in Hampson-Russell format!')
   ABORTED =true;
   seismic=[];
   return
end

%       Prepare seismic structure
seismic.type='seismic';
seismic.tag='wavelet';
[dummy,name]=fileparts(filename);   %#ok  First output argument not required
seismic.name=name;
seismic.first=(1-nt0)*step;
seismic.last=seismic.first+(nsamp-1)*step;
seismic.step=step;
seismic.units='ms';

%	Read the first trace-data line to determine the number of columns
seismic.traces=fscanf(fid,'%g',[1,nsamp])';

seismic.null=[];

fclose(fid);

if S4M.history
   seismic=s_history(seismic,'add',['File ',seismic.name,' from Hampson-Russell']);
end

if strcmpi(S4M.precision,'single');
   seismic=single(seismic);
end

ABORTED=false;
