function s_wavelet4hampson_russell(wavelet,filename)
% Write a wavelet to an ASCII file in Hampson-Russell format
%
% Written by: E. Rietsch: July 20, 2005
% Last updated: December 18, 2006: Use function "open_file"
%
%             s_wavelet4hampson_russell(wavelet,filename)
% INPUT
% wavelet     wavelet
% filename    optional filename including path; if not given the filename 
%             can be selected interactively

global S4M WF

if nargin < 2
   filename='*.txt';
end

[fid,filename]=open_file('wt',filename);

if fid < 0
   if ~isdeployed
      error(' Abnormal termination.')
   end
end

%       Line-feed character
if ispc
   linefeed=char(10);
else
   linefeed=char(13);
end

%       Write file description
fprintf(fid,'%s',['Wavelet in Hampson-Russell ASCII format.',linefeed]);

if ~isempty(WF)
   try
      fprintf(fid,'%s',['Created by SeisLab work flow "',S4M.name,'" (',S4M.time,')',linefeed]);
   catch
   end
end

if ~isempty(S4M.script)
   try
   fprintf(fid,'%s',['Created by SeisLab script "',S4M.script,'" (',S4M.time,')',linefeed]);
   catch
   end
end


        try
fprintf(fid,'%s',['#STRATA_WPARAMS',linefeed]);

fprintf(fid,'%s',['~SR ',num2str(wavelet.step),linefeed]);

tz=(-wavelet.first/wavelet.step)+1;
fprintf(fid,'%s',['~TZ ',num2str(tz),linefeed]);

nsamp=size(wavelet.traces,1);
fprintf(fid,'%s',['~NS ',num2str(nsamp),linefeed]);

fprintf(fid,'%s',['~PR ','0',linefeed]);

format='%10.6g %s';
for ii=1:nsamp
   fprintf(fid,format,wavelet.traces(ii));
   fprintf(fid,linefeed); 
end

        catch
ple
if ~isdeployed
   disp(['Error writing file "',filename,'"'])
else
   warndlg(['File "',filename,'" could not be written.'])
end
        end

fclose(fid);
