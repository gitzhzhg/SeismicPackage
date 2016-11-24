function [zmd,azmi,incl,utmx,utmy,utmzone]=readtvd(filename)
% [zmd,azmi,incl,utmx,utmy]=readtvd(filename)
%
% Read a well bore deviation file survey. The file is
% defined as consisting of any number of headers followed
% by three columns of ascii numbers: measured_depth,
% azimuth, inclination. Headers are denoted by // in the
% first two columns.
%
% Note, the first header line is examined for well surface
% location information. If found, these are returned as
% utmx, utmy and utmzone
%
% NOTE: This SOFTWARE may be used by any individual or corporation for any purpose
% with the exception of re-selling or re-distributing the SOFTWARE.
% By using this software, you are agreeing to the terms detailed in this software's
% Matlab source file.

% BEGIN TERMS OF USE LICENSE
%
% This SOFTWARE is maintained by the CREWES Project at the Department
% of Geology and Geophysics of the University of Calgary, Calgary,
% Alberta, Canada.  The copyright and ownership is jointly held by
% its 'AUTHOR' (identified above) and the CREWES Project.  The CREWES
% project may be contacted via email at:  crewesinfo@crewes.org
%
% The term 'SOFTWARE' refers to the Matlab source code, translations to
% any other computer language, or object code
%
% Terms of use of this SOFTWARE
%
% 1) This SOFTWARE may be used by any individual or corporation for any purpose
%    with the exception of re-selling or re-distributing the SOFTWARE.
%
% 2) The AUTHOR and CREWES must be acknowledged in any resulting publications or
%    presentations
%
% 3) This SOFTWARE is provided "as is" with no warranty of any kind
%    either expressed or implied. CREWES makes no warranties or representation
%    as to its accuracy, completeness, or fitness for any purpose. CREWES
%    is under no obligation to provide support of any kind for this SOFTWARE.
%
% 4) CREWES periodically adds, changes, improves or updates this SOFTWARE without
%    notice. New versions will be made available at www.crewes.org .
%
% 5) Use this SOFTWARE at your own risk.
%
% END TERMS OF USE LICENSE
%open the file
fid=fopen(filename);
rec=fgetl(fid);
utmx=0;
utmy=0;
utmzone=0;
%read past comments
while( strcmp(rec(1:2),'//') )
 %look for location info
 ind=findstr(rec,'X=');
 if(~isempty(ind))
		ind2=findstr(rec,'Y=');
		ind3=findstr(rec,'ZONE=');
		utmx=sscanf(rec(ind+2:length(rec)),'%lg');
		utmy=sscanf(rec(ind2+2:length(rec)),'%lg');
		utmzone=sscanf(rec(ind3+5:length(rec)),'%d');
	end
	rec=fgetl(fid);
end
%load the rest of the file
dat=fscanf(fid,'%g',[3,inf]);
fclose(fid);
dat=dat.';
zmd=dat(:,1);
azmi=dat(:,2);
incl=dat(:,3);