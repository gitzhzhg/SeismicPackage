function [zcs,tcs]=readcheckshot(filename)
% [zcs,tcs]=readcheckshot(filename)
%
% Read in checkshot information from a standar ascii file. The file is
% defined as one with an arbitrary number of header lines containing any
% information followed by two columns of numbers: the first being depth
% and the second time. The header lines must begin with two slashes (//)
% but are otherwise arbitrary and are ignored by this function. The times
% may be one-way or two-way since CSCORRDISP is designed to handle both.
% Time will be output in seconds. If the time in the file contain any values
% greater than 10.0, they are assumed to be milliseconds and converted.
%
% G.F. Margrave November 1994
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
fid=fopen(filename);
if(fid==-1)
	zcs=-1;
	tcs=[];
	return;
end
%read past the header
p1=0;
buf=fgetl(fid);
p2=ftell(fid);
while(strcmp(buf(1:2),'//'))
	p1=p2;
	buf=fgetl(fid);
	p2=ftell(fid);
end
%reposition the file pointer
fseek(fid,p1,-1);
%read the data
dat=fscanf(fid,'%g');
zcs=dat(1:2:length(dat));
tcs=dat(2:2:length(dat));
zcs=zcs(:);
tcs=tcs(:);
tmax=max(tcs);
if(tmax>10)
	tcs=tcs/1000.;
end
fclose(fid);