function [w,t,wname]=readwavelet(filename)
% [w,t,wname]=readwavelet(filename)
%
% Reads a wavelet from disk in either 2 column headerless format or
% in GMA format
% Any kind of a read failure (no file, empty file etc) is flagged by 
% returning w=-1
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
	w=-1;
	t=-1;
    wname='';
%open the file
fid=fopen(filename);
if(fid==-1)
	return;
end
%read the first line
rec=fgetl(fid);
if( ~isempty(rec) )
	if( rec==-1 )
		return;
	end
end
%test for gma file
gma=0;
if( length( rec) > 8 )
	if( strcmp(rec(2:8),'WAVELET') )
		gma=1;
	end
end
% read a gma file
if(gma)
	%form a name from record 1
	%wname=sscanf(rec,'%*s %*s %s %*f %*s %*f %*s %*f %*s %*s %*s');
	wname=deblank(rec(12:40));
	ind=find(abs(wname==32));
	if(~isempty(ind))
		wname=wname(1:ind(1)-1);
	end
	wname=[wname '_logm'];
	%next record
	rec=fgetl(fid);
	%read sample rate, start & end times
	dt=sscanf(rec,'%*s %*s %*s %f %*s %*s %*s %*f %*s %*s %*s %*f %*s');
 tmin=sscanf(rec,'%*s %*s %*s %*f %*s %*s %*s %f %*s %*s %*s %*f %*s');
 tmax=sscanf(rec,'%*s %*s %*s %*f %*s %*s %*s %*f %*s %*s %*s %f %*s');
 t=tmin:dt:tmax;
 t=t'/1000;
 %skip two lines
 rec=fgetl(fid);
 rec=fgetl(fid);
 %read in the amplitudes
 w=fscanf(fid,'%g',inf);
 return;
else
 %we assume it contains just two columns of numbers
 frewind(fid);
 dat=fscanf(fid,'%g %g',[2,inf]);
 if(isempty(dat))
		return;
	end
 t=dat(1,:)';
 w=dat(2,:)';
 wname='nameless';
 return;
end