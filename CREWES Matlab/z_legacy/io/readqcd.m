function [logname,z,samps,topnames,ztops,kb,units]=readqcd(filename)
% [logname,z,samps,topnames,ztops,kb,units]=readqcd(filename)
%
% READGMA reads a well log and formation tops (if any) from a diskfile 
% which is assumed to be in QCD ascii format.
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
if( fid == -1 )
   errst = sprintf('Could not open file %s',filename);
   error(errst);
end
%read the first line
rec=fgetl(fid);
if(length(rec)<59)
	logname=-1;
	return;
end
% get the log name
logname = sscanf(rec(1:28),'%s');
% start depth, end depth, sample rate nsamP
zbeg=sscanf(rec(29:35),'%f');
zend=sscanf(rec(36:43),'%f');
dz=sscanf(rec(46:52),'%f');
%dat=sscanf(rec(29:52),'%f');
%zbeg=dat(1);
%zend=dat(2);
%dz=dat(3);
nz=sscanf(rec(53:59),'%f');
if(isempty(nz));
	nz=20000;
end
%units flag
if(length(rec)>=70)
	units=rec(70);
	if(abs(units)==32)
		units='M';
	end
else
	units='M';
end
ntest=(zend-zbeg)/dz;
if( ntest < 0 | isinf(ntest) | isnan(ntest) | ntest > 100000.)
	logname=-1;
	return;
end
%grab some memory
z=nan*ones(nz+10,1);
samps=z;
%loop to read in samples
if( nz~=20000 )
	disp([int2str(nz) ' samples to read'])
else
	disp('reading samples')
end
rec=fgetl(fid);
ntops=0;
ksamp=0;
while( ksamp<nz & rec ~= -1 )
 %test for enpty line
 if(~isempty(rec))
		% test for onset of tops
		if( strcmp(rec(1:6),'#TOPS#') )
			ntops=sscanf(rec(7:length(rec)),'%d');
			break;
		end
		%test for kb. This is always at the end of the file
		if(strcmp( rec(1:2),'KB') )
			kb=sscanf(rec(3:length(rec)),'%f');
			return;
		end
		% get number of entries on line
		num=sscanf(rec(78:80),'%d');
		znot=sscanf(rec(1:7),'%f');
		for k=1:num
			ksamp=ksamp+1;
			z(ksamp)=znot+(k-1)*dz;
			samps(ksamp)=sscanf(rec(8+(k-1)*7:14+(k-1)*7),'%f');
		end
		if( rem(ksamp,500)==0 )
			disp([int2str(ksamp) ' samples read']);
		end
	end
rec=fgetl(fid);
end
%free up unneeded memory
ind=find(~isnan(z));
z=z(ind);
samps=samps(ind);
if( ~ntops )
	if(rec==-1)
		return;
	else
		%look for tops
		while( rec~= -1)
			if(~isempty(rec))
				if( strcmp(rec(1:6),'#TOPS#') )
					ntops=sscanf(rec(7:length(rec)),'%d');
					break;
				end
				if( strcmp(rec(1:2),'KB') )
					kb=sscanf(rec(3:length(rec)),'%f');
					return;
				end
			end
			rec=fgetl(fid);
		end
	end
end
if( ntops )
	topnames=ones(ntops,30);
	ztops=nan*zeros(ntops,1);
	ktop=0;
	while(ktop < ntops )
		rec=fgetl(fid);
		if(~isempty(rec))
			ktop=ktop+1;
			name=sscanf(rec(1:32),'%s');
			n=min([30 length(name)]);
			topnames(ktop,1:n)=name;
			ztops(ktop)=sscanf(rec(33:length(rec)),'%f');
		end
	end
end
%attempt to read kb
rec=fgetl(fid);
while(rec~=-1)
	if(~isempty(rec))
		if( strcmp(rec(1:2),'KB') )
			kb=sscanf(rec(3:length(rec)),'%f');
		end
	end
	rec=fgetl(fid);
end
fclose(fid);