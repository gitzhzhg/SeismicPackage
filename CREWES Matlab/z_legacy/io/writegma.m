function flag=writegma(filename,logname,z,samps,units,kb,topnames,...
		ztops)
% flag=writegma(filename,logname,z,samps,units,kb,topnames,...
%				ztops)
%
% Write a log to disk in GMA format
% filename ... string containing the file name
% logname ... string containing the logname. If longer than 16
%	characters, it will be truncated.
% z ... vector of depths for the log
% samps ... vector of log samples. (z and samps must be the same
%		length)
% units ... Units flag. Use 'M' for metric and 'F' for imperial
% kb ... kelly bushing elevation
%	******* default = 0.0 ******
% topnames ... string matrix of top names. One name per row. 
%	Maximum name length is 30 (16 in LOGM)
%  ******** default is no tops *******
% ztops ... vector of formation top depths. Must be one per
%	topname.
%  ******** default is no tops *******
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
if(nargin<6)
	kb=0.0;
end
if(nargin<7)
	topnames=[];
	ztops=[];
end
%open the file
fid= fopen(filename,'wt');
if(fid==-1)
	flag=fid;
	error('Unable to open output file')
end
rec=blanks(80);
if(length(logname)>16) logname=logname(1:16); end
rec(1:length(logname))=logname;
zbeg=z(1);zend=z(length(z));dz=z(2)-z(1);
s=sprintf('%7.1f',zbeg);
if(length(s)>7) s=s(1:7); end
rec(29:28+length(s))=s;
s=sprintf('%7.1f',zend);
if(length(s)>7) s=s(1:7); end
rec(36:35+length(s))=s;
s=sprintf('%6.4f',dz);
if(length(s)>6) s=s(1:6); end
rec(47:46+length(s))=s;
s=sprintf('%8.0f',length(z));
rec(54:53+length(s))=s;
rec(70)=units;
%write first record
fprintf(fid,'%s\n',rec);
%loop to write out samples
rec=blanks(80);
inum=1;
nz=length(z);
while(inum < nz)
	rec(1:7)=sprintf('%7.1f',z(inum));
	nnums=min([10 nz-inum+1]);
	rec(8:7+nnums*7)=sprintf('%7.1f',samps(inum:inum+nnums-1));
	rec(78:80)=sprintf('%3d',nnums);
	fprintf(fid,'%s\n',rec);
	rec=blanks(80);
	inum=inum+nnums;
end
	
%write out tops
ntops=size(topnames,1);
if(ntops)
	rec=sprintf('#TOPS#%5d',ntops);
	fprintf(fid,'%s\n',rec);
	for k=1:ntops
		name=strunpad(topnames(k,:));
		if(length(name)>30) name=name(1:30); end
		rec=blanks(40);
		rec(1:length(name))=sprintf('%s',name);
		rec(33:39)=sprintf('%7.1f',ztops(k));
		fprintf(fid,'%s\n',rec);
	end
end
	%kb
	rec=sprintf('KB%7.1f',kb);
	fprintf(fid,'%s\n',rec);