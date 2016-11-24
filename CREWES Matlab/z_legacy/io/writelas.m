function writelas(fullfilename,lasheader,logmat,z)

% writelas(fullfilename,lasheader,logmat,z)
% writelas(fullfilename,lasheader,logmat)
%
% WRITELAS writes and LAS (Log ASCII Standard) dataset to disk.
%
% fullfilename ... a string containing the fully qualified filename
% lasheader ... a string matrix containing as las header which
%		matches the data in z and logmat. Two easy ways to get this
%		are 1) from READLAS or 2) from MAKELASHEADER
% logmat ... matrix of logs, one per column. Note that since LAS
%	does not support NaN's, all NaNs in the logmatrix must be set
%	to a NULL value consistent with that in the LAS header.
% z ... vector of depths
%
% NOTE: if Z is not supplied then it is assumed that the first column
%  of logmat containes the depths. If z is supplied, the it is
%  prepended to logmat as column 1.
%
% NOTE: number of rows in logmat must be the same as the length
% of z
%
% G.F. Margrave
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

if( nargin==4 )
	logmat=[z(:) logmat];
end

%test for computer. Special method for mac
comp=computer;
if(strcmp(comp,'MAC2')|strncmp(comp,'PCWIN',5))
	fid=fopen(fullfilename,'w');
	if(fid==-1)
	   msgbox('Cannot open output file. Perhaps it is marked as read only?')
	   return
	end
	[nlines,nchar]=size(lasheader);
	for kline=1:nlines
		fprintf(fid,'%s\r\n',lasheader(kline,:));
	end
	%now build format string for the logmat
	[ns,nlogs]=size(logmat);
	fmtstr='';
	for k=1:nlogs
		fmtstr=[fmtstr '%13.7e   '];
	end
	fmtstr=[fmtstr '\r\n'];
	
	%write out the logmat
	fprintf(fid,fmtstr,logmat');
	
	fclose(fid);
else
	fid=fopen(fullfilename,'w');
	[nlines,nchar]=size(lasheader);
	for kline=1:nlines
		fprintf(fid,'%s\r\n',lasheader(kline,:));
	end
	fclose(fid);
	tmpname=['/tmp/junk' num2str(fix(clock)) '.tmp'];
	indies=find(tmpname==' ');
	tmpname(indies)='';
	eval(['save ' tmpname ' logmat -ascii']);
	unixcmd = ['cat ' tmpname ' >> ' fullfilename];
	unix(unixcmd);
end

disp(['file ' fullfilename ' made ']);