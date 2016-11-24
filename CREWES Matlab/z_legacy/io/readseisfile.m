function [LINE,TR,XTR,YTR,ZTR1,ZTR2,ZTR3,ZTR4,ZTR5,ZTR6,index,com,ncom]=readseisfile(filename)
%   readseisfile.m, shell for readseisfile2
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

% get the size of the file
str = ['wc ',filename];
[wcstat,wcstr] = unix(str);
wc = str2num(sscanf(wcstr,'%s',1));
disp('disregard stty: warning message above ');
disp('count lines, count words, count characters, filename =');
disp(wcstr);
[LINETMP,TR,XTR,YTR,ZTR1,ZTR2,ZTR3,ZTR4,ZTR5,ZTR6,index,comtmp,ncom]=readseisfile2(filename,wc);
ntrace=wc-ncom;
LINE = reshape(LINETMP,12,ntrace)';
comtmp2 = comtmp(:,1:40)';
com = reshape(comtmp2,40,ncom)';
ind = find(com==10);
com(ind) = 32*ones(size(ind));
com = setstr(com);
if( ~isempty(ZTR1) )
	ind = find(ZTR1==0);
	ZTR1(ind) = NaN*ones(size(ind));
end
if( ~isempty(ZTR2) )
	ind = find(ZTR2==0);
	ZTR2(ind) = NaN*ones(size(ind));
end
if( ~isempty(ZTR3) )
	ind = find(ZTR3==0);
	ZTR3(ind) = NaN*ones(size(ind));
end
if( ~isempty(ZTR4) )
	ind = find(ZTR4==0);
	ZTR4(ind) = NaN*ones(size(ind));
end
if( ~isempty(ZTR5) )
	ind = find(ZTR5==0);
	ZTR5(ind) = NaN*ones(size(ind));
end
if( ~isempty(ZTR6) )
	ind = find(ZTR6==0);
	ZTR6(ind) = NaN*ones(size(ind));
end
TR=TR(1:ntrace);
XTR=XTR(1:ntrace);
YTR=YTR(1:ntrace);
ZTR1=ZTR1(1:ntrace);
ZTR2=ZTR2(1:ntrace);
ZTR3=ZTR3(1:ntrace);
ZTR4=ZTR4(1:ntrace);
ZTR5=ZTR5(1:ntrace);
ZTR6=ZTR6(1:ntrace);
index=index(1:ntrace);