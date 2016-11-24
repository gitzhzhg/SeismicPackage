function [logmat,logmnem,logdesc,wellname,wellid,loc,nullval,dpthunits,...
		kb,tops,ztops,lasheader]=readlas(filein,errflag)
% READLAS: read LAS well log files
%
% [logmat,mnem,desc,name,id,loc,null,units,kb,tops,ztops,lash]=readlas(filein)
%
% READLAS reads well logs from a disk file in LAS (Log ASCII Standard) format
%
% filein ... string with the file name (and full path if desired)
% errflag ... flag indicating error behavior. if errflag == 0, then if an
% error occurs during reading, this function will abort calling MATLABS
% ERROR function. If errflag==1 and an error occurs, the function will
% return normally with logmat set to -1 and all other values [].
% *************** default = 0 *********************
%
% logmat ... matrix containing the logs in the file. Each log is in a column 
% and the first column contains the depths. Thus, if logmat has n columns,
% then there are n-1 logs
% mnem ...matrix of standard 4 letter mnemonics of the n columns of logmat
% desc ... matrix of string descriptors of the n columns of the logmat
% name ... string with the well name
% id ... the unique well id
% loc ... string containg the well location
% null ... the null value used in the logs
% units ... string indicating the depth units of the log
% kb ... elevation of the kelly bushing
% tops ... names of tops found in the file
% ztops ... depths of tops
% lash ... matlab string matrix containing the entire las header
%
% G.F. Margrave, May 1994
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

% initilize variables
logmat=[]; logmnem=[]; logdesc=[]; wellname=[]; wellid=[]; loc=[]; nullval=[]; 
dpthunits=[]; kb=[]; tops=[] ;ztops=[] ;lasheader=[] ;

if (nargin==1)
    errflag=0;
end
 
fid = fopen(filein); 
lashead=32*ones(3,100);
bmax=0;

buffer=fgetl(fid);
 
ncomments=0;
if(strcmp(buffer(1),'#')==1)   
    while(strcmp(buffer(1),'#')==1)
    ncomments=ncomments+1; 
         lashead(ncomments,1:length(buffer))=buffer;
         if(length(buffer)>bmax) 
             bmax=length(buffer);
         end   
         buffer=fgetl(fid);
    end
end
 
lashead(ncomments+1,1:length(buffer))=buffer;
if(length(buffer)>bmax) 
     bmax=length(buffer); 
end  
 
if(~strcmp(lower(buffer(1:2)),'~v'))
   if(errflag)
      logmat=-1;
      return;
   else
       disp('File is not an LAS file');
       return;
   end
end

% Check the necessary ~ flags are present 
if(buffer(1) == '~') 
   if( lower(buffer(2)) == 'v')
      section_v = 'OK'; 
   end
end 

% determine the version of las
buffer=fgetl(fid);
%test for blank line
while(isempty(deblank(buffer)))
   buffer=fgetl(fid);
end
lashead(ncomments+2,1:length(buffer))=buffer;
if (length(buffer)>bmax) 
    bmax=length(buffer); 
end
ind=find(buffer==':')-1;
vernum=sscanf(buffer(1:ind),'%*s%f');

if vernum > 2 
    errordlg('Only LAS version 1 and 2 files are currently supported.','Incompatible LAS file');
    return;
end

%check for wrapping
buffer=fgetl(fid);
%test for blank line
while(isempty(deblank(buffer)))
    buffer=fgetl(fid);
end
lashead(ncomments+3,1:length(buffer))=buffer;
if(length(buffer)>bmax) bmax=length(buffer); end
ind=find(buffer==':')-1;
wrapflag=sscanf(buffer(1:ind),'%*s%s');
if(strcmp(wrapflag,'YES'))
	errordlg('readlas cannot read wrapped las files');
end

%Always read the version 2 reader. The formats should be backwards compatible and
%the version 2 reader should tolerate a version 1 file

[logmat,logmnem,logdesc,wellname,wellid,loc,nullval,dpthunits, ...
			kb,tops,ztops,lasheader]= readlasv2(fid,buffer);
        
% [logmat,logmnem,logdesc,wellname,wellid,loc,nullval,dpthunits, ...
% 			kb,tops,ztops,lasheader]= readlas_new(fid,buffer);

%fix up the header
[nr,nc]=size(lasheader);
if( bmax<= nc)
    if(nc>100)
        lashead=[lashead setstr(32*ones(size(lashead,1),nc-100))];
	end
	lasheader=[lashead(:,1:nc);lasheader];
else
	lasheader=[lasheader 32*ones(nr,bmax-nc)];
	lasheader=[lashead(:,1:bmax);lasheader];
end

fclose(fid); 
wellname=char(wellname);
loc=char(loc);
wellid=char(wellid);
logmnem=char(logmnem);
logdesc=char(logdesc);
tops=char(tops);
lasheader=char(lasheader);