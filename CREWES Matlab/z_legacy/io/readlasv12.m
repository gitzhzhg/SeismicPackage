function [logmat,logmnem,logdesc,wellname,wellid,loc,nullval,dpthunits,...
		kb,tops,ztops,lasheader]=readlasv12(fid,buffer)
%
% [logmat,logmnem,logdesc,wellname,wellid,loc,nullval,dpthunits,...
%		kb,tops,ztops,lasheader]=readlasv12(filename)
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

logmat=[];logmnem=[];logdesc=[];wellname=[];wellid=[];loc=[];nullval=[];
dpthunits=[];kb=[];tops=[];ztops=[];lasheader=[];

%allocate a 150x100 header
	lasheader=32*ones(150,100);
	nhead=0;
	bmax=0;

 while (~strcmp(buffer(1:2),'~A'))

		%read well information block
		if( strcmp(buffer(1:2),'~W') )
			nhead=nhead+1;
			if(length(buffer)>bmax) bmax=length(buffer); end
			lasheader(nhead,1:length(buffer))=buffer;

			disp('reading well info');
			buffer=fgetl(fid);

			while( buffer(1:1)~='~' )
				nhead=nhead+1;
				if(length(buffer)>bmax) bmax=length(buffer); end
				lasheader(nhead,1:length(buffer))=buffer;

				if(buffer(1,1)==' ')
					ichk=2:5;
				else
					ichk=1:4;
				end

				 %  Check for start, stop and step 
				if(strcmp(buffer(ichk),'STRT') == 1)                 
					 ind=find(buffer==':')-1;
					 start = sscanf(buffer(1:ind), '%*s%f');  
				end
				if(strcmp(buffer(ichk),'STOP') == 1)                 
					 ind=find(buffer==':')-1;
					 stop = sscanf(buffer(1:ind), '%*s%f');  
				end 
				if(strcmp(buffer(ichk),'STEP') == 1)                 
					 ind=find(buffer==':')-1;
					 step = sscanf(buffer(1:ind), '%*s%f');  
				end
				if(strcmp(buffer(ichk),'NULL') == 1)                 
					 ind=find(buffer==':')-1;
					 nullval = sscanf(buffer(1:ind), '%*s%f');  
				end

				if(strcmp(buffer(ichk),'WELL') == 1)                 
					 ind=find(buffer==':')+1;
					 wellname = buffer(ind:length(buffer));
					 ind=find(abs(wellname)~=32);
					 wellname=wellname(1:ind(length(ind)));
				end

				if(strcmp(buffer(ichk),'LOC ') == 1)                 
					 ind=find(buffer==':')+1;
					 loc = sscanf(buffer(ind:length(buffer)),'%s');  
				end

				if(strcmp(buffer(ichk),'UWI ') == 1)                 
					 ind=find(buffer==':')+1;
					 wellid = sscanf(buffer(ind:length(buffer)), '%s');  
				end

				buffer=[];
				while(isempty(buffer))
					buffer=fgetl(fid);
				end
			end
			disp('finished reading well info');
		end

		%Parameter information block
		if( strcmp(buffer(1:2),'~P') )
			nhead=nhead+1;
			if(length(buffer)>bmax) bmax=length(buffer); end
			lasheader(nhead,1:length(buffer))=buffer;

			disp('reading parameter info');
			buffer=fgetl(fid); 
			while( buffer(1:1)~= '~' )
				nhead=nhead+1;
				if(length(buffer)>bmax) bmax=length(buffer); end
				lasheader(nhead,1:length(buffer))=buffer;

				if(buffer(1,1)==' ')
					ichk=2:5;
					ichk2=2:4;
				else
					ichk=1:4;
					ichk2=1:3;
				end
				
				if(strcmp(buffer(ichk2),'EKB'))
					ind=find(buffer==':')-1;
					kb=sscanf(buffer(11:ind),'%f');
				end
				
				if(strcmp(buffer(ichk),'EREF'))
					ind=find(buffer==':')-1;
					kb=sscanf(buffer(11:ind),'%f');
				end

				buffer=[];
				while(isempty(buffer))
					buffer=fgetl(fid);
				end
			end
			disp('finished reading parameter info');
		end

		%Tops block
		if( strcmp(buffer(1:2),'~T') )
			nhead=nhead+1;
			if(length(buffer)>bmax) bmax=length(buffer); end
			lasheader(nhead,1:length(buffer))=buffer;

			disp('reading tops');
			buffer=fgetl(fid);
			nhead=nhead+1;
			if(length(buffer)>bmax) bmax=length(buffer); end
			lasheader(nhead,1:length(buffer))=buffer;

			%allocate space for 100 tops
			tops=ones(100,30);
			ztops=nan*ones(100,1);
			ntops=0;

			buffer=fgetl(fid);
			nmax=0;
			while( buffer(1:1)~= '~' )
				nhead=nhead+1;
				if(length(buffer)>bmax) bmax=length(buffer); end
				lasheader(nhead,1:length(buffer))=buffer;

				ntops=ntops+1;
				inot=findstr(buffer,' . ');
				if(isempty(inot))
					inot=findstr(buffer,'   ');
				end
				inot=inot(1);
				ind=find(buffer==':')-1;
				top=sscanf(buffer(1:inot-1),'%s');
				n=length(top);
				if(n>nmax) nmax=n; end
				tops(ntops,1:n)=top;
				ztops(ntops)=sscanf(buffer(inot+2:ind),'%f');
				buffer=[];
				while(isempty(buffer))
					buffer=fgetl(fid);
				end
			end

			tops=tops(1:ntops,1:nmax);
			ztops=ztops(1:ntops);

			disp([ int2str(ntops) ' tops read'])

		end

		%Curve information block
		if( strcmp(buffer(1:2),'~C') )
			nhead=nhead+1;
			if(length(buffer)>bmax) bmax=length(buffer); end
			lasheader(nhead,1:length(buffer))=buffer;

			disp('reading curve info')
			buffer=fgetl(fid);
			nhead=nhead+1;
			if(length(buffer)>bmax) bmax=length(buffer); end
			lasheader(nhead,1:length(buffer))=buffer;

			buffer=fgetl(fid);
			nhead=nhead+1;
			if(length(buffer)>bmax) bmax=length(buffer); end
			lasheader(nhead,1:length(buffer))=buffer;

			buffer=fgetl(fid);

			%anticipate up to 30 curves
			logdesc=ones(30,60);
			nlogs=0;
			nmax=0;

			while( buffer(1:1)~= '~' )
				nhead=nhead+1;
				if(length(buffer)>bmax) bmax=length(buffer); end
				lasheader(nhead,1:length(buffer))=buffer;

				if(buffer(1,1)==' ')
					ichk=2:5;
					ichk2=7:10;
				else
					ichk=1:4;
					ichk2=6:9;
				end

				nlogs=nlogs+1;
				ind=find(buffer==':');
				lognum=sscanf(buffer(ind+1:ind+5),'%f');
				ind2=find(abs(buffer(ind:length(buffer)))>64);
				name=buffer((ind+ind2(1)-1:length(buffer)));
				ind2=find(abs(name)~=32);
				name=name(1:ind2(length(ind2)));
				n=min([60 length(name)]);
				if(n>nmax) nmax=n; end
				logdesc(nlogs,1:n)=name;

				logmnem(nlogs,:)=buffer(ichk);

				if(strcmp(buffer(ichk),'DEPT') )
					dpthunits=sscanf(buffer(ichk2),'%s');
				end
				if(strcmp(buffer(ichk),'ETIM') )
					dpthunits=sscanf(buffer(ichk2),'%s');
				end

				buffer=[];
				while(isempty(buffer))
					buffer=fgetl(fid);
				end
			end
			logdesc=logdesc(1:nlogs,1:nmax);
			logmnem=logmnem(1:nlogs,:);
			disp('finished reading curve info')
		end

		if( ~strcmp(buffer(1:1),'~') )
			buffer=fgetl(fid);
		end

  end  %endwhile  
		nhead=nhead+1;
		if(length(buffer)>bmax) bmax=length(buffer); end
		lasheader(nhead,1:length(buffer))=buffer;
  % fix up the header
  lasheader=lasheader(1:nhead,1:bmax);

	%read the logs in one swell foop
	 disp('reading logs')
  logmat=fscanf(fid,'%g',[nlogs,inf]);
  logmat=logmat.';