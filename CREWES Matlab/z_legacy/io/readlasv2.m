function [logmat,logmnem,logdesc,wellname,wellid,loc,nullval,dpthunits,...
		kb,tops,ztops,lasheader]=readlasv2(fid,buffer)

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

% $Id: readlasv2.m,v 1.17 2004/11/17 01:58:52 henry Exp $

% initialize variables
logmat=[]; logmnem=[]; logdesc=[]; wellname=[]; wellid=[]; loc=[]; 
nullval=[]; dpthunits=[]; kb=[]; tops=[]; ztops=[]; lasheader=[];

%allocate a 150x100 header
lasheader=32*ones(150,100);
nhead=0;
bmax=0;
while (isempty(deblank(buffer)))
    buffer=readalinelas(fid);
end
 
% Keep reading until we hit the ~a block.  This introduces the curve data
while (~strcmpi(buffer(1:2),'~a'))
        if( strcmp(buffer(1,1),'#')|(isempty(deblank(buffer))) )
			buffer=readalinelas(fid);
		end

            
		%other information block
		if( strcmpi(buffer(1:2),'~o') )
			nhead=nhead+1;

			if(length(buffer)>bmax) bmax=length(buffer); end
			lasheader(nhead,1:length(buffer))=buffer;

			disp('reading other info');
			buffer=readalinelas(fid);
			if(isempty(buffer)) buffer=' '; end
			while( buffer(1:1)~='~' )
				nhead=nhead+1;
				if(length(buffer)>bmax) bmax=length(buffer); end
				if(length(buffer)<bmax)
					buffer=[buffer blanks(bmax-length(buffer))];
				end
				lasheader( nhead,1:length(buffer))=buffer;

				buffer=[];
				while(isempty(buffer))
					buffer=readalinelas(fid);
				end
			if(isempty(buffer)) buffer=' '; end
			end
			disp('finished reading other info');
		end


		%read well information block
		if( strcmpi(buffer(1:2),'~w') )
		   nhead=nhead+1;
		   if(length(buffer)>bmax) bmax=length(buffer); end
		   lasheader(nhead,1:length(buffer))=buffer;
		        
		   disp('reading well info');
		   buffer=readalinelas(fid);
           start=[];stop=[];step=[];
           nullval=[];wellname=[];loc=[];wellid=[];
           wellkb=[];
           while( buffer(1:1)~='~' )
		      nhead=nhead+1;
              if(length(buffer)>bmax) bmax=length(buffer); end
		      if(length(buffer)<bmax)
			         buffer=[buffer blanks(bmax-length(buffer))];
		      end
		      lasheader(nhead,1:length(buffer))=buffer;
		      
		      %  Check for start, stop and step and kb
              if(isempty(start))
                  start=readwellinfo('STRT',buffer);
              end
              if(isempty(stop))
                  stop=readwellinfo('STOP',buffer);
              end
              if(isempty(step))
                  step=readwellinfo('STEP',buffer);
              end
              if(isempty(nullval))
                  nullval=readwellinfo('NULL',buffer);
              end

              firsttok=strtok(buffer);
              if(findstr(firsttok,'WELL'))                     
                  [t,r]=strtok(buffer);
                  wellname=strtok(r,' .');
              end
		      
              % I added in this elevation bit because the parameter block
              % doesn't seem to EXIST in the las files we work with. If it
              % does show up in some LAS files, the later stuff will
              % overwrite the set kb. The import completely dies if there
              % is no valid kb. 
              % Chad Hogan, April 2004.
              
              
              if ~isempty(findstr(firsttok,'ELEV')) && strcmp(firsttok,'ELEV_TYPE')~=1
                  [t,r]=strtok(buffer);
                  wellkb=strtok(r,' .M');
                  kb=str2num(wellkb);
              end
              
              if(findstr(firsttok,'LOC '))                 
                  ind=find(buffer==':')-1;
                  loc = sscanf(buffer(11:ind),'%s');  
              end
              
              if(findstr(firsttok,'UWI '))                 
                  ind=find(buffer==':')-1;
                  wellid = sscanf(buffer(11:ind), '%s');  
              end
              
              buffer=[];
		      while(isempty(buffer))
                 buffer=readalinelas(fid);
		      end
		   end
           disp('finished reading well info');
		end
		
		%Parameter information block
		if( strcmpi(buffer(1:2),'~p') )
			nhead=nhead+1;
			if(length(buffer)>bmax) bmax=length(buffer); end
			lasheader(nhead,1:length(buffer))=buffer;

			disp('reading parameter info');
			buffer=readalinelas(fid);
			while( buffer(1:1)~= '~' )
				 nhead=nhead+1;
				 if(length(buffer)>bmax) bmax=length(buffer); end
				if(length(buffer)<bmax)
					buffer=[buffer blanks(bmax-length(buffer))];
				end
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
					buffer=readalinelas(fid);
				end
			end
			disp('finished reading parameter info');
		end

		%Tops block
		if( strcmpi(buffer(1:2),'~t') )
			nhead=nhead+1; 
			if(length(buffer)>bmax) 
                bmax=length(buffer);
            end
			lasheader(nhead,1:length(buffer))=buffer;

			disp('reading tops');
			buffer=readalinelas(fid);
			nhead=nhead+1; 
			if(length(buffer)>bmax) 
                bmax=length(buffer); 
            end
			lasheader(nhead,1:length(buffer))=buffer;

			%allocate space for 100 tops
			tops=32*ones(100,30);
			ztops=nan*ones(100,1);
			ntops=0; nmax=0;

			if (strcmp(buffer(1:1),'#') | isempty(deblank(buffer)))
				buffer=readalinelas(fid);
			end
			nmax=0;
			
			while( buffer(1:1)~= '~' )
				nhead=nhead+1;
				ind=find(buffer==':')-1;
				if(isempty(ind)) ind=length(buffer); end
				if(length(buffer)>bmax) bmax=length(buffer); end
				if(length(buffer)<bmax)
					buffer=[buffer blanks(bmax-length(buffer))];
				end
                lasheader(nhead,1:length(buffer))=buffer;
    
                %test for comment line or blank line
                [top,r]=strtok(buffer);
                if(~isempty(top))
                    if(top(1)~='#')
       				    ntops=ntops+1;
	
                        [top,r]=strtok(buffer);
						n=length(top);
						if(n>nmax) nmax=n; end
                        ttop=strunpad(top);
						
                        %read the rest of the string until we find something numeric
                        while(~isempty(r))
                             [tmp,r]=strtok(r);
                             if (~isempty(tmp))
                                 if(strcmp(tmp(1,end),':'))
                                     % checking to see if top number has
                                     % ':' connect right at the end, if it
                                     % does, removing it
                                     tmp=tmp(1,1:end-1);
                                 end
                                 if ~strcmp(tmp,'.')                                     
                                     num = str2num(tmp);
                                     if(~isempty(num))
                                        ztops(ntops)=num;
                                        break
                                     elseif(~strcmp(tmp,':'))
                                        ttop=[ttop ' ' tmp];
                                     end
                                 end
                             end
                        end
                        tops(ntops,1:length(ttop))=ttop;
                    end
                end
					
				buffer=[];
				while(isempty(buffer))
					buffer=readalinelas(fid);
				end
			end

			tops=tops(1:ntops,1:nmax);
			ztops=ztops(1:ntops);

			disp([ int2str(ntops) ' tops read'])

		end

		%Curve information block
		if( strcmpi(buffer(1:2),'~c') )
		   nhead=nhead+1;
		   if(length(buffer)>bmax) bmax=length(buffer); end
		   lasheader(nhead,1:length(buffer))=buffer;
		   
		   disp('reading curve info')
		   buffer=readalinelas(fid);
		   
		   %anticipate up to 30 curves
		   logdesc=ones(30,60);
		   logmnem=ones(30,4);

		   nlogs=0;
		   nmax=0;
		   
		   while( buffer(1:1)~= '~' )
		        while(isempty(deblank(buffer)))
			        buffer=readalinelas(fid);
                end
		        if(buffer(1,1)=='~') break; end
		        nhead=nhead+1;
		        if(length(buffer)>bmax) 
                    bmax=length(buffer); 
                end
		        if(length(buffer)<bmax)
                    buffer=[buffer blanks(bmax-length(buffer))];
		        end
		        lasheader(nhead,1:length(buffer))=buffer;
		      
		        if( buffer(1) ~= '#' )
					 nlogs=nlogs+1;
					 ind=find(buffer==':');
					 lognum=sscanf(buffer(ind+1:ind+5),'%f');
					 ind2=find(abs(buffer(ind:length(buffer)))>48);
					 if(isempty(ind2)) ind2=length(buffer); end
					 name=buffer((ind+ind2(1)-1:length(buffer)));
					 ind2=find(abs(name)~=32);
					 name=name(1:ind2(length(ind2)));
					 n=min([60 length(name)]);
					 if(n>nmax) nmax=n; end
					 logdesc(nlogs,1:n)=name(1:n);
					 
					 if(buffer(1,1)==' ')
					    ichk=2:5;
					    ichk2=2:4;
					    ichk3=7:10;
					 else
					    ichk=1:4;
					    ichk2=1:3;
					    ichk3=6:9;
					 end
					 
					 [tok,rem]=strtok(buffer,'. '); %finds everything before the '.'
					 %note both period and space in the delimiters above
					 %tok=strtok(tok);%strips off any leading and trailing blanks
					 
					 %logmnem(nlogs,:)=buffer(ichk);
					 ltok=min([4 length(tok)]);
					 tok=tok(1:ltok);
					 logmnem(nlogs,:)=[tok blanks(4-length(tok))];
			         if(ltok==4)
						 if(strcmp(tok,'DEPT') )
						    %dpthunits=sscanf(buffer(ichk3),'%s');
						    dpthunits=strtok(rem,'. ');
                            if(strcmp(dpthunits(1),'.'))
                                dpthunits=dpthunits(2:end);
                            end
						 end
						 if(strcmp(tok,'ETIM') )
						    %dpthunits=sscanf(buffer(ichk3),'%s');
						    dpthunits=strtok(rem,'. ');
                            if(strcmp(dpthunits(1),'.'))
                                dpthunits=dpthunits(2:end);
                            end
						 end
				    end
		      end
			 
		      buffer=[];
		      while(isempty(buffer))
			     buffer=readalinelas(fid);
		      end
		   end
		   logdesc=logdesc(1:nlogs,1:nmax);
		   logmnem=logmnem(1:nlogs,:);
		   disp('finished reading curve info')
		end

		if( ~strcmp(buffer(1:1),'~') )
		   buffer=readalinelas(fid);
		end

  while (isempty(deblank(buffer)))
    buffer=readalinelas(fid);
  end
 
 end  %endwhile  
 nhead=nhead+1;
 if(length(buffer)>bmax) bmax=length(buffer); end
 if(length(buffer)<bmax)
	buffer=[buffer blanks(bmax-length(buffer))];
 end
 lasheader(nhead,1:length(buffer))=buffer;
 % fix up the header
 lasheader=lasheader(1:nhead,1:bmax);

 %read the logs in one swell foop
 disp('reading logs')
 logmat=fscanf(fid,'%g',[nlogs,inf]);
 logmat=logmat.';

function buffer=readalinelas(fid)
    buffer=fgetl(fid);
    if(length(buffer)<2) buffer=[buffer blanks(2-length(buffer))]; 
end

function value=readwellinfo(flag,buffer)
    value=[];
    [t,r]=strtok(buffer);
    if(~isempty(t))
        if(length(t)>3)
            if(strcmp(t(1:4),flag))
                while(~isempty(r))
                    [t,r]=strtok(r);
                    value=sscanf(t,'%g');
                    if(~isempty(value))
                       break;
                    end
                end
            end
        end
     end
     