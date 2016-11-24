function lash = lashcurves(lash,mnems,units,values,descs)
% lash = lashcurves(lash,mnems,units,values,descs) ... insert mode
% lash = lashcurves(lash,flags) ... delete mode
% 
% LASHCURVES adds or deletes log entries to or from an LAS header.
% Given an las header in a string matrix (such as is provided by
% readlas) LASHCURVES adds curve definitions as specified in the string
% matricies mnems, units, values, descs. (All must have the same number of
% rows).  If the second argument is not a string matrix, it must be a vector
% of numeric flags describing which logs are to be deleted.
% 
% searches it for references to curves whose
% 4 letter mnemonics are in mnems and removes all such references.
% Or if additional arguments are supplied, then the mnemonics
% are assumed to be added to the header.
% lash ... string matrix containing the las header
% mnems ... string matrix containing the 4 letter mnemonics (one mnemonic
%		per line)
% flags ... vector of numeric flags, one entry for each curve in the header (in
%	the order given in the header) If the flag is 1, then the curve is
%	retained in the header, if 0, it is deleted. (Note the number of
%	curves in the header is one more than the number of logs because the
%	first curve is always depth or time)
% units ... string matrix containing the units specs (4 letters) for each 
%	mnemonic. 
%	******* default is blank ******
% values ... string matrix containing the API values for each mnemonic
%	******* default is blank ******
% descs ... string matrix of colloquial descriptions for each log.
%	******* default is blank ******
% 
% G.F. Margrave, Department of Geology and Geophysics,
%	University of Calgary, 1996
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
%
%determine mode
if(isstr(mnems))
	%we are adding curves
	[n_new,m]=size(mnems);
	if(nargin<3)
		units=setstr(32*ones(n_new,4));
	end
	if(nargin<4)
		values=setstr(32*ones(n_new,20));
	end
	if(nargin<5)
		descs=setstr(32*ones(n_new,1));
	end
	
	% get the curves block from the header
	cblock=lashgetblk(lash,'curve');
	
	%decompose the block
	[mnemsb,unitsb,valuesb,descsb,cmnts,irc]=lashblkread(cblock);
	
	if(isempty(mnemsb))
		mnemsb=mnems;
		unitsb=units;
		valuesb=values;
		descsb=descs;
	else	
		%loop and add curves
		mnemsb=str2mat(mnemsb,mnems);
		unitsb=str2mat(unitsb,units);
		valuesb=str2mat(valuesb,values);
		descsb=str2mat(descsb,descs);
	end
	
	%pad out values if needed
	[n,m]=size(valuesb);
	npad=20-m;
	if(npad>0)
		valuesb=[valuesb setstr(32*ones(n,npad))];
	end
	
	%make a new block
	cblk=lashblkcreate('curve',cmnts,irc,mnemsb,unitsb,valuesb,descsb);
	
	%insert the block
	lash=lashsetblk(lash,'curve',cblk);
	
	
	
else
	
	%we are deleting curves
	flags=mnems;
	
	% get the curves block from the header
	cblock=lashgetblk(lash,'curve');
	
	%decompose the block
	[mnemsb,unitsb,valuesb,descsb,cmnts,irc]=lashblkread(cblock);
	
	%abort if we don't have exactly one flag per curve
	[ncurves,m]=size(mnemsb);
	if(length(flags)~=ncurves)
		error('there must be one flag per curve in LAS header');
	end
	
	%determine what to delete
	ind=find(flags==0);
	if(~isempty(ind))
		mnemsb(ind,:)=[];
		unitsb(ind,:)=[];
		valuesb(ind,:)=[];
		descsb(ind,:)=[];
	end
	
	%pad out values if needed
	[n,m]=size(valuesb);
	npad=20-m;
	if(npad>0)
		valuesb=[valuesb setstr(32*ones(n,npad))];
	end
	
	%make a new block
	cblk=lashblkcreate('curve',cmnts,irc,mnemsb,unitsb,valuesb,descsb);
	
	%put the block back in the header
	lash=lashsetblk(lash,'curve',cblk);
		
end
		
%modify the last line (~A) of the header
lash=lashlastline(lash,mnemsb);