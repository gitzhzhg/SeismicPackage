function fmatout=fmset(fmat,x,y,samples)

% fmatout=fmset(fmat,x,y,samples)
%
% This is the main routine for building spatially flexible matricies or
% 'fleximats'.  A fleximat is used to store column vectors of data that
% exist at possibly irregular x locations and have different lengths but
% each have the same discrete sample interval (in y). This routine will
% update an existing fleximat with a new column. If the existing fmat is []
% then a new one is created. If the new column has the same x coordinate as
% an existing column then the existing column is replace else the fmat is
% expanded by one column.  Columns are not in any particular x coordinate
% order but are in the order that they were created in. Columns are padded
% with nan's to adjust for different lengths. A column can be deleted by
% setting its contents to [] e.g.: fmat=fmset(fmat,xnot,[],[]); will delete
% the column corresponding to x==xnot.
% Never access the data in an fmat directly, instead use fmget to do so. 
% fmget can return the matrix of data, its x coordinates or its y coordinates.
%
%   fmat = the input fleximat. May be [] for creation of a new fleximat
%   x = the x or column coordinate for the samples being added. The length 
%       of x must be the same as the number of columns in samples
%   y = the row coordinates for the samples being added. The length of y 
%       must be the same as the number of rows in samples
%   samples = matrix of samples to be added to the fleximat
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

%test a few things
[rs,cs]=size(samples); if( isempty(x) ) x=1; end
if( cs~= length(x) )
		if( length(x)~= 1 & rs~= 1)
			error('x coordinate and sample matrix must have same number of columns');
		end
end

if(rs==1 | cs==1) lsamp=length(samples); 
else
	lsamp=rs;
end

if(length(y)~=lsamp)
	error('y vector and sample vector must be equal lengths');
end

% make sure we have a regular sample rate. We allow a zero length y since that is the
% logical choice when deleting a column
test=diff(y);
if(length(y)> 0)
	if(abs(sum(diff(test)))/length(y)>10*eps)
		error(' fleximats must have regularly sampled columns');
	end
end

y=y(:);

if( cs==1 | rs==1 )
	samples=samples(:);
	[rs,cs]=size(samples);
end

% see is fmat is null
if(isempty(fmat))
		
	fmatout=[0 x; y samples];

	return;
end

sampin=samples;
fmatout=fmat;
xin=x;
if(isempty(samples)) cs=1; end
yorig=y;
for k=1:cs
 if(~isempty(samples))
		samples=sampin(:,k);
	end
	x=xin(k);
    y=yorig;
	[nr,nc]=size(fmatout);
	nx=nc-1;
	ny=nr-1;
	yout=fmatout(2:nr,1);
	xout=fmatout(1,2:nc);
	matout=fmatout(2:nr,2:nc);

	% see if the samples are null which means we are deleting
	if(isempty(samples))
		ind=find(xout==x);
		nx=length(xout);
		xout=[xout(1:ind(1)-1) xout(ind(1)+1:length(xout))];
		matout=[matout(:,1:ind(1)-1) matout(:,ind(1)+1:nx)];
		fmatout=[0 xout; yout matout];
		return;
	end
	

	% see if new vector has the same sample rate as the old one

	dy=yout(2)-yout(1);
	if(abs(test(1)-dy)/dy>1.e06*eps)
		error(' new vector must have the same sample rate as the old ');
	end

	%see if we need to resample the new vector to account for grid mis-alignment
	n1exact=(yout(1)-y(1))/dy;
	n1=floor(n1exact);
	%if( abs(n1exact-n1)>abs(n1exact/100)) % this condition allows 1% error or smaller
		ybeg=yout(1)-n1*dy;
		yend=y(length(y)-1)+(ybeg-y(1));
		yprime=ybeg:dy:yend;
		samples=sincinan(samples',y',yprime);
		samples=samples(:);
		y=yprime';
		yprime=[];
	%else
	%	ybeg=yout-n1*dy;

	%	y=xcoord(ybeg,dy,length(y))';
	%end

	% determine if we need to expand y
	y1=min(y);y2=max(y);
	y1out=min(yout);y2out=max(yout);

	%ntop=0;
	ntop=round((y1out-y1)/dy);
	if(y1<y1out)
		nn=round((y2out-y1)/dy);
		y2out=y1+nn*dy;
		yout=(y1:dy:y2out)';
	end

	nbot=round((y2-y2out)/dy);
	if(y2>y2out)
		nn=round((y2-y1)/dy);
		y2=y1+nn*dy;
		yout=(yout(1):dy:y2)';
	end

	ny=length(yout);

	%expand the matrix if needed

	if(ntop>0)
		nrowtop=ntop;
	else
		nrowtop=0;
	end
	if(nbot>0)
		nrowbot=nbot;
	else
		nrowbot=0;
	end

	matout=[nan*ones(nrowtop,nx); matout; nan*ones(nrowbot,nx)];

	%expand the input samples if need be
	if(ntop<0)
		nsamptop=abs(ntop);
	else
		nsamptop=0;
	end
	if(nbot<0)
		nsampbot=abs(nbot);
	else
		nsampbot=0;
	end

	samples=[nan*ones(nsamptop,1);samples;nan*ones(nsampbot,1)];

	% ok see if we are replacing a column
	ind=find(xout==x);

	if(length(ind)>0)
		matout(1:ny,ind)=samples;
	else
		matout=[matout samples];
		xout=[xout x];
	end

	%make the output fleximat
	fmatout=[0 xout;yout matout];

end