function seisex=extrapfoci(seis,f,x,v,table,ktable,ntable,params)
%
% seisex=extrapfoci(seis,f,x,v,dz,table,ktable,params)
%
% Explicit space-frequency domain wavefield extrapolation based
% on the FOCI method. This method first designs a forward
% extrapolation operator for a step of dz/2 and then a least-squares
% inverse of this operator. The final operator is given by
% final_op = conv(conj(inverse_op), forward_op)
%
% seis ... input seismic matrix in (x,f) domain
% f ... frequency coordinate for rows of seis
% x ... space coordinate for columns of seis
% v ... velocity vector (same size as f)
% dz ... depth step size
% table ... table of extrapolation operators, one per row
% ktable ... vector of k values labeling the rows of table
%           Assumes regular sampling
% ntable ... vector of the same size as ktable giving operator lengths
% params ... vector of parameters. Nan entires invoke defaults
% params(1) ... minimum frequency to extrapolate
% params(2) ... maximum frequency to extrapolate
% params(3) ... round velocities to the nearest this many m/s (or ft/sec)
%               (applies to focussing phase shift only)
%            ****** default 100 ******
% 
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

if(~isnan(params(1)))
    fmin=params(1);
else
    fmin=f(1);
end
if(~isnan(params(2)))
    fmax=params(2);
else
    fmax=(f(end));
end
if(~isnan(params(3)))
    nround=params(3);
else
    nround=100;
end

nx=length(x);

if(length(f)>1)
	iuse=between(fmin,fmax,f,2);
	dk=ktable(2)-ktable(1);
	kmin=ktable(1);
else
    iuse=1;
    dk=1;
    kmin=ktable(1);
end

%disp([int2str(length(iuse)) ' frequencies to extrapolate'])

%process velocities
vround=nround*round(v/nround);
test=zeros(size(vround));
nvels=1;
inext=1;
while(~prod(test))
    vels{nvels}=vround(inext);
    ivels{nvels}=find(vround==vels{nvels});
    test(ivels{nvels})=1;
    ii=find(test==0);
    if(~isempty(ii))
        inext=ii(1);
        nvels=nvels+1;
    end
end
%so vels is a cell array of the unique velocites in vround
%ivels points to the locations of each unique velocity

nopm=size(table,2);%maximum operator length
nopm2=floor(nopm/2);

seisex=zeros(size(seis));

for jf=1:length(iuse)
    tmpout=zeros(1,nx+nopm-1); %storage for extrapolated frequency
    tmpin=seis(iuse(jf),:);
%     for jx=1:length(x)
%         k=f(iuse(jf))/v(jx);
%         %grab closest operator in table.
%         jk=round((k-kmin)/dk)+1;
%         nop=ntable(jk);
%         nop2=floor(nop/2);
%         op=table(jk,1:nop);
%         dnop=nopm-nop;
%         dnop2=nopm2-nop2;
%         %tmpout(jx:jx+nop-1)=tmpout(jx:jx+nop-1)+op.*seis(iuse(jf),jx);
%         tmpout(jx+dnop2:jx+nopm-dnop2-1)=tmpout(jx+dnop2:jx+nopm-dnop2-1)+op.*tmpin(jx);
%     end
%
% The loop above (commented out) loops over spatial position. Unless
% velocity is extremely chaotic it is always faster to loop over velocity
% as below
%
    for jvel=1:nvels
        %grab closest operator in table. No interpolation since they are
        %different lengths
        k=f(iuse(jf))/vels{jvel};
        jk=round((k-kmin)/dk)+1;
        if(jk<1) jk=1; end
        if(jk>length(ntable))jk=length(ntable);end
        nop=ntable(jk);
        nop2=floor(nop/2);
        op=table(jk,1:nop);
        %dnop=nopm-nop;
        %dnop2=nopm2-nop2;
        tmp=zeros(size(tmpout));
        %tmp(ivels{jvel}+nop2+1)=tmpin(ivels{jvel});
        tmp(ivels{jvel}+nopm2+1)=tmpin(ivels{jvel});
        tmpout=tmpout+convz(tmp,op);
    end
    seisex(iuse(jf),:)=tmpout(1+nopm2:1+nopm2+nx-1);
end      