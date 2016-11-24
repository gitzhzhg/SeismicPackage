function s=slicemat(a,traj,hwid,flag)
% SLICEMAT slices a matrix along a trajectory
%
% s=slicemat(a,traj,hwid,flag)
%
% SLICEMAT slices through a matrix along a certain trajectory.
% The trajectory must be specified in terms of a row index for
% each column and may not be double valued. The width of the slice
% is prescribed by its half width. The output matrix will contain 
% the slice with the matrix elements on the trajectory being on the
% central row. Whereever the half width of the slice prescribes
% values outside the bounds of the input martix, zeros are returned.
%
%	a = input matrix
%	traj = trajectory of the slice. Must be a vector with one 
%		entry per column of a. Each entry specifies a row index.
%       Non-integral values are rounded.
%	hwid = half width of the slice specified in rows. The output
%		slice will have 2*hwid + 1 rows. This can be a single integer
%     or it can have one entry per column if the half width is 
%     variable. Non-integral values are rounded.
%	flag = 1 ... When the slice excedes the bounds of a return 0
%			 2 ... When the slice excedes the bounds of a return NaN
%   ********* default = 1 **********
%	s = output matrix containing the slice of a. If [m,n]=size(a),
%		then the size of s is 2*hwid+1 rows and n columns.
%
% 	example a= ((1:5)')*(ones(1,10))
%		traj=[1:5 5:-1:1];
%		s=slicemat(a,traj,1)
%		s =
%		0     1     2     3     4     4     3     2     1     0
%		1     2     3     4     5     5     4     3     2     1
%		2     3     4     5     0     0     5     4     3     2
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

% G.F. Margrave, Feb. 1995

if(nargin< 4)
	flag=1;
end
if(length(hwid)==1)
	hwid=hwid*ones(1,size(a,2));
end

[m,n]=size(a);
hmx=max(hwid);
if(flag==1)
	s=zeros(2*hmx+1,n);
else
	s=nan*zeros(2*hmx+1,n);
end

for k=1:n
	hw=round(hwid(k));
	i1=max(1,round(traj(k))-hw);
	i2=min(m,round(traj(k))+hw);

	ind=(i1:i2)-round(traj(k))+hmx+1;

	s(ind,k) = a(i1:i2,k);
end

% if(flag==1)
% 	s=zeros(1,n*(2*hmx+1));
% else
% 	s=nan*zeros(1,n*(2*hmx+1));
% end
% 
% in=zeros(1,n*(2*max(hwid)+1));
% out=in;
% M=0;
% for k=1:n
% 	hw=round(hwid(k));
%     ii=max(1,round(traj(k))-hw):min(m,round(traj(k))+hw);
%     in(M+1:M+length(ii))=ii+(k-1)*m;
%     out(M+1:M+length(ii))=ii-round(traj(k))+hmx+1+M;
%     M=M+length(ii);
% end
% s(out(1:M)) = a(in(1:M));
% s=reshape(s,2*hmx+1,n);