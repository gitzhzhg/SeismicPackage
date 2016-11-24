function seisf=assemble_chunks(seischunk,fchunk,xchunk,ichunk,dx,nf,nx,vcrit)
% ASSEMBLE_CHUNKS ... Reassemble the complete wavefield from is spatially resampled chuncks
%
% seisf=assemble_chunks(seischunk,fchunk,xchunk,ichunk,dx,nf,nx,vcrit)
%
% seischunk ... cell array of the seismic data as partitioned into frequency chunks. 
%       Length(seischunck) is the number of chuncks. (These are usually
%       created by CREATE_CHUNKS). seisf=seischunk{j} is a seimic matrix for the 
%       jth frequency band.
% fchunk ... cell array of the frequency coordinate vectors for each chunk.
% xchunk ... cell array of the x coordinate vectors for each chunk. These
%       will each have a different sample rate. This means that
%       seisf=seischunk{j} is a matrix of size nf-by-nx where
%       nf=length(fchunk{j}) and nx=length(xchunk{j}).
% ichunk ... cell array of the frequency coordinate indicies of each chunk.
%       That is ifreqs=ichunk{k} gives the row numbers of the position of
%       seisf=seischunk{j} in the re-assembled matrix.
% dx ... desired spatial sample rate of the output
% nf ... total number of frequencies desried in the output
% nx ... total number of spatial samples in the output
% vcrit ... the 'critical' velocity that drives the spatial resampling. If
%       you have no better choice, make vcrit=min(v(:)), i.e. the slowest
%       velocity in your model.
% seisf ... re-assembled seismic matirx in the frequency domain. Size is nf-by-nx
% 
%
% G.F. Margrave, CREWES/POTSI 2004
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
nchunks=length(seischunk);
if(nargin<8)
    vcrit=ones(nchunks,1);%just make it a small number so that no effect
end
seisf=zeros(nf,nx);
for kchunk=1:nchunks
    xtmp=xchunk{kchunk};
    kcrit=fchunk{kchunk}/vcrit(kchunk);
    seisf(ichunk{kchunk},:)=kresample(seischunk{kchunk},xtmp,dx,kcrit);
end