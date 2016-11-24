function trout=pad_trace(trin,trdsign,flag)
% PAD_TRACE: pads (truncates) one trace with zeros to be the length of another
%
% trout=pad_trace(trin,trdsign,flag)
%
% PAD_TRACE pads (or truncates) trin to the same length as design vector trdsign
%
% trin= input trace to be padded (truncated)
% trdsign= design trace to give desired length (always a vector)
% flag=0 the pad is added to the end of trin
%     =1 the pad is added such that the central sample of trin
%        stays in the middle.
% ***********default=0 *************
% trout= output trace
%
% Example
%  pad_trace([1 2 3 4 5], zeros(11), 1)
%  ans =
%     0     0     0     1     2     3     4     5     0     0     0
%
%
% by G.F. Margrave, June 1991
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

 if nargin<3, flag=0; end
 [m,n]=size(trin);
 if((m-1)*(n-1)~=0); error('pad only works for vectors, not matrices'); end
 trflag=0;
 if(m==1); trin=trin.'; trflag=1; end
 nin=length(trin);
 nout=length(trdsign);
 
 %
 % we have different cases for odd and even for both input and
 % output sizes
 %     
 trout=zeros(nout,1);
 if(flag==0)
     %simple case, pad on end
     if(nout>=nin)
         trout(1:nin)=trin;
     else
         trout=trin(1:nout);
     end
 else
     %harder case. Maintain position of center sample
     %
     % we have different cases for odd and even for both input and
     % output sizes
     %
     if(iseven(nin))
         if(iseven(nout))
             %even in even out
             if(nout>nin)
                 %sample at nin/2+1 goes to nout/2+1
                 j0=(nout-nin)/2;
                 trout(j0+1:j0+nin)=trin;
             else
                 %as before
                 k0=(nin-nout)/2;
                 trout=trin(k0+1:k0+nout);
             end
         else
             %even in odd out
             if(nout>nin)
                 %sample at nin/2 goes to (nout+1)/2
                 j0=(nout+1)/2-nin/2-1;
                 trout(j0+1:j0+nin)=trin;
             else
                k0=-(nout+1)/2+nin/2+1;
                trout=trin(k0+1:k0+nout);
             end
             
         end
     else
         if(iseven(nout))
             %odd in even out
             if(nout>nin)
                 %sample at (nin+1)/2 goes to nout/2+1
                 j0=nout/2+1-(nin+1)/2;
                 trout(j0+1:j0+nin)=trin;
             else
                 %as before
                 k0=-nout/2-1+(nin+1)/2;
                 trout=trin(k0+1:k0+nout);
             end
         else
             %odd in odd out
             if(nout>nin)
                 %sample at nin/2 goes to (nout+1)/2
                 j0=(nout-nin)/2;
                 trout(j0+1:j0+nin)=trin;
             else
                k0=(nin-nout)/2;
                trout=trin(k0+1:k0+nout);
             end
             
         end
         
     end
 end
 
 if(trflag)
     trout=trout.';
 end
 
 function flag=iseven(n)
 %test for even/odd
 if(floor(n/2)*2==n)
     flag=1;
 else
     flag=0;
 end
         
     