function SEGY_WriteBinaryHeader(FILE, bhead)
% SEGY_WRITEBINARYHEADER(FILE, bhead)
% 
% This function will stick the header onto the FILE. 'bhead' is a binary
% header structure for a SEGY file. You can get a skeleton one from the
% function SEGY_GETBINARYHEADER. Once you have that, fix the elements so they
% match your data, and then write them to the FILE with this function.
%
% This function will immediately seek to the binary header location (just
% after the text header, so at 3200 bytes) that you have and stick a header
% in there. IT WILL OVERWRITE THE BINARY HEADER THAT IS ALREADY THERE. If
% there is no header, it will overwrite whatever is in that space. So,
% this function is handy for starting out with a new file or for updating an
% existing binary header in a SEGY file.
%
% Note that you must have the text header in place already before you write
% the binary header. Make sure you have run SEGY_WRITETEXTHEADER before you
% try to write with this, or it will become confused and fail.
%
% YOU CANNOT INSERT A BINARY HEADER IN FRONT OF A BUNCH OF TRACES WITH
% THIS FUNCTION. I don't know when you'd want that, except maybe for a
% quick'n'dirty conversion of a Seismic Unix file into full
% SEGY. However, you cannot do that with this function.
%
% Chad Hogan, 2004
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

% $Id: SEGY_WriteBinaryHeader.m,v 1.3 2009/07/20 19:53:39 kwhall Exp $

if(fseek(FILE, 3200, 'bof') ~= 0)
    % rewind the file to the binary header start
    disp(ferror(FILE));
    warning('seeking failed, hopefully you are writing SU');
    fseek(FILE, 0, 'bof');
end


if(fwrite(FILE, bhead.jobid,    'int')   ~= 1) disp(ferror(FILE)); error; end
if(fwrite(FILE, bhead.lino,     'int')   ~= 1) disp(ferror(FILE)); error; end
if(fwrite(FILE, bhead.reno,     'int')   ~= 1) disp(ferror(FILE)); error; end
if(fwrite(FILE, bhead.ntrpr,    'short') ~= 1) disp(ferror(FILE)); error; end
if(fwrite(FILE, bhead.nart,     'short') ~= 1) disp(ferror(FILE)); error; end
if(fwrite(FILE, bhead.hdt,      'unsigned short') ~= 1) disp(ferror(FILE)); error; end
if(fwrite(FILE, bhead.dto,      'unsigned short') ~= 1) disp(ferror(FILE)); error; end
if(fwrite(FILE, bhead.hns,      'unsigned short') ~= 1) disp(ferror(FILE)); error; end
if(fwrite(FILE, bhead.nso,      'unsigned short') ~= 1) disp(ferror(FILE)); error; end
if(fwrite(FILE, bhead.format,   'short') ~= 1) disp(ferror(FILE)); error; end
if(fwrite(FILE, bhead.fold,     'short') ~= 1) disp(ferror(FILE)); error; end
if(fwrite(FILE, bhead.tsort,    'short') ~= 1) disp(ferror(FILE)); error; end
if(fwrite(FILE, bhead.vscode,   'short') ~= 1) disp(ferror(FILE)); error; end
if(fwrite(FILE, bhead.hsfs,     'short') ~= 1) disp(ferror(FILE)); error; end
if(fwrite(FILE, bhead.hsfe,     'short') ~= 1) disp(ferror(FILE)); error; end
if(fwrite(FILE, bhead.hslen,    'short') ~= 1) disp(ferror(FILE)); error; end
if(fwrite(FILE, bhead.hstyp,    'short') ~= 1) disp(ferror(FILE)); error; end
if(fwrite(FILE, bhead.schn,     'short') ~= 1) disp(ferror(FILE)); error; end
if(fwrite(FILE, bhead.hstas,    'short') ~= 1) disp(ferror(FILE)); error; end
if(fwrite(FILE, bhead.hstae,    'short') ~= 1) disp(ferror(FILE)); error; end
if(fwrite(FILE, bhead.htatyp,   'short') ~= 1) disp(ferror(FILE)); error; end
if(fwrite(FILE, bhead.hcorr,    'short') ~= 1) disp(ferror(FILE)); error; end
if(fwrite(FILE, bhead.bgrcv,    'short') ~= 1) disp(ferror(FILE)); error; end
if(fwrite(FILE, bhead.rcvm,     'short') ~= 1) disp(ferror(FILE)); error; end
if(fwrite(FILE, bhead.mfeet,    'short') ~= 1) disp(ferror(FILE)); error; end
if(fwrite(FILE, bhead.polyt,    'short') ~= 1) disp(ferror(FILE)); error; end
if(fwrite(FILE, bhead.vpol,     'short') ~= 1) disp(ferror(FILE)); error; end

%write unassigned hdr values
for i=1:120
    if(fwrite(FILE, 0, 'short') ~= 1) disp(ferror(FILE)); error; end % filler.
end

if(fwrite(FILE, bhead.rev,     'short') ~= 1) disp(ferror(FILE)); error; end
if(fwrite(FILE, bhead.trfix,   'short') ~= 1) disp(ferror(FILE)); error; end
if(fwrite(FILE, bhead.nthdr,   'short') ~= 1) disp(ferror(FILE)); error; end

%write unassigned hdr values
for i=1:47
    if(fwrite(FILE, 0, 'short') ~= 1) disp(ferror(FILE)); error; end % filler.
end