function header = reshapeHeader( header, dimension )
%function obj = reshapeHeader( header, dimension )
%   Reshape text header
%  Where:
%   dimension = '1D' or '2D'
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

if(~ischar(header))
    error ('Textual header must be of type char');
end

[m n]=size(header);

switch(m+n)
    case(3201)     % 1D vector input
        if (n > m) % input is row vector
            header = header'; % always start with a column vector
        end
        switch(dimension)
            case('1D')
                   % all's well, do nothing else
            case('2D')
                header = reshape(header,80,40)';
            otherwise
                error('dimension must be ''1D'' or ''2D''');
        end
    case(120)      % input is 2D matrix
        if (m > n) % input is rotated?
            error('Input must be 40 rows by 80 columns');
        end
        switch(dimension)
            case('1D')
                header = reshape(header',3200,1);
            case('2D')
                   % all's well, do nothing
            otherwise
                error('dimension must be either ''vector'' or ''matrix''');
        end
    otherwise
       error ('Textual header must be 3200 bytes');
end

end