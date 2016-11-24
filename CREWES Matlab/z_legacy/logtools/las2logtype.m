function itype=las2logtype(mnem)

% itype=las2logtype(mnem)
%
% Convert a 4 letter las mnemonic identifying a log to a numeric logtype.
% The master list of possible numeric logtypes is to be found in 
% the source code of this function.
%
% As of Dec 7, 1995, these were:
% -1 ... unknown or un-specified
% 0  ... p-wave sonic
% 1  ... bulk density
% 2  ... formation denisty
% 3  ... apparent density
% 4  ... gamma ray
% 5  ... spontaneous potential
% 6  ... caliper
% 7  ... s-wave sonic
% 8  ... neutron porosity
% 9  ... apparent porosity
% 10 ... porosity density (LS)
% 11 ... porosity effective
% 12 ... porosity total
% 13 ... focussed resistivity
% 14 ... medium induction
% 15 ... deep induction
% 16 ... SFL resistivity
% 17 ... mel caliper
% 18 ... micronormal
% 19 ... microinverse
% 20 ... porosity density (SS)
% 21 ... Poissons ratio
% 22 ... VP/VS ratio
% 23 ... Youngs Modulus
% 24 ... Lames Lamda Constant
% 25 ... Lames Rigidity
% 26 ... Bulk Modulus
% 27 ... P-wave velocity
% 28 ... S-wave velocity
% 29 ... S-wave from array sonic 
% 30 ... P-wave from array sonic
% 31 ... Gamma ray density
% 32 ... Gamma ray porosity
% 33 ... Photoelectric cross section
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

    if (~ischar(mnem))
        error('mnem must be a character array (string)');
    end

    % strip trailing spaces and convert to uppercase
	mnem = upper(deblank(mnem));
	
	%strip any trailing digits from the mnemonic
	p = length(mnem);
    %check to preserve mnemonics 'dt33',dt'44','dt55','dt66'
    pmin=2;
    if(p>=4)
        if(strcmp(mnem(1:4),'DT33')|strcmp(mnem(1:4),'DT44')|strcmp(mnem(1:4),'DT55')|strcmp(mnem(1:4),'DT66'))
            pmin=4;
        end
    end
	c = mnem(p);
	while (l2lt_isdigit(c) && (p>1)) 
        p = p - 1;
        c = mnem(p);
	end
    % deblank handles the case of 'DT 5'
    if(p<pmin);p=pmin;end
	mnem = deblank(mnem(1:p));
    % look for trailing '-' or '_'
    if(mnem(end)=='-'  || mnem(end)=='_')
        %this handles the case of 'RHOB-2'
        mnem=mnem(1:end-1);
    end
	
	switch mnem
	case {'AU','AC','DT','CO','SON','PSON','DTP','ITT','DT4P','DTCR','DTCO'...
            'DTC','DT33','DT33-1'}
        itype=0;
	case {'RHOB','DENE','DEN','RHOZ'}
        itype=1;
	case 'RHGF';
        itype=2;
	case 'RHGA'
        itype=3;
	case {'GRC','GR'}
        itype=4;
	case 'SP'
        itype=5;
	case 'CALI'
        itype=6;
	case {'DTSW','SSON','SDT','DTS','DT2','DTSD','DTSM','DT44','DT55','DT66',...
            'DTXX','DTYY','FastShear','SlowShear'}
        itype=7;
	case {'NPHI','PHIN'}
        itype=8;
	case 'PHIA'
        itype=9;
	case {'PHID','DPHI'}
        itype=10;
	case {'EPHI','PHIE'}
        itype=11;
	case 'PHIT'    % as in a phit of rage
        itype=12;
	case {'SFLU','SFL'}
        itype=13;
	case 'ILM'
        itype=14;
	case 'ILD'
        itype=15;
	case 'SFLR'
        itype=16;
	case 'UNVI'
        itype=17;
	case 'MNOR'
        itype=18;
	case 'MINV'
        itype=19;
	case 'DPSS';
        itype=20;     
	case 'POIS';
        itype=21;     
	case 'VPVS';
        itype=22;     
	case 'YNGM';
        itype=23;     
	case 'LMDA';
        itype=24;     
	case 'RIDG';
        itype=25;     
	case 'BMOD';
        itype=26;     
	case 'VP';
        itype=27;     
	case 'VS';
        itype=28;
	case 'ASSW';
        itype=29;     
	case 'ASPW';
        itype=30;     
	case 'GRD';
        itype=31;     
	case 'GRP';
        itype=32;     
	case 'PEF';
        itype=33;     
	otherwise
        itype=-1;
	end
    

% Returns true if d is a digit '0'..'9', false otherwise
function x = l2lt_isdigit(d)
    x = ((d >= '0') & (d <= '9'));