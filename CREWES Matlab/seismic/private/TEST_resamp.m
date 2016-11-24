function [failures, TOTALTESTS]=TEST_resamp;
% TEST_RESAMP: Test the resamp function to make sure that it works properly.
% This can only be run through resamp itself, 
%
% results=resamp('selftest');
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

% Chad Hogan, April 2004.

% $Id: TEST_resamp.m,v 1.2 2004/07/30 21:23:40 kwhall Exp $

% This is pretty crude stuff, but it's a start at least to make sure that
% resamp isn't horribly botched. As I understand more of resamp, I'll add
% more tests.

% TO ADD ANOTHER TEST::::::::::::::::::::::::
% Every time you add a test, increment this number.
TOTALTESTS = 3;
% Then write your test below. Do whatever you like. However, if you have a
% failure, try to give the user some information about what failed (a
% 'warning' is mandatory of course). Also, increment 'results' every time
% you have a successful test.
results = 0;

%%%%%%
% TEST 1
%%%%%%

disp('test #1: testing the most basic functionality');

sig1  = [1.5 2.4 9.7 8.4 2.97 5.4 9.8 2.3 4.2 4.7 5.4 9.8 0.33 2.2 4.6 6.3 2.6 7.3 7.2 8.5 3.5];
time1 = [0:0.1:2];


[sig2, time2] = resamp(sig1, time1, 0.2); 
[sig3, time3] = resamp(sig1, time1, 0.05);

sig2good = [5.66197867776952	5.76593553221223	4.52836118117348	4.41084962781126	5.20478452854665	6.23780331518532	6.41980499687063	4.97633119535524	4.26859469520927	4.96742961263511	4.47771868110648];
sig3good = [1.50000170043230	1.21617338765648	2.39999798092758	6.30195974743375	9.70000067173536	10.31242777458402	8.40000074526698	5.14802767450255	2.97000007712321	3.13061651472738	5.39999744379347	8.86091720265508	9.80000393319790	6.06026798872905	2.29999698906764	2.41420918128633	4.20000076982697	4.89002608522540	4.70000184371752	4.37972822005756	5.39999586995070	8.53342811749420	9.80000475004719	5.57530616561596	0.32999665631840	0.08366169914845	2.20000182840712	3.59513384271617	4.59999807823965	6.05595268597862	6.30000285554064	4.00228024679034	2.59999690008097	4.49033174736359	7.30000270293169	7.55187449322561	7.19999753293494	8.25830091701488	8.50000223768566	6.03081845470749	3.49999849375904];

if (size(sig2) ~= size(time2))
    warning('SANITY check #1a FAILED, time vector doesn''t match signal size');
elseif (size(sig3) ~= size(time3))
    warning('SANITY check #1b FAILED, time vector doesn''t match signal size'); 
elseif (~isempty(find((sig2good - sig2) > 1e-14)))
    warning('resamp test #1c FAILED, results inaccurate. See TEST_resamp.m');
elseif (~isempty(find((sig3good - sig3) > 1e-14)))
    warning('resamp test #1d FAILED, results inaccurate. See TEST_resamp.m');
else
    disp('test #1 PASSED');
    results = results + 1;
end

%%%%%%
% TEST 2
%%%%%%

disp('test #2: testing zone chopping with NaN');

sig1  = [1.5 2.4 9.7 8.4 2.97 nan 5.4 9.8 2.3 4.2 4.7 5.4 9.8 0.33 nan 2.2 4.6 6.3 2.6 7.3 7.2 8.5 3.5];
time1 = [0:0.1:2.2];

[sig2, time2] = resamp(sig1, time1, 0.2); 
[sig3, time3] = resamp(sig1, time1, 0.05);

sig2good = [6.06591344523204	-0.05798201915956	13.35562599930528	6.74405312474847	7.41399754950114	3.73844721762752	3.06850150321375	NaN	7.79327573551779	-4.93623086733957	2.75672332112766	15.48623148747553];
sig3good = [1.50000170043230	1.21617338765648	2.39999792873977	6.30891885732522	9.70000085870744	10.26868246497226	8.40000075495621	5.27069229526351	2.96999883293991	NaN	NaN	NaN	5.39999618905921	8.28085336597415	9.80000432757119	6.48723818413003	2.29999693752718	2.40732786735316	4.20000076982697	4.89002608522540	4.70000180355653	4.37030005762458	5.39999603501053	8.72140339950806	9.80000448282988	5.70941472958755	0.32999694339234	NaN	NaN	NaN	2.20000014031911	3.18262941798436	4.59999849395860	6.20861362153188	6.30000281587782	3.99698475005988	2.59999690008097	4.71442371978530	7.30000270293169	7.55187449322561	7.19999753293494	8.25830091701488	8.50000223768566	6.03081845470749	3.49999849375904];

if (size(sig2) ~= size(time2))
    warning('SANITY check #2a FAILED, time vector doesn''t match signal size');
elseif (size(sig3) ~= size(time3))
    warning('SANITY check #2b FAILED, time vector doesn''t match signal size'); 
elseif (~isempty(find((sig2good - sig2) > 1e-14)))
    warning('resamp test #2c FAILED, results inaccurate. See TEST_resamp.m');
elseif (~isempty(find((sig3good - sig3) > 1e-14)))
    warning('resamp test #2d FAILED, results inaccurate. See TEST_resamp.m');
else
    disp('test #2 PASSED');
    results = results + 1;
end

%%%%%%
% TEST 3
%%%%%%

disp('test #3: testing extended block of NaN within signal');

sig1  = [1.5 2.4 9.7 8.4 2.97 nan nan nan nan nan nan nan nan nan nan nan nan nan 5.4 9.8 2.3 4.2 4.7 5.4 9.8 0.33 nan 2.2 4.6 6.3 2.6 7.3 7.2 8.5 3.5];
time1 = [0:0.1:3.4];

[sig2, time2] = resamp(sig1, time1, 0.2); 
[sig3, time3] = resamp(sig1, time1, 0.05);

sig2good = [6.06591344523204	-0.05798201915956	13.35562599930528	NaN	NaN	NaN	NaN	NaN	NaN	6.74405312474848	7.41399754950112	3.73844721762751	3.06850150321377	NaN	7.79327573551779	-4.93623086733957	2.75672332112766	15.48623148747553];
sig3good = [1.50000170043230	1.21617338765648	2.39999792873977	6.30891885732522	9.70000085870744	10.26868246497226	8.40000075495621	5.27069229526351	2.96999883293991	NaN	NaN	NaN	NaN	NaN	NaN	NaN	NaN	NaN	NaN	NaN	NaN	NaN	NaN	NaN	NaN	NaN	NaN	NaN	NaN	NaN	NaN	NaN	NaN	NaN	NaN	NaN	5.39999618905921	8.50159207779556	9.80000432757119	6.48723818413003	2.29999693752718	2.40732786735316	4.20000076982697	4.89002608522540	4.70000180355653	4.38508358445963	5.39999603501053	8.49757052424276	9.80000448282988	5.70941472958755	0.32999694339234	NaN	NaN	NaN	2.20000014031911	3.18262941798436	4.59999849395860	6.12643420985323	6.30000281587782	3.99698475005988	2.59999690008097	4.71442371978530	7.30000270293169	7.55187449322561	7.19999753293494	8.25830091701488	8.50000223768566	6.03081845470749	3.49999849375904];

if (size(sig2) ~= size(time2))
    warning('SANITY check #3a FAILED, time vector doesn''t match signal size');
elseif (size(sig3) ~= size(time3))
    warning('SANITY check #3b FAILED, time vector doesn''t match signal size'); 
elseif (~isempty(find((sig2good - sig2) > 1e-14)))
    warning('resamp test #3c FAILED, results inaccurate. See TEST_resamp.m');
elseif (~isempty(find((sig3good - sig3) > 1e-14)))
    warning('resamp test #3d FAILED, results inaccurate. See TEST_resamp.m');
else
    disp('test #3 PASSED');
    results = results + 1;
end

%%%%%%%%%%%%%%%%
failures = TOTALTESTS - results;
disp('----------------------------');
disp('resamp self test COMPLETED.');
disp('----------------------------');
passedstr = sprintf('%d/%d tests PASSED', results, TOTALTESTS);
failedstr = sprintf('%d/%d tests FAILED', failures, TOTALTESTS);
disp(passedstr);
disp(failedstr);