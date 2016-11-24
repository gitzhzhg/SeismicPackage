%******* The Earth Object Data System ***********************
%
% A) CONCEPTS
% 
%   Earth Objects are special data structures designed for use in Matlab 
% to store complex earth science datatypes. Like everything else in Matlab, 
% Earth Objects are matrices which can grow or shrink as needed. They are 
% unique in that special meaning is attached to select elements which 
% allows their interpretation as earth data types. As an example, one might 
% wish to have a datatype called 'named matrix' which could be used as a 
% matlab matrix with a string providing a descriptive name. Suppose the 
% matrix we wish to give a name to has m rows and n columns and the name 
% is to be k characters in length. Then we could define a named matrix as a 
% Matlab column vector of length k + 2 +m*n which the following storage 
% scheme:
%	
%	rows 1 through k .... will contain the name converted from a Matlab
%		 string to integers
%	row k+1 ... will contain the number of data rows: m
%	row k+2 ... will contain the number of data columns: n
%	rows k+3 through k+m*n+2 ... will contain all of the elements of the
%		 m by n matrix strung out in columnwise fashion.
%
%   (Note that k would have to be a constant for all such objects and can 
% be considered as the maximum name length. A sensible value for k might 
% be 30.)
%   Then, we would have to agree never to operate numerically directly 
% on such a Matlab vector. Instead, we could build special access routines 
% called nmget and nmset which would get and set the values in such a 
% named matrix. The syntax for nmset might be something like:
%	object_out = nmset(object_in, 'string', value)
% where 
%	object_in is the input named matrix object
%	object_out is the output named matrix object (This will usually be
%		the same Matlab variable as object_in)
%	'string' is a Matlab string specifying which field we are setting. It
%		could have any of the values: 'name', 'datarows', 'datacols', 
%		and 'data'.
%	value is a Matlab variable giving the value to set into the
%		appropriate spot in the named matrix.
%		
% Similarly, nmget might work something like
%	value = nmget(object_in,'string')
%		
% where the meanings are the same as before. If my_nm is the Matlab 
% variable namefor such a named matrix, then some examples of using nmget 
% and nmset might be:
%	
%	my_nm = nmset(my_nm,'name','Fred') ... change the name to 'Fred'
%	your_nm = nmset(my_nm,'name','Matilda') ... change the name to
%		'Matilda' but output the result into a new named matrix with
%		the variable name your_nm. my_nm remains unchanged.
%	my_nm = nmset(my_nm,'data',some_matrix) ... change the matrix
%		data to the values in the matrix some_matrix. Presumably, this
%		would also set the values of 'datacols' and 'datarows' as
%		appropriate.
%	thename = nmget(my_nm,'name') ... get the name of my_nm (the first
%		k rows) and output it as a Matlab string.
%	thedata = nmget(my_nm,'data') ... get the matrix of data from my_nm
%		for plotting or crunching. This will automatically reshape it
% 		into theproper size by getting the values in 'datarows' and 
%		'datacols'.
%
%   It is easy to see that this logic can be extended to build very
% complex objects which can contain all earth science information relevant
% to a given project. In a direct sense, we have designed a method for
% building "abstract data types" within Matlab which is one of the major
% goals of object-oriented programming (oop). The development of Matlab
% software using such data structures, including i/o routines and links to
% other software systems has a number of advantages including:
%	-  Data organization: the encapsulation of all data with a common
%		theme into a single Matlab variable greatly simplifies file i/o
%		and the processof providing that data to a Matlab application.
%	- Matlab applications can be independent of the data internals: So
%		long as applications are developed which expect such earth
%		objects as input data and which only interact with the objects
%		through their "get" and "set" methods; then, these applications
%		are independent of the detailed internal structure of the data
%		objects.
%
%   These benefits mean that Matlab applications can evolve 
% independently of the evolution of the data objects which leads to much 
% more robust software. And, such applications can exchange data more 
% simply without extensive user intervention.
%	
% B) Current Object Zoology
% 
%   Currently, two major types of objects exist with and a number more 
% are under development. In existence now are "random objects" and "gridded 
% objects" which are created with the Matlab routines "randobj" and 
% "gridobj" and whose get and set methods are "objget" and "objset". In 
% development are "container objects" (objects which will contain other 
% objects) and specific objects to model wells and their varied information.
% 	
%   Gridded objects may be used to store any earth information which is 
% sampled on a regular x-y grid (1-D or 2-D grids). Examples include seismic 
% traces, gridded gravity and magnetics data, gridded reservoir data, etc. A 
% single gridded object may contain an unlimited number of gridded datasets 
% though all must have the same geometry. (The object grows as needed to 
% accommodate new datasets.) Each dataset has a unique 30 character 
% (maximum) name, a create and last modified date and a protection flag to 
% guard against accidental erasure. The object itself has fields for such 
% things as object creation date, last modified date, geometry description, 
% object name (unlimited length), and more. The geometry is typically 
% specified in local coordinates (numbers ranging from 0 to 100 or so) but 
% global coordinates (probably UTM's) are supported too. Gridded objects are 
% created directly with "gridobj" though "readzmap" also creates gridded 
% objects by calling "gridobj" internally. Gridded objects have an assumed 
% (x,y) coordinate system in which x varies along the rows (is constant for 
% each column) and y varies along the columns (is constant for each row).
% 
%   Random objects are used to store earth datasets which are sampled 
% at irregular locations such that (x,y) coordinates must be stored for each 
% sample point. As with gridded objects, any number of datasets with 
% similar geometry may be stored. There is no assumed coordinate system 
% for random objects; rather, x and y coordinates must be explicitly set. It 
% is a good idea when doing so to give the coordinates the names 'x' and 'y' 
% because many tools look for such datasets when needing coordinate 
% information. For example, "objget" looks for datasets called 'x' and 'y' 
% when asked to provide 'xglobal' or 'yglobal'.
% 	
%   In development are container objects which are designed to store an 
% arbitrary number of other objects and a number of other objects based on 
% containers such as wells, horizons, cross sections, ...
% 	
% C) Possible Settings in Random and Gridded Objects
%
%	Fields in random and gridded objects are set by calling "objset". The 
% syntax for callin "objset" is summarized as:
%	object = objset( objin, item, value);
%	object = objset( objin, item, value, datanum);
%	object = objset( objin, item, value, dataname);
%
%    OBJSET accepts an EarthObject (created by RANDOBJ, GRIDOBJ) and 
% sets the values of one of it's items. If the item already exists, then its 
% values are replaced; otherwise a new item is added to the object and its 
% values inserted. New items are assumed to be data (as opposed to headers) 
% and must be the same size and geometry as other data items already in the 
% object. The identity of header fields in the object is fixed so new header 
% information cannot be added; however, the existing headers can be changed 
% (updated) at any point. A data item may be protected from accidental 
% overwrite by setting its preserve flag to 'on'. The first form of the 
% command is used most often and sets items that are not specific to a 
% particular dataset within the object. 
%
% Examples: (let myObj be an existing object)
%    myObj = objset(myObj, 'username', 'barney'); ... change the username
%            to barney
%  yourObj = objset(myObj,'username','barney'); ... as above but create a
%            new object
%    myObj = objset(myObj,' Leduc ',newGrid); ... a grid referred to by the
%            matlab variable "newGrid" is put in the object and named ' Leduc '
%    myObj = objset(myObj,'protect','on',' Leduc '); ... the ' Leduc ' grid 
%            is protected.
%    myObj = objset(myObj,'protect','off',' Leduc '); ... the ' Leduc ' grid 
%            is unprotected.
%    myObj = objset(myObj,'protect','on',3); ... the third grid is 
%            unprotected.
%
% Argument definitions:
%    objin = the variable name of the object to be updated
%     item = a string specifying the item to be set (max of 30 characters)
%    value = the value(s) of the item
%  datanum = the sequential number of one of the data fields in the 
%            object
% dataname = string giving the name of one of the data fields in the 
%            object
%   object = the output object. Normally this will be the same matrix as 
%            objin
% 
%	Possible specifications for item strings include any of the 
% following:
% string ........... field which will be set
%--------------------------------------------------------------
% 'objvernum' .... a version number referring to the object software which
%	created the object. Older objects may not work directly with newer 
%	software and will require conversion. You should not attempt to 
%	change this value.
% 'objcreate' .... object creation date returned as a sequence of 6 integers.
%	The key is: yy mm dd hh mn ss where yy is year, mm is month, dd is 
%	day, hh is hour, mn is minute, and ss is seconds. GRIDOBJ and 
%	RANDOBJ set this field automatically when an object is created. The 
%	matlab command: fix(clock) will return a six integer field 
%	appropriate for any object date field.
% 'objmodified' .... date the object was last modified. OBJSET will update 
%	this automatically anytime a field in the object is set or a new one 
%	added. See 'objcreate' above for a description of the date format.
% 'objtype' .... short string giving the object type (grid or rand etc) (Set 
%	automatically by GRIDOBJ and RANDOBJ)
% 'datatype'.... short string giving the data type (seis, mag, grav etc)
%	(Usually set by GRIDOBJ or RANDOBJ)
% 'username' .... 6 character string giving a user id assicuated with the 
%	object.
% 'name'.... possibly long string giving the object name
%	(Usually set by GRIDOBJ or RANDOBJ)
% 'xaxisname' ... max of 10 characters giving the name of the x axis
% 'yaxisname' ... max of 10 characters giving the name of the y axis
% 'north' ... azimuth angle in degrees of true north. 0 is along the
%	x axis and 90 along the y axis
% 'east' ... azimuth angle in degrees of true east.
% 'datacols'.... the number of columns in the data matrix
%	(Usually set by GRIDOBJ or RANDOBJ)
% 'datarows'.... the number of rows in the data matrix
%	(Usually set by GRIDOBJ or RANDOBJ)
% 'delx'.... the spacing between columns in physical units
%	(Usually set by GRIDOBJ, not used by RANDOBJ)
% 'dely'.... the spacing between rows in physical units
%	(Usually set by GRIDOBJ, not used by RANDOBJ)
% 'xnot'.... the coordinate of the first column in physical units
%	(Usually set by GRIDOBJ, not used by RANDOBJ)
% 'ynot'.... the coordinate of the first row in physical units
%	(Usually set by GRIDOBJ, not used by RANDOBJ)
% 'globorigin' .... a 1 by 2 vector giving global coordinates [x,y] for the 
%	data origin. See discussion of local and global coordinates below.
% 'xyscale' .... scalar relating local and global coordinates
% 'xyrotate' .... rotation angle (degrees) relating local and global 
%                 coordinates.
%
% Additionally, any of the strings returned by objget(anyObject,'fieldnames') 
% or objget(anyobject,'namesmatrix') may be used as item strings. For 
% example, suppose objget(myObject,'fieldnames') returns the string: 
% wabamun|peace river|leduc|nordegg
% Then, the data grid named 'peace river' may be set by
%	myObject = objget(myObject,'peace river', peaceGrid); ... peaceGrid is 
%		a matlab matrix containing the z values of the grid 'peace 
%		river'
%
% There are four other items which are specific to each data field and can be 
% set using the 4 argument syntax for objset. These are:
%
% 'dataname' .... the name of a data field. This is useful when you want to 
%	change the name of an existing data field:
%	myObject = objget(myObject,'dataname','Peace River','peace river');
%		will cause the datafield known as 'peace river' to henceforth
%		be 'Peace River'
%	myObject = objget(myObject,'dataname','Merry Christmas',5);
%		will cause the fifth data field to be named 'Merry Christmas'
% 'datacreate' .... the creation data of a data field. Set automatically by 
%	OBJSET whenever a new data field is added. (See 'objcreate' above 
%	for a description of the date format.)
% 'datamodified' .... the date the data field was last modified. Updated 
%	automatically by OBJSET whenever a datafields values are changed.
%	(See 'objcreate' above for a description of the date format.)
%
% D)  Local and Global Coordinates in Earth Objects 
%
%   Generally, resource exploration requires maps using a global 
% coordinate system such as UTM; however, because these coordinates are 
% typically numbers around a million or more, many numerical algorithms 
% will loose precision or even abort if such coordinates are used. To cope 
% with this problem, Earth Objects are designed to store a local coordinate 
% system, typically numbers between 0 and 100, as well as the information 
% necessary to compute global coordinates whenever necessary. If x and y 
% represent such local coordinates and xgnot, ygnot, xysc, and xyrot are the 
% numbers stored in 'globorigin', 'xyscale', and 'xyrotate' respectively, then 
% the global coordinates xglob and yglob are related to x and y through:
%
%	x = (xglob-xgnot)/xyscale  
%	and  
%	y=(yglob-ygnot)/xyscale ... if xyrot == 0.0
%	
%	or, if xyrot is non-zero:
%	
%	x =  cos(theta)*(xglob-xgnot)/xyscale 
%		+ sin(theta)*(yglob-ygnot)/xyscale
%	and
%	y = -sin(theta)*(xglob-xgnot)/xyscale 
%		+ cos(theta)*(yglob-ygnot)/xyscale
%	where theta = pi*xyrot/180.
%
%   So, the strategy is to store x,y coordinates in the object that are 
% local together with the information necessary to recompute the global 
% coordinates and use objget to obtain the global (or local) coordinates 
% whenever needed.
%
% E) Information Retrieval from Random and Gridded Objects with "objget"
%
% 	itemValue = objget(object, item)
% 	itemvalue = objget(object, item, datanum)
% 	itemvalue = objget(object, item, dataname)
%
%    OBJGET performs the inverse operation to OBJSET in that it 
% retrieves data from an EarthObject (created by RANDOBJ, GRIDOBJ) Most 
% invocations will use the first syntax. 
% Examples: 
%	objget(myobject,'name') ... returns the object's name
%	objget(myobject,'x') ... returns the object's x coordinates
%	objget(myobject,'fred') ... returns the data field named fred
%	objget(myobject,5) ... returns the fifth dataset in the object
%
%   The second syntax is provided to obtain fields specific to of a 
% dataset when only its name or number are known. For example, the name of 
% the 5th dataset is obtained with:
%	objget(myobject,'dataname',5) ... will return the name of the 5th data
%		grid 
% If the returned name is 'peace river' for example, then its last 
% modification date can be obtained by either of:
%	objget(myobject,'datamodified',5); ... 'peace river' is the 5th dataset
%	objget(myobject,'datamodified','peace river'); ... ask for it my name
%
% Argument definitions:
%	object = the EarthObject to be interrogated
%	item = a string identifying the item to be retrieved, or an integer
%		denoting the index of the item. This last case is useful if 
%		a query such as objget( object, 'fieldnames') has first been
%		made to determine the names of available fields in the object.
%		Any field can then be obtained by simply specifying it's number
%		(the first is number 1 etc...)
%	datanum = an integer referring to one of the data fields in the 
%		object. Valid numbers are 1 through m where m is the number 
%		of data fields stored. (and the number of rows returned by 
%		objget(myObject,'namesmatrix') )
%	dataname = a string giving the name of a dataset stored in the 
%		object. Blanks are important: the string 'leduc' will not match 
%		' leduc' or 'leduc '
%
%	itemValue =  the returned value of the item. The form of itemValue
%		may be any of scalar, vector, or matrix depending on what was 
%		stored.
%
%
%    Possible specifications for item strings include any of the 
% following:
% string ........... returned itemValue
% ------------------------------------------------------------
% 'fieldnames' .... a string vector containing the names of the data
%	fields in the object. The names are separated by
%	the | character.
% 'namesmatrix' ... a matrix of strings containing the field names. The
%	names of each data field are rows in the matrix.
% 'objvernum' .... a version number referring to the object software which
%	created the object. Older objects may not work directly with newer 
%	software and will require conversion.
% 'objcreate' .... object creation date returned as a sequence of 6 integers. 
%	The key is: yy mm dd hh mn ss where yy is year, mm is month, dd is 
%	day, hh is hour mn is minute, and ss is seconds. Thus a returned date 
% 	of 1993 11 4 13 20 16 is November 4, 1993 at 1:20:16 pm
% 'objmodified' .... date the object was last modified. 
%	See 'objcreate' above for a description of the date format.
% 'objtype' .... short string giving the object type (grid or rand etc)
% 'datatype'.... short string giving the data type (seis, mag, grav etc)
% 'username' .... 6 character string giving a user id assicuated with the 
% 	object
% 'name'.... possibly long string giving the object name
% 'xaxisname' ... max of 10 characters giving the name of the x axis
% 'yaxisname' ... max of 10 characters giving the name of the y axis
% 'north' ... azimuth angle in degrees of true north. 0 is along the
%	x axis and 90 along the y axis
% 'east' ... azimuth angle in degrees of true east.
% 'datacols'.... the number of columns in the data matrix
% 'datarows'.... the number of rows in the data matrix
% 'delx'.... the spacing between columns in physical units (local coords)
%	Not relevent for Random objects
% 'dely'.... the spacing between rows in physical units (local coords)
%	Not relevent for Random objects
% 'xnot'... the coordinate of the first column in physical units (local coords)
%	Not relevent for Random objects
% 'ynot'.... the coordinate of the first row in physical units (local coords)
%	Not relevent for Random objects
% 'globorigin' .... a vector of length 2 giving the coordinates of the first row 
%	and column in global coordinates
% 'xyscale' .... a scalar giving the scale factor between local and global 
%	coordinates.If local coordinates are kilometers and global are 
%	meters, then this should be 1000.
% 'xyrotate' .... a scalar giving the rotation angle between local and global 
%	coordinates in degrees
% 'x' .... a vector of the local column coordinates in physical units
%	For gridded objects, this is generated automatically from xnot and 
%	delx. For random objects, it is only available if a dataset named 'x' 
%	has been set.
% 'xglobal' .... a vector giving the global column coordinates.
% 'y' .... a vector of the local row coordinates in physical units
%	For gridded objects, this is generated automatically from xnot and 
%	delx. For random objects, it is only available if a dataset named 'x' 
%	has been set.
% 'xglobal' .... a vector giving the global column coordinates.
% 'xg' ... a grid the same size as the data with the local x coordinates 
%          of each point. Only available for gridded objects
% 'yg' ... a grid the same size as the data with the local y coordinates 
%          of each point. Only available for gridded objects
%
% Additionally, any of the strings returned by 'fieldnames' or 'namesmatrix' 
% maybe used as item strings. For example, suppose 
% objget(myObject,'fieldnames')
% returns the string: wabamun|peace river|leduc|nordegg. Then, the dataset 
% named 'peace river' may be stored in the Matlab variable z by either of
%	z = objget(myObject,'peace river'); ... ask for it by name
%	z = objget(myObject,2); ... because its the second dataset
%
% There are four other items which are specific to each dataset and can be
% retrieved using the 3 argument syntax for objget. These are:
% 
% 'dataname' .... the name of a dataset. This is useful when you know the 
%	sequential dataset number and need the name. For example, using the 
%	object referred to above, 
%	objget(myObject,'dataname',3) 
%	will return the string 'leduc'
% 'datacreate' .... the creation data of a dataset.
%	(See 'objcreate' above for a description of the date format)
% 'datamodified' .... the date the dataset was last modified.
%	(See 'objcreate' above for a description of the date format.)
% 'protect' ... the data protection flag. 1 indicates the dataset is protected 
%	and may not be overwritten, while 0 indicates no protection. Of 
%	course, protected data can be unprotected using objset.
%
%
% For further information contact Gary Margrave, CREWES Project, University
% of Calgary
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