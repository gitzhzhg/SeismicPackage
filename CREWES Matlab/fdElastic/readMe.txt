Instructions for running Matlab finite-difference code mainline mainFD2d.m

Download .m files to a Matlab library

In MyDocuments create a modelling folder named FDmodels (now coded in mainFD2d)
 In this folder create project folders (with descriptive names)
    You put your input files in these, and any output will end up there too

Download .parm and .geo files into a project folder (as patterns)

feature jit off - This typed in before running often reduces execution time to 1/10

mainFD2d starts the program, and presents a GUI where a project folder is chosen

Subsequent mainFD2d calls continue the program from the same folder

Try the second menu item to present the geological (.geo) model in various ways

clear modelDir (before mainFD2d)- will allow a different project file to be selected




The .geo and .parm files specify all aspects of the finite-difference modelling
	(except the model may be continued with more time steps)

  The presentation of model results is specified by menu items


Format of the data files:
	% marks the end of data items and the start of comments
	% at the start of a line indicates all comments

Format of the .parm file:
	The first item on a line is the variable name specified
		the spelling must be exact
	The second item (after a space) is the variable value
	Numeric values are assumed to be floating point
	String values (file names) are enclosed in single quotes

Format of the .geo file:
	Horizons; Vp, Vs, rho, then X/Z pairs (no interpolation)
		Each Vp, Vs and rho apply below the horizon (until the next)
	Boxes: 	Must follow horizons.
		Have many corners and close from last to first vertex
		Their Vp, Vs and rho values apply inside,
		and each overrides anyting specified earlier