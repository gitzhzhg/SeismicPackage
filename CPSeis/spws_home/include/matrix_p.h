/*<license>
 -------------------------------------------------------------------------------
  Copyright (c) 2007 ConocoPhillips Company
 
  Permission is hereby granted, free of charge, to any person obtaining a copy
  of this software and associated documentation files (the "Software"), to deal
  in the Software without restriction, including without limitation the rights
  to use, copy, modify, merge, publish, distribute, sublicense, and/or sell
  copies of the Software, and to permit persons to whom the Software is
  furnished to do so, subject to the following conditions:
 
  The above copyright notice and this permission notice shall be included in all
  copies or substantial portions of the Software.
 
  THE SOFTWARE IS PROVIDED "AS IS", WITHOUT WARRANTY OF ANY KIND, EXPRESS OR
  IMPLIED, INCLUDING BUT NOT LIMITED TO THE WARRANTIES OF MERCHANTABILITY,
  FITNESS FOR A PARTICULAR PURPOSE AND NONINFRINGEMENT. IN NO EVENT SHALL THE
  AUTHORS OR COPYRIGHT HOLDERS BE LIABLE FOR ANY CLAIM, DAMAGES OR OTHER
  LIABILITY, WHETHER IN AN ACTION OF CONTRACT, TORT OR OTHERWISE, ARISING FROM,
  OUT OF OR IN CONNECTION WITH THE SOFTWARE OR THE USE OR OTHER DEALINGS IN THE
  SOFTWARE.
 -------------------------------------------------------------------------------
 </license>*/
/*
 * Copyright(c) 1992 Bell Communications Research, Inc. (Bellcore)
 *                        All rights reserved
 * Permission to use, copy, modify and distribute this material for
 * any purpose and without fee is hereby granted, provided that the
 * above copyright notice and this permission notice appear in all
 * copies, and that the name of Bellcore not be used in advertising
 * or publicity pertaining to this material without the specific,
 * prior written permission of an authorized representative of
 * Bellcore.
 *
 * BELLCORE MAKES NO REPRESENTATIONS AND EXTENDS NO WARRANTIES, EX-
 * PRESS OR IMPLIED, WITH RESPECT TO THE SOFTWARE, INCLUDING, BUT
 * NOT LIMITED TO, THE IMPLIED WARRANTIES OF MERCHANTABILITY AND
 * FITNESS FOR ANY PARTICULAR PURPOSE, AND THE WARRANTY AGAINST IN-
 * FRINGEMENT OF PATENTS OR OTHER INTELLECTUAL PROPERTY RIGHTS.  THE
 * SOFTWARE IS PROVIDED "AS IS", AND IN NO EVENT SHALL BELLCORE OR
 * ANY OF ITS AFFILIATES BE LIABLE FOR ANY DAMAGES, INCLUDING ANY
 * LOST PROFITS OR OTHER INCIDENTAL OR CONSEQUENTIAL DAMAGES RELAT-
 * ING TO THE SOFTWARE.
 *
 * MatrixWidget Author: Andrew Wason, Bellcore, aw@bae.bellcore.com
 */

/*
 * MatrixP.h - Private definitions for Matrix widget
 */

#ifndef _Xbae_MatrixP_h
#define _Xbae_MatrixP_h

/*
 *  "@(#)MatrixP.h	3.11 7/8/92"
 */

#include <Xm/XmP.h>
#include <matrix.h>


/*
 * New types for the class methods
 */

#if defined (__cplusplus) || defined(c_plusplus)
#define CONST const
extern "C" {
#else
#define CONST
#endif

typedef void (*XbaeMatrixSetCellProc)(
#if NeedFunctionPrototypes
				      Widget		/* w */,
				      int		/* row */,
				      int		/* column */,
				      CONST String	/* value */
#endif
				      );

typedef String (*XbaeMatrixGetCellProc)(
#if NeedFunctionPrototypes
					Widget	/* w */,
					int	/* row */,
					int	/* column */
#endif
					);

typedef void (*XbaeMatrixEditCellProc)(
#if NeedFunctionPrototypes
				       Widget	/* w */,
				       int	/* row */,
				       int	/* column */
#endif
				       );

typedef void (*XbaeMatrixSelectCellProc)(
#if NeedFunctionPrototypes
					 Widget	/* w */,
					 int	/* row */,
					 int	/* column */
#endif
					 );

typedef void (*XbaeMatrixSelectRowProc)(
#if NeedFunctionPrototypes
					 Widget	/* w */,
					 int	/* row */
#endif
					 );

typedef void (*XbaeMatrixSelectColumnProc)(
#if NeedFunctionPrototypes
					 Widget	/* w */,
					 int	/* column */
#endif
					 );

typedef void (*XbaeMatrixDeselectAllProc)(
#if NeedFunctionPrototypes
					 Widget	/* w */
#endif
					 );

typedef void (*XbaeMatrixDeselectCellProc)(
#if NeedFunctionPrototypes
					   Widget/* w */,
					   int	/* row */,
					   int	/* column */
#endif
					 );

typedef void (*XbaeMatrixDeselectRowProc)(
#if NeedFunctionPrototypes
					   Widget/* w */,
					   int	/* row */
#endif
					 );

typedef void (*XbaeMatrixDeselectColumnProc)(
#if NeedFunctionPrototypes
					   Widget/* w */,
					   int	/* column */
#endif
					 );

typedef Boolean (*XbaeMatrixCommitEditProc)(
#if NeedFunctionPrototypes
					    Widget	/* w */,
					    Boolean	/* unmap */
#endif
					    );

typedef void (*XbaeMatrixCancelEditProc)(
#if NeedFunctionPrototypes
					 Widget  /* w */,
					 Boolean/* unmap */
#endif
					 );

typedef void (*XbaeMatrixAddRowsProc)(
#if NeedFunctionPrototypes
				      Widget	/* w */,
				      int	/* position */,
				      String * 	/* rows */,
				      String * 	/* labels */,
				      Pixel * 	/* colors */,
				      int 	/* num_rows */
#endif
				      );

typedef void (*XbaeMatrixDeleteRowsProc)(
#if NeedFunctionPrototypes
					 Widget	/* w */,
					 int	/* position */,
					 int	/* num_rows */
#endif
					 );

typedef void (*XbaeMatrixAddColumnsProc)(
#if NeedFunctionPrototypes
					 Widget		/* w */,
					 int		/* position */,
					 String *	/* columns */,
					 String *	/* labels */,
					 short *	/* widths */,
					 int *		/* max_lengths */,
					 unsigned char* /* alignments */,
					 unsigned char* /* label_alignments */,
					 Pixel *	/* colors */,
					 int		/* num_columns */
#endif
					 );

typedef void (*XbaeMatrixDeleteColumnsProc)(
#if NeedFunctionPrototypes
					    Widget	/* w */,
					    int		/* position */,
					    int		/* num_columns */
#endif
					    );

typedef void (*XbaeMatrixSetRowColorsProc)(
#if NeedFunctionPrototypes
					   Widget	/* w */,
					   int		/* position */,
					   Pixel *	/* colors */,
					   int		/* num_colors */
#endif
					   );

typedef void (*XbaeMatrixSetColumnColorsProc)(
#if NeedFunctionPrototypes
					      Widget	/* w */,
					      int	/* position */,
					      Pixel *	/* colors */,
					      int	/* num_colors */
#endif
					      );

typedef void (*XbaeMatrixSetCellColorProc)(
#if NeedFunctionPrototypes
					      Widget	/* w */,
					      int	/* row */,
					      int	/* column */,
					      Pixel 	/* color */
#endif
					      );

#if defined (__cplusplus) || defined(c_plusplus)
}
#endif
#undef CONST

/*
 * Different than the traversal directions in Xm.h
 */
#define NOT_TRAVERSING	-1

/*
 * New fields for the Matrix widget class record
 */
typedef struct {
    XbaeMatrixSetCellProc		set_cell;
    XbaeMatrixGetCellProc		get_cell;
    XbaeMatrixEditCellProc		edit_cell;
    XbaeMatrixSelectCellProc		select_cell;
    XbaeMatrixSelectRowProc		select_row;
    XbaeMatrixSelectColumnProc		select_column;
    XbaeMatrixDeselectAllProc		deselect_all;
    XbaeMatrixDeselectCellProc		deselect_cell;
    XbaeMatrixDeselectRowProc		deselect_row;
    XbaeMatrixDeselectColumnProc	deselect_column;
    XbaeMatrixCommitEditProc		commit_edit;
    XbaeMatrixCancelEditProc		cancel_edit;
    XbaeMatrixAddRowsProc		add_rows;
    XbaeMatrixDeleteRowsProc		delete_rows;
    XbaeMatrixAddColumnsProc		add_columns;
    XbaeMatrixDeleteColumnsProc		delete_columns;
    XbaeMatrixSetRowColorsProc		set_row_colors;
    XbaeMatrixSetColumnColorsProc	set_column_colors;
    XbaeMatrixSetCellColorProc		set_cell_color;
    XtPointer				extension;
} XbaeMatrixClassPart;

/*
 * Full class record declaration
 */
typedef struct _XbaeMatrixClassRec {
    CoreClassPart		core_class;
    CompositeClassPart		composite_class;
    ConstraintClassPart		constraint_class;
    XmManagerClassPart		manager_class;
    XbaeMatrixClassPart		matrix_class;
} XbaeMatrixClassRec;

extern XbaeMatrixClassRec xbaeMatrixClassRec;


/*
 * Inheritance constants for set/get/edit methods
 */
#define XbaeInheritGetCell ((XbaeMatrixGetCellProc) _XtInherit)
#define XbaeInheritSetCell ((XbaeMatrixSetCellProc) _XtInherit)
#define XbaeInheritEditCell ((XbaeMatrixEditCellProc) _XtInherit)
#define XbaeInheritSelectCell ((XbaeMatrixSelectCellProc) _XtInherit)
#define XbaeInheritSelectRow ((XbaeMatrixSelectRowProc) _XtInherit)
#define XbaeInheritSelectColumn ((XbaeMatrixSelectColumnProc) _XtInherit)
#define XbaeInheritDeselectAll ((XbaeMatrixDeselectAllProc) _XtInherit)
#define XbaeInheritDeselectCell ((XbaeMatrixDeselectCellProc) _XtInherit)
#define XbaeInheritDeselectRow ((XbaeMatrixDeselectRowProc) _XtInherit)
#define XbaeInheritDeselectColumn ((XbaeMatrixDeselectColumnProc) _XtInherit)
#define XbaeInheritCommitEdit ((XbaeMatrixCommitEditProc) _XtInherit)
#define XbaeInheritCancelEdit ((XbaeMatrixCancelEditProc) _XtInherit)
#define XbaeInheritAddRows ((XbaeMatrixAddRowsProc) _XtInherit)
#define XbaeInheritDeleteRows ((XbaeMatrixDeleteRowsProc) _XtInherit)
#define XbaeInheritAddColumns ((XbaeMatrixAddColumnsProc) _XtInherit)
#define XbaeInheritDeleteColumns ((XbaeMatrixDeleteColumnsProc)_XtInherit)
#define XbaeInheritSetRowColors ((XbaeMatrixSetRowColorsProc)_XtInherit)
#define XbaeInheritSetColumnColors ((XbaeMatrixSetColumnColorsProc)_XtInherit)
#define XbaeInheritSetCellColor ((XbaeMatrixSetCellColorProc)_XtInherit)


/*
 * New data structures for the ScrollMgr code
 */
typedef struct _SmScrollNode {
    int x;
    int y;
    struct _SmScrollNode *next;
    struct _SmScrollNode *prev;
} SmScrollNodeRec, *SmScrollNode;

typedef struct _SmScrollMgr {
    int offset_x;
    int offset_y;
    int scroll_count;
    SmScrollNode scroll_queue;
    Boolean scrolling;
} SmScrollMgrRec, *SmScrollMgr;


/*
 * Data structure for column labels
 */
typedef struct _ColumnLabelLines {
    int lines;
    int *lengths;
} ColumnLabelLinesRec, *ColumnLabelLines;


/*
 * New fields for the Matrix widget record
 */
typedef struct {
    /* resources */
    int		rows, columns;	    /* number of cells per row/column */
    Dimension	fixed_rows,	    /* number of fixed rows OR columns */
		fixed_columns;
    Pixel	**colors;	    /* 2D array of Pixels */
    Dimension	visible_columns;    /* number of columns to make visible */
    Dimension	visible_rows;	    /* number of rows to make visible */
    short	*column_widths;	    /* width of each column in chars */
    int		*column_max_lengths;/* max length of each column in chars */
    short	row_label_width;    /* max width of row labels in chars */

    String	**cells;	    /* 2D array of strings */
    String	*row_labels;	    /* array of labels next to each row */
    String	*column_labels;	    /* array of labels above each column */
    unsigned char
		*column_alignments; /* alignment of each column */
    unsigned char
		*column_label_alignments; /* alignment of each column label */
    unsigned char
		row_label_alignment;/* alignment of row labels */

    XmFontList	font_list;	    /* fontList of widget and textField */

    Dimension	space;		    /* spacing for scrollbars */
    Boolean	bold_labels;	    /* draw bold row/column labels */

    Boolean	**selected_cells;   /* 2D array indicating selected cells */

    Dimension	cell_margin_width,  /* margin resources for textField */
		cell_margin_height;
    Dimension	cell_shadow_thickness; /* shadow thickness for textField */
    Dimension	cell_highlight_thickness; /* highlight thickness for
					   * textField */
    XtTranslations
		text_translations;  /* translations for textField widget */

    XtCallbackList
		modify_verify_callback; /* verify change to textField */
    XtCallbackList
		enter_cell_callback;/* called when a cell is entered */
    XtCallbackList
		leave_cell_callback;/* called when a cell is left */
    XtCallbackList
		traverse_cell_callback; /* determines the next cell to
					   traverse to */
    XtCallbackList
		select_cell_callback;/* called when cells are selected */

    int		top_row;	    /* vertical origin (in row space) */


    /* private state */
    int		horiz_origin;	    /* horizontal origin (in pixel space) */

    int		current_row,	    /* row/column we are editing */
		current_column;

    int		text_baseline;	    /* baseline of text in each cell */

    GC		draw_gc;	    /* GC for drawing cells/labels */
    GC		inverse_gc;	    /* GC for drawing selected cells */
    GC		draw_clip_gc;	    /* GC for clipped cells/labels */
    GC		inverse_clip_gc;    /* GC for clipped selected cells */
    GC		cell_top_shadow_clip_gc; /* GC for clipped top shadow */
    GC		cell_bottom_shadow_clip_gc; /* GC for clipped bottom shadow */

    XFontStruct	*font;		    /* fontStruct from fontList */

    int		*column_positions;  /* pixel position of each column */

    unsigned int cell_total_width;  /* width of cell area in pixels */

    int		cell_visible_height; /* height of visible cell area in pixels
					(not truncated to nearest row) */

    Dimension	desired_width,	    /* width/height widget wants to be */
		desired_height;

    SmScrollMgr matrix_scroll_mgr,  /* ScrollMgrs for Matrix and Clip */
		clip_scroll_mgr;

    int		traversing;	    /* set to direction we are traversing,
				     * or NOT_TRAVERSING */

    ColumnLabelLines
		column_label_lines; /* structs to handle multi line column
				     * labels */
    int		column_label_maxlines;
				    /* max # lines in column labels */

} XbaeMatrixPart;

/*
 * Full instance record declaration
 */
typedef struct _XbaeMatrixRec {
    CorePart		core;
    CompositePart	composite;
    ConstraintPart	constraint;
    XmManagerPart	manager;
    XbaeMatrixPart	matrix;
} XbaeMatrixRec;

#endif /* _Xbae_MatrixP_h */
