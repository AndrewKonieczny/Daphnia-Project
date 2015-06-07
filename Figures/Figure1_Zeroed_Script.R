# Figure1_1Zeroed
# Bobby McDonnell
# May 22, 2015

Calibration_Data <- 
  # Reads a file in table format and creates a data frame from it, with cases
  # corresponding to lines and variables to fields in the file.
  read.csv( # See http://www.inside-r.org/r-doc/utils/read.table    
    file = paste0( # See http://www.inside-r.org/r-doc/base/paste
      "~/Documents/MyFolders/DaphniaLab/Raw Data/Calibration/", 
      "RawFiberCalibrationData.csv"
    )
  )

Calibration <- 
  # This function creates data frames, tightly coupled collections of variables
  # which share many of the properties of matrices and of lists, used as the
  # fundamental data structure by most of R's modeling software.
  data.frame(
    # Take a sequence of vector, matrix or data frames arguments and combine by
    # columns (cbind) or rows (rbind), respectively. These are generic functions
    # with methods for other R classes.
    cbind( # See http://www.inside-r.org/r-doc/base/cbind
      Calibration_Data$Disp.Y,  Calibration_Data$Force..uN.
    )
  )

# Functions to get or set the names of an object.
# Here the function is setting the names of the columns.
names( # See http://www.inside-r.org/r-doc/base/names
  x = Calibration
  ) <- c("Y", "Force")

# NA values in the calibration data set are a result of ImageJ being unable to
# distinguish between the black mark on the fiber and the black background of
# the setup. So ImageJ thought of the fiber as two objects which each had
# centroids of their own. ImageJ then produced two rows, since it believed there
# were two objects, in the output. The two (x,y) sets from ImageJ were averaged,
# and an NA value was placed in one of the rows while the averaged position was
# placed in the other row. So NA_Valued_Data is a vector of the spacific points
# which have NA values which are then removed from the Force vector. If this 
# procedure were not done, there would be additional (meaningless) zeros in the
# Force vector. The aDataSet$aCol[-NA_Valued_Data] expression means everything
# in the vector, aDataSet$aCol is a vector from a data frame (aDataSet), except
# for the specified columns.
NA_Valued_Data <- 
  which( # See http://www.inside-r.org/r-doc/base/which
    is.na( # See http://www.inside-r.org/r-doc/base/is.na
      Calibration$Y
    )
  )
Displacement <- Calibration$Y[-NA_Valued_Data]
Force <- Calibration$Force[-NA_Valued_Data]

lm.data <- 
  glm( # See http://www.inside-r.org/r-doc/stats/glm
    formula = Force ~ Displacement + 0
    #   formula: An object of class "formula" (or one that can be coerced to
    #            that class): a symbolic description of the model to be fitted.
    #            The details of model specification are given under ‘Details’.
  )


# textSize has nothing to do with point size, it is just a value to keep the
# text sizes consistant
textSize <- 10 



# unhash the png function line to output a png of the plot, or unhash the pdf 
# function line to output a pdf of the plot

# # Graphics devices for JPEG, PNG or TIFF format bitmap files.
# png(
# filename = "Figure1_Zeroed_Cal_Plot.png", 
# # filename: The name of the output file. The page number is substituted if a C
# #           integer format is included in the character string, as in the 
# #           default. (The result must be less than PATH_MAX characters long,
# #           and may be truncated if not. See postscript for further details.)
# #           Tilde expansion is performed where supported by the platform.
#     family = "Helvetica",
# #   family: The font family to be used, see postscript. Default "Helvetica". 
#     height = 2400
# #   height: The width and height of the graphics region in inches. The  
# #           default values are 7.
#     width = 2400, 
# #   width: The width and height of the graphics region in inches. The default 
# #          values are 7.
# )

# pdf starts the graphics device driver for producing PDF graphics.
pdf( # notes below from: http://www.inside-r.org/r-doc/grDevices/pdf
  file = "Figure1_Zeroed_Cal_Plot.pdf", 
  #   file: A character string giving the name of the file. If it is of the form
  #         "|cmd", the output is piped to the command given by cmd. If it is 
  #         NULL, then no external file is created (effectively, no drawing 
  #         occurs), but the device may still be queried (e.g., for size of 
  #         text). For use with onefile = FALSE give a C integer format such as 
  #         "Rplot%03d.pdf" (the default in that case). (See postscript for 
  #         further details.)
  #         Tilde expansion (see path.expand) is done.
  height = 37,
  #   height: The width and height of the graphics region in inches. The default
  #           values are 7.
  width = 37, 
  #   width: The width and height of the graphics region in inches. The default 
  #          values are 7.
  family = "Helvetica"
  #   family: The font family to be used, see postscript. Defaults to 
  #           "Helvetica".
)

# par can be used to set or query graphical parameters. Parameters can be set by
# specifying them as arguments to par in tag = value form, or by passing them as
# a list of tagged values.
par( # notes below from: http://www.inside-r.org/r-doc/graphics/par
  cex.axis = textSize * 0.75,
  #   cex.axis: The magnification to be used for axis annotation relative to the
  #             current setting of cex.
  col = "black", 
  #   col: A specification for the default plotting color. See section ‘Color 
  #        Specification’.
  #        Some functions such as lines and text accept a vector of values which
  #        are recycled and may be interpreted slightly differently.
  las = 1, 
  #   las: Numeric in {0,1,2,3}; the style of axis labels.
  #        0: always parallel to the axis [default],
  #        1: always horizontal,
  #        2: always perpendicular to the axis,
  #        3: always vertical.
  #        Also supported by mtext. Note that string/character rotation via 
  #        argument srt to par does not affect the axis labels.
  lty = "solid", 
  #   lty: The line type. Line types can either be specified as an integer (0 =
  #        blank, 1 = solid (default), 2 = dashed, 3 = dotted, 4 = dotdash, 5 =
  #        longdash, 6 = twodash) or as one of the character strings "blank", 
  #        "solid", "dashed", "dotted", "dotdash", "longdash", or "twodash", 
  #        where"blank" uses ‘invisible lines’ (i.e., does not draw them).
  #        Alternatively, a string of up to 8 characters (from c(1:9, "A":"F"))
  #        may be given, giving the length of line segments which are drawn and
  #        skipped. See section ‘Line Type Specification’.
  #        Functions such as lines and segments accept a vector of values which
  #        are recycled.
  mgp = c(1,6,6), 
  #   mpg: The margin line (in mex units) for the axis title, axis labels and  
  #        axis line. Note that mgp[1] affects title whereas mgp[2:3] affect  
  #        axis. The default is c(3, 1, 0).
  oma = c(bottom = 20, left = 20, top = 3, right = 1), 
  #   oma: A vector of the form c(bottom, left, top, right) giving the size of
  #        the outer margins in lines of text.
  pin = c(width = 30, height = 30), 
  #   pin: The current plot dimensions, (width, height), in inches.
  ylbias = 0.8
  #   ylbias: A positive real value used in the positioning of text in the  
  #           margins by axis and mtext. The default is in principle device-
  #           specific, but currently 0.2 for all of Rs own devices. Set this to
  #           0.2 for compatibility with R < 2.14.0 on x11 and windows()
  #           devices.
)

# Generic function for plotting of R objects. For more details about the 
# graphical parameter arguments, see par.
# For simple scatter plots, plot.default will be used. However, there are plot
# methods for many R objects, including functions, data.frames, density objects,
# etc. Use methods(plot) and the documentation for these.
plot( # notes below from: http://www.inside-r.org/r-doc/graphics/plot
  type = "p",
  #   type: What type of plot should be drawn. Possible types are
  #         "p" for points,
  #         "l" for lines,
  #         "b" for both,
  #         "c" for the lines part alone of "b",
  #         "o" for both ‘overplotted’,
  #         "h" for ‘histogram’ like (or ‘high-density’) vertical lines,
  #         "s" for stair steps,
  #         "S" for other steps, see ‘Details’ below,
  #         "n" for no plotting.
  #         All other types give a warning or an error; using, e.g., type =  
  #         "punkte" being equivalent to type = "p" for S compatibility. Note  
  #         that some methods, e.g. plot.factor, do not accept this.
  x = Displacement, 
  #   x: The coordinates of points in the plot. Alternatively, a single plotting
  #      structure, function or any R object with a plot method can be provided.
  xlab = "", 
  #   xlab: A title for the x axis: see title.
  y = Force,
  #   y: The y coordinates of points in the plot, optional if x is an 
  #      appropriate structure.
  ylab = "", 
  #   ylab: A title for the y axis: see title. 
  
  # notes below from: http://www.inside-r.org/r-doc/graphics/par
  bty = "n", 
  #   bty: A character string which determined the type of box which is drawn
  #        about plots. If bty is one of "o" (the default), "l", "7", "c", "u",
  #        or "]" the resulting box resembles the corresponding upper case 
  #        letter. A value of "n" suppresses the box.
  cex = 9, 
  #   cex: A numerical value giving the amount by which plotting text and 
  #        symbols should be magnified relative to the default. This starts as
  #        1 when a device is opened, and is reset when the layout is changed,
  #        e.g. by setting mfrow.
  #        Note that some graphics functions such as plot.default have an 
  #        argument of this name which multiplies this graphical parameter, and
  #        some functions such as points and text accept a vector of values
  #        which are recycled.
  lwd = 5, 
  #   lwd: The line width, a positive number, defaulting to 1. The 
  #        interpretation is device-specific, and some devices do not implement
  #        line widths less than one. (See the help on the device for details of
  #        the interpretation.)
  #        Functions such as lines and segments accept a vector of values which
  #        are recycled: in such uses lines corresponding to values NA or NaN
  #        are omitted. The interpretation of is device-specific.
  xaxs = "i", 
  #   xaxs: The style of axis interval calculation to be used for the x-axis.
  #         Possible values are "r", "i", "e", "s", "d". The styles are 
  #         generally controlled by the range of data or xlim, if given.
  #         Style "r" (regular) first extends the data range by 4 percent at 
  #         each end and then finds an axis with pretty labels that fits within
  #         the extended range.
  #         Style "i" (internal) just finds an axis with pretty labels that fits
  #         within the original data range.
  #         Style "s" (standard) finds an axis with pretty labels within which 
  #         the original data range fits.
  #         Style "e" (extended) is like style "s", except that it is also 
  #         ensures that there is room for plotting symbols within the bounding
  #         box.
  #         Style "d" (direct) specifies that the current axis should be used on
  #         subsequent plots.
  #         (Only "r" and "i" styles have been implemented in R.)
  xpd = T, 
  #   xpd: A logical value or NA. If FALSE, all plotting is clipped to the plot
  #        region, if TRUE, all plotting is clipped to the figure region, and if
  #        NA, all plotting is clipped to the device region. See also clip.
  yaxs = "i", 
  #   yaxs: The style of axis interval calculation to be used for the y-axis.
  #         See xaxs above.
  
  # notes below from: http://www.inside-r.org/r-doc/graphics/plot.default
  axes = F
  #   axes: a logical value indicating whether both axes should be drawn on the
  #         plot. Use graphical parameter "xaxt" or "yaxt" to suppress just one
  #         of the axes.
)

# Generic function to add a suitable axis to the current plot.
# This is the major tick marks for the x-axis
axis( # notes below from: http://www.inside-r.org/r-doc/graphics/axis
  at = seq(0, 500, 100), 
  #   at: The points at which tick-marks are to be drawn.
  line = 0, 
  #   line: on which MARgin line, starting at 0 counting outwards. See mtext.
  lwd = 16,  
  #   lwd: The line width, a positive number, defaulting to 1. The 
  #        interpretation is device-specific, and some devices do not implement
  #        line widths less than one. (See the help on the device for details of
  #        the interpretation.)
  #        Functions such as lines and segments accept a vector of values which
  #        are recycled: in such uses lines corresponding to values NA or NaN
  #        are omitted. The interpretation of is device-specific.
  padj = 1,
  #   padj: adjustment for each string perpendicular to the reading direction 
  #         (which is controlled by adj). For strings parallel to the axes, 
  #         padj = 0 means right or top alignment, and padj = 1 means left or 
  #         bottom alignment. See mtext.
  side = 1, 
  #   side: An integer specifying which side of the plot the axis is to be 
  #         drawn on. The axis is placed as follows: 1 = below, 2 = left, 
  #         3 = above and 4 = right.
  
  # notes below from: http://www.statmethods.net/advgraphs/axes.html
  tck = -0.02
  #   tck: length of tick mark as fraction of plotting region (negative number
  #        is outside graph, positive number is inside, 0 suppresses ticks, 1 
  #        creates gridlines) default is -0.01
)

# Generic function to add a suitable axis to the current plot.
# This is the major tick marks for the y-axis
axis( # notes below from: http://www.inside-r.org/r-doc/graphics/axis
  at = seq(0, 21, 5),
  #   at: The points at which tick-marks are to be drawn.
  label = NULL,
  #   label: a character vector of labels to be placed at the tickmarks 
  #          (if NULL, the at values will be used)
  line = 0, 
  #   line: on which MARgin line, starting at 0 counting outwards. See mtext.
  lwd = 16,  
  #   lwd: The line width, a positive number, defaulting to 1. The 
  #        interpretation is device-specific, and some devices do not implement
  #        line widths less than one. (See the help on the device for details of
  #        the interpretation.)
  #        Functions such as lines and segments accept a vector of values which
  #        are recycled: in such uses lines corresponding to values NA or NaN
  #        are omitted. The interpretation of is device-specific.
  side = 2, 
  #   side: An integer specifying which side of the plot the axis is to be 
  #         drawn on. The axis is placed as follows: 1 = below, 2 = left, 
  #         3 = above and 4 = right.
  
  # notes below from: http://www.statmethods.net/advgraphs/axes.html
  tck = -0.02
  #   tck: length of tick mark as fraction of plotting region (negative number
  #        is outside graph, positive number is inside, 0 suppresses ticks, 1 
  #        creates gridlines) default is -0.01
)

# Generic function to add a suitable axis to the current plot.
# This is the minor tick marks for the x-axis
axis( # notes below from: http://www.inside-r.org/r-doc/graphics/axis
  at = seq(0, 500, 25), 
  #   at: The points at which tick-marks are to be drawn.
  label = NA,
  #   label: a character vector of labels to be placed at the tickmarks 
  #          (if NULL, the at values will be used)
  line = 0, 
  #   line: on which MARgin line, starting at 0 counting outwards. See mtext.
  lwd = 8,  
  #   lwd: The line width, a positive number, defaulting to 1. The 
  #        interpretation is device-specific, and some devices do not implement
  #        line widths less than one. (See the help on the device for details of
  #        the interpretation.)
  #        Functions such as lines and segments accept a vector of values which
  #        are recycled: in such uses lines corresponding to values NA or NaN
  #        are omitted. The interpretation of is device-specific.
  side = 1, 
  #   side: An integer specifying which side of the plot the axis is to be 
  #         drawn on. The axis is placed as follows: 1 = below, 2 = left, 
  #         3 = above and 4 = right.
  
  # notes below from: http://www.statmethods.net/advgraphs/axes.html
  tck = -0.01
  #   tck: length of tick mark as fraction of plotting region (negative number
  #        is outside graph, positive number is inside, 0 suppresses ticks, 1 
  #        creates gridlines) default is -0.01
)

# Generic function to add a suitable axis to the current plot.
# This is the minor tick marks for the y-axis
axis( # notes below from: http://www.inside-r.org/r-doc/graphics/axis
  at = seq(0, 21, 1), 
  #   at: The points at which tick-marks are to be drawn.
  label = NA, 
  #   label: a character vector of labels to be placed at the tickmarks 
  #          (if NULL, the at values will be used)
  line = 0, 
  #   line: on which MARgin line, starting at 0 counting outwards. See mtext.
  lwd = 8, 
  #   lwd: The line width, a positive number, defaulting to 1. The 
  #        interpretation is device-specific, and some devices do not implement
  #        line widths less than one. (See the help on the device for details of
  #        the interpretation.)
  #        Functions such as lines and segments accept a vector of values which
  #        are recycled: in such uses lines corresponding to values NA or NaN
  #        are omitted. The interpretation of is device-specific.
  side = 2, 
  #   side: An integer specifying which side of the plot the axis is to be 
  #         drawn on. The axis is placed as follows: 1 = below, 2 = left, 
  #         3 = above and 4 = right.
  
  # notes below from: http://www.statmethods.net/advgraphs/axes.html
  tck = -0.01
  #   tck: length of tick mark as fraction of plotting region (negative number
  #        is outside graph, positive number is inside, 0 suppresses ticks, 1 
  #        creates gridlines) default is -0.01
)

# This function adds one or more straight lines through the current plot.
# This is the regression line 
abline( # notes below from: http://www.inside-r.org/r-doc/graphics/abline
  reg = lm.data, 
  #   reg: An object with a coef method. See ‘Details’.
  lwd = 10
  #   lwd: The line width, a positive number, defaulting to 1. The 
  #        interpretation is device-specific, and some devices do not implement
  #        line widths less than one. (See the help on the device for details of
  #        the interpretation.)
  #        Functions such as lines and segments accept a vector of values which
  #        are recycled: in such uses lines corresponding to values NA or NaN
  #        are omitted. The interpretation of is device-specific.
)

# This function draws a box around the current plot in the given color and 
# linetype. The bty parameter determines the type of box drawn. See par for 
# details.
box( # notes below from: http://www.inside-r.org/r-doc/graphics/box
  bty = "l", 
  #   bty: A character string which determined the type of box which is drawn
  #        about plots. If bty is one of "o" (the default), "l", "7", "c", "u",
  #        or "]" the resulting box resembles the corresponding upper case
  #        letter. A value of "n" suppresses the box.
  which = "plot", 
  #   which: Character, one of "plot", "figure", "inner" and "outer".
  lwd = 16
  #   lwd: The line width, a positive number, defaulting to 1. The 
  #        interpretation is device-specific, and some devices do not implement
  #        line widths less than one. (See the help on the device for details of
  #        the interpretation.)
  #        Functions such as lines and segments accept a vector of values which
  #        are recycled: in such uses lines corresponding to values NA or NaN
  #        are omitted. The interpretation of is device-specific.
)

# Text is written in one of the four margins of the current figure region or one
# of the outer margins of the device region.
mtext( # notes below from: http://www.inside-r.org/r-doc/graphics/mtext
  cex = textSize, 
  #   cex: A numerical value giving the amount by which plotting text and 
  #        symbols should be magnified relative to the default. This starts as
  #        1 when a device is opened, and is reset when the layout is changed,
  #        e.g. by setting mfrow.
  #        Note that some graphics functions such as plot.default have an 
  #        argument of this name which multiplies this graphical parameter, and
  #        some functions such as points and text accept a vector of values
  #        which are recycled. 
  line = 22 
  #   line: On which MARgin line, starting at 0 counting outwards.
  side = 1, 
  #   side: On which side of the plot 
  #         (1 = bottom, 2 = left, 3 = top, 4 = right).
  text = "Displacement (pixels)", 
  #   text: A character or expression vector specifying the text to be 
  #         written. Other objects are coerced by as.graphicsAnnot.
)

# Text is written in one of the four margins of the current figure region or one
# of the outer margins of the device region.
mtext( # http://www.inside-r.org/r-doc/graphics/mtext
  cex = textSize, 
  #   cex: A numerical value giving the amount by which plotting text and 
  #        symbols should be magnified relative to the default. This starts as
  #        1 when a device is opened, and is reset when the layout is changed,
  #        e.g. by setting mfrow.
  #        Note that some graphics functions such as plot.default have an 
  #        argument of this name which multiplies this graphical parameter, and
  #        some functions such as points and text accept a vector of values
  #        which are recycled. 
  las = 3
  #   las: Numeric in {0,1,2,3}; the style of axis labels.
  #        0: always parallel to the axis [default],
  #        1: always horizontal,
  #        2: always perpendicular to the axis,
  #        3: always vertical.
  #        Also supported by mtext. Note that string/character rotation via 
  #        argument srt to par does not affect the axis labels.
  line = 18, 
  #   line: On which MARgin line, starting at 0 counting outwards.
  side = 2, 
  #   side: On which side of the plot 
  #         (1 = bottom, 2 = left, 3 = top, 4 = right).
  text = "Force (µN)", 
  #   text: A character or expression vector specifying the text to be 
  #         written. Other objects are coerced by as.graphicsAnnot.
)

# These functions provide control over multiple graphics devices.
# See http://www.inside-r.org/r-doc/grDevices/dev.off
dev.off()
