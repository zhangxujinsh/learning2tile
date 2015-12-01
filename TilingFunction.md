## Definition ##
a mask (or tiling mask) is a tiling from which more complicated tilings are derived by combining the tiles in it. Any tiling can be a mask.  However,we ﬁnd the types of tilings illustrated below are good candidates of masks. Note that a mask is also associated with a parameter controlling its complexity. In this following we introduce four types of masks namely, rectangle, diamond, hexagon and ellipse masks.

## Rectangle ##
To generate the a rectangle mask call **TilingRectangle**,

**TilingRectangle <- function(u, v, txyc\_matrix, width, height)**

where u is the number of rectangle in each row and v is the number in each column.

### Examples ###

```R

source("http://learning2tile.googlecode.com/svn/trunk/tiling_lib.r")
txyc_matrix = as.matrix(expand.grid(1:500,1:333))
t = TilingRectangle(3,3, txyc_matrix, 500, 333)
PlotTiling(txyc_matrix, t)
```
![http://learning2tile.googlecode.com/svn/wiki/images/rectangle3_3.png](http://learning2tile.googlecode.com/svn/wiki/images/rectangle3_3.png)

To save the above plot in a file called "3x3\_rectangle\_tiling.png" use
```R

SaveTilingPlot(txyc_matrix, t, "3x3_rectangle_tiling.png")
```

```R

t = TilingRectangle(3,2, txyc_matrix, 500, 333)
PlotTiling(txyc_matrix, t)
SaveTilingPlot(txyc_matrix, t, "3x3_rectangle_tiling.png")
```
![http://learning2tile.googlecode.com/svn/wiki/images/rectangle3_2.png](http://learning2tile.googlecode.com/svn/wiki/images/rectangle3_2.png)


## Diamond ##
To generate the a tiling by diamonds call **TilingRectangle**,

**TilingDiamond <- function(u, v, txyc\_matrix, width, height)**

where u and v controls the number of left and right diagonal lines.

### Examples ###
```R

t = TilingDiamond (1,1, txyc_matrix, 500, 333)
PlotTiling(txyc_matrix, t)
```
![http://learning2tile.googlecode.com/svn/wiki/images/diamond1_1.png](http://learning2tile.googlecode.com/svn/wiki/images/diamond1_1.png)

```R

t = TilingDiamond (1,2, txyc_matrix, 500, 333)
PlotTiling(txyc_matrix, t)
```
![http://learning2tile.googlecode.com/svn/wiki/images/diamond1_2.png](http://learning2tile.googlecode.com/svn/wiki/images/diamond1_2.png)


```R

t = TilingDiamond (3,1, txyc_matrix, 500, 333)
PlotTiling(txyc_matrix, t)
```
![http://learning2tile.googlecode.com/svn/wiki/images/diamond3_1.png](http://learning2tile.googlecode.com/svn/wiki/images/diamond3_1.png)

```R

t = TilingDiamond (5,5, txyc_matrix, 500, 333)
PlotTiling(txyc_matrix, t)
```
![http://learning2tile.googlecode.com/svn/wiki/images/diamond5_5.png](http://learning2tile.googlecode.com/svn/wiki/images/diamond5_5.png)


## Hexagon ##
To generate the a tiling by hexagons call **TilingHexagon**,

**TilingHexagon <- function(u, txyc\_matrix, width, height)**

u is the parameter controlling the edge length of each hexagon and it follows:

\text{hex\_edge\_len} = \frac{\text{height}}{u \times \sqrt{3}}

### Examples ###
```R

t = TilingHexagon(1, txyc_matrix, 500, 333)
PlotTiling(txyc_matrix, t)
```
![http://learning2tile.googlecode.com/svn/wiki/images/hexagon1.png](http://learning2tile.googlecode.com/svn/wiki/images/hexagon1.png)

```R

t = TilingHexagon(1.5, txyc_matrix, 500, 333)
PlotTiling(txyc_matrix, t)
```
![http://learning2tile.googlecode.com/svn/wiki/images/hexagon1.5.png](http://learning2tile.googlecode.com/svn/wiki/images/hexagon1.5.png)

```R

t = TilingHexagon(2, txyc_matrix, 500, 333)
PlotTiling(txyc_matrix, t)
```
![http://learning2tile.googlecode.com/svn/wiki/images/hexagon2.png](http://learning2tile.googlecode.com/svn/wiki/images/hexagon2.png)

```R

t = TilingHexagon(5, txyc_matrix, 500, 333)
PlotTiling(txyc_matrix, t)
```
![http://learning2tile.googlecode.com/svn/wiki/images/hexagon5.png](http://learning2tile.googlecode.com/svn/wiki/images/hexagon5.png)

## Ellipse ##
To generate the a tiling of camera style call **TilingEllipse**

**TilingEllipse <- function(u=5, txyc\_matrix, width, height)**

, where u controls the number of ellipse in the center.

### Examples ###

```R

t = TilingEllipse(2, txyc_matrix, 500, 333)
PlotTiling(txyc_matrix, t)
```
![http://learning2tile.googlecode.com/svn/wiki/images/2_ellipse.png](http://learning2tile.googlecode.com/svn/wiki/images/2_ellipse.png)

```R

t = TilingCamera(5, txyc_matrix, 500, 333)
PlotTiling(txyc_matrix, t)
```
![http://learning2tile.googlecode.com/svn/wiki/images/5_ellipse.png](http://learning2tile.googlecode.com/svn/wiki/images/5_ellipse.png)


```R

t = TilingCamera(7, txyc_matrix, 500, 333)
PlotTiling(txyc_matrix, t)
```
![http://learning2tile.googlecode.com/svn/wiki/images/7_ellipse.png](http://learning2tile.googlecode.com/svn/wiki/images/7_ellipse.png)

## Comments ##
Add your content here.  Format your content with:
  * Text in **bold** or _italic_
  * Headings, paragraphs, and lists
  * Automatic links to other wiki pages