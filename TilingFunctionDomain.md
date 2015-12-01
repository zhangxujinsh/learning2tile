#Introducing the domain of the tiling functions

## Exploring Tiling Space ##

To generate all possible tiling for a tiling mask use the following method. Considering the run-time performance all code here is implemented in Java instead of R (though we do provide an [R](http://www.r-project.org/) version).


|**Tiling Style**|**Method**|
|:---------------|:---------|
|rectangle       | ```java
 public void generateRectangleTiling(int column, int row, File neighborfile, File outdir) ```|
|diamond         | ```java
 public void generateDiamondTiling(int u, int v, File neighborfile, File outdir)```|
|hexagon         | ```java
 public void generateHexagonTiling(double u, File neighborfile, File outdir)```|
|ellipse         | ```java
 public void generateEllipseTiling(int u, File neighborfile, File outdir)```|

It requires the tiling parameters and a neighbor file indicating the adjacent tiles for each tiles.

## Example ##
For example we want to generate all tiling functions for the diamond (ellipse=1) base screen.
![http://learning2tile.googlecode.com/svn/wiki/images/1_ellipse_id.png](http://learning2tile.googlecode.com/svn/wiki/images/1_ellipse_id.png)

We should provide a neighbor file [link](http://learning2tile.googlecode.com/svn/wiki/tilingfuns/ellipse_1_neighbors.txt) looks like this:
```
0 1,2,3,4
1 0,2,4
2 0,1,3
3 0,2,4
4 0,1,3
```
And then we call the function
```java

generateEllipseTiling(1, new File("ellipse_1_neighbors.txt"), new File("G:\\tilingfun"))
```
Then three files will be generated in the **G:\tilingfun** folder.

  * partition functions

  * tiling functions

  * equal tiling functions


## Summary ##

We summarize some tiling functions in the following table:

|**Tiling Style**|**Parameters**|**#Partition**|**#Tiling Functions**|**#Equal Tiling**|**Neighbor File** |
|:---------------|:-------------|:-------------|:--------------------|:----------------|:-----------------|
|rectangle       |(2,2)         | 15 ([download](http://learning2tile.googlecode.com/svn/wiki/tilingfuns/rectangle_2x2_partition.txt))|12 ([download](http://learning2tile.googlecode.com/svn/wiki/tilingfuns/rectangle_2x2_tiling_fnctions.txt))|4 ([download](http://learning2tile.googlecode.com/svn/wiki/tilingfuns/rectangle_2x2_equal_tiling.txt))|([download](http://learning2tile.googlecode.com/svn/wiki/tilingfuns/rectangle_2x2.neighbors.txt))|
|rectangle       |(3,3)         | 21147([download](http://learning2tile.googlecode.com/svn/wiki/tilingfuns/rectangle_3x3_partition.txt))| 1434 ([download](http://learning2tile.googlecode.com/svn/wiki/tilingfuns/rectangle_3x3_tiling_fnctions.txt))| 12 ([download](http://learning2tile.googlecode.com/svn/wiki/tilingfuns/rectangle_3x3_equal_tiling.txt))|([download](http://learning2tile.googlecode.com/svn/wiki/tilingfuns/rectangle_3x4.neighbors.txt))|
|rectangle       |(3,4)         | 4213597      | 27780([download](http://learning2tile.googlecode.com/svn/wiki/tilingfuns/rectangle_3x4_tiling_fnctions.txt))| 78 ([download](http://learning2tile.googlecode.com/svn/wiki/tilingfuns/rectangle_3x4_equal_tiling.txt))|([download](http://learning2tile.googlecode.com/svn/wiki/tilingfuns/rectangle_3x4.neighbors.txt))|
|rectangle       |(4,3)         | 4213597      | 27780([download](http://learning2tile.googlecode.com/svn/wiki/tilingfuns/rectangle_4x3_tiling_fnctions.txt))| 78 ([download](http://learning2tile.googlecode.com/svn/wiki/tilingfuns/rectangle_4x3_equal_tiling.txt))|([download](http://learning2tile.googlecode.com/svn/wiki/tilingfuns/rectangle_4x3.neighbors.txt))|
|rectangle       |(4,4)         | 10480142147  | 1691690([download](http://learning2tile.googlecode.com/svn/wiki/tilingfuns/rectangle_4x4_tiling_fnctions.txt)) | 225([download](http://learning2tile.googlecode.com/svn/wiki/tilingfuns/rectangle_4x4_equal_tiling.txt))|([download](http://learning2tile.googlecode.com/svn/wiki/tilingfuns/rectangle_4x4.neighbors.txt))|
|diamond         |(1,1)         | 15([download](http://learning2tile.googlecode.com/svn/wiki/tilingfuns/diamond_1x1_partition.txt))| 12([download](http://learning2tile.googlecode.com/svn/wiki/tilingfuns/diamond_1x1_tiling_fnctions.txt))| 4([download](http://learning2tile.googlecode.com/svn/wiki/tilingfuns/diamond_1x1_equal_tiling.txt))|([download](http://learning2tile.googlecode.com/svn/wiki/tilingfuns/diamond_1x1_neighbors.txt))|
|diamond         |(2,2)         | 52([download](http://learning2tile.googlecode.com/svn/wiki/tilingfuns/diamond_2x2_partition.txt))| 16([download](http://learning2tile.googlecode.com/svn/wiki/tilingfuns/diamond_2x2_tiling_fnctions.txt))| 2([download](http://learning2tile.googlecode.com/svn/wiki/tilingfuns/diamond_2x2_equal_tiling.txt))|([download](http://learning2tile.googlecode.com/svn/wiki/tilingfuns/diamond_2x2_neighbors.txt))|
|diamond         |(3,3)         | 4213597      | 17326([download](http://learning2tile.googlecode.com/svn/wiki/tilingfuns/diamond_3x3_tiling_fnctions.txt))| 23([download](http://learning2tile.googlecode.com/svn/wiki/tilingfuns/diamond_3x3_equal_tiling.txt))|([download](http://learning2tile.googlecode.com/svn/wiki/tilingfuns/diamond_3x3_neighbors.txt))|
|diamond         |(4,4)         | 27644437     | 22944([download](http://learning2tile.googlecode.com/svn/wiki/tilingfuns/diamond_4x4_tiling_fnctions.txt))| 2([download](http://learning2tile.googlecode.com/svn/wiki/tilingfuns/diamond_4x4_equal_tiling.txt))|([download](http://learning2tile.googlecode.com/svn/wiki/tilingfuns/diamond_4x4_neighbors.txt))|
|diamond         |(5,4)         | 682076806159 | -                   | 70([download](http://learning2tile.googlecode.com/svn/wiki/tilingfuns/diamond_5x4_equal_tiling.txt))|([download](http://learning2tile.googlecode.com/svn/wiki/tilingfuns/diamond_5x4_neighbors.txt))|
|hexagon         |1             | 52([download](http://learning2tile.googlecode.com/svn/wiki/tilingfuns/hexagon_1_partition.txt))| 20 ([download](http://learning2tile.googlecode.com/svn/wiki/tilingfuns/hexagon_1_tiling_fnctions.txt))| 2([download](http://learning2tile.googlecode.com/svn/wiki/tilingfuns/hexagon_1_equal_tiling.txt))|([download](http://learning2tile.googlecode.com/svn/wiki/tilingfuns/hexagon_1_neighbors.txt))|
|hexagon         |1.5           | 4140([download](http://learning2tile.googlecode.com/svn/wiki/tilingfuns/hexagon_1.5_partition.txt))| 466([download](http://learning2tile.googlecode.com/svn/wiki/tilingfuns/hexagon_1.5_tiling_fnctions.txt))| 7([download](http://learning2tile.googlecode.com/svn/wiki/tilingfuns/hexagon_1.5_equal_tiling.txt))|([download](http://learning2tile.googlecode.com/svn/wiki/tilingfuns/hexagon_1.5_neighbors.txt))|
|hexagon         |2             | 2764437      | 78268([download](http://learning2tile.googlecode.com/svn/wiki/tilingfuns/hexagon_2.0_tiling_fnctions.txt))| 2([download](http://learning2tile.googlecode.com/svn/wiki/tilingfuns/hexagon_2.0_equal_tiling.txt))|([download](http://learning2tile.googlecode.com/svn/wiki/tilingfuns/hexagon_2_neighbors.txt))|
|ellipse         |1             | 52([download](http://learning2tile.googlecode.com/svn/wiki/tilingfuns/ellipse1_partition.txt))| 43([download](http://learning2tile.googlecode.com/svn/wiki/tilingfuns/ellipse1_tiling_fnctions.txt))| 2([download](http://learning2tile.googlecode.com/svn/wiki/tilingfuns/ellipse1_equal_tiling.txt)) |([download](http://learning2tile.googlecode.com/svn/wiki/tilingfuns/ellipse_1_neighbors.txt))|
|ellipse         |4             | 4140([download](http://learning2tile.googlecode.com/svn/wiki/tilingfuns/ellipse4_partition.txt))| 344([download](http://learning2tile.googlecode.com/svn/wiki/tilingfuns/ellipse4_tiling_fnctions.txt))| 5([download](http://learning2tile.googlecode.com/svn/wiki/tilingfuns/ellipse4_equal_tiling.txt)) |([download](http://learning2tile.googlecode.com/svn/wiki/tilingfuns/ellipse_4_neighbors.txt))|
|ellipse         |8             | 4213597      | 5504([download](http://learning2tile.googlecode.com/svn/wiki/tilingfuns/ellipse8_tiling_fnctions.txt))| 10([download](http://learning2tile.googlecode.com/svn/wiki/tilingfuns/ellipse8_equal_tiling.txt)) |([download](http://learning2tile.googlecode.com/svn/wiki/tilingfuns/ellipse_8_neighbors.txt))|



## Details ##

Add your content here.  Format your content with:
  * Text in **bold** or _italic_
  * Headings, paragraphs, and lists
  * Automatic links to other wiki pages