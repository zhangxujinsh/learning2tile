# Problem #
Learning "an optimal" spatial BoW representation from data.

# Abstract #
Spatial Pyramid Matching (SPM) assumes that the spatial Bag-of-Words (BoW) representation is independent of data. However, evidence has shown that the assumption usually leads to a suboptimal representation. In this paper, we propose a novel method called Jensen-Shannon (JS) Tiling to learn the BoW representation from data directly at the BoW level. The proposed JS Tiling is especially appropriate for large-scale datasets as it is orders of magnitude faster than existing methods, but with comparable or even better classification precision. Specifically, JS Tiling systematically generates all possible spatial BoW representations called tilings, which are then evaluated using the computationally inexpensive metric based on the JS divergence. Experimental results on four benchmarks including two TRECVID12 datasets validate that JS Tiling outperforms the SPM and the state-of-the-art methods. The runtime comparison demonstrates that selecting BoW representations by JS Tiling is more than 1,000 times faster than running classifiers.


# JS Tiling step by step #
  * [Getting started](learning2tile_overview.md)
  * [What's a tiling?](TilingMembership.md)
  * [How many tilings?](TilingFunctionDomain.md)
  * [What's a mask?](TilingFunction.md)


# Slides #
[slides presented](http://www.cs.cmu.edu/~lujiang/resources/JSTiling_Presentation.pdf)

# References #
Lu Jiang, Wei Tong, Deyu Meng, Alexander Hauptmann. Towards Efficient Learning of Optimal Spatial Bag-of-Words Representations.
In ACM International Conference on Multimedia Retrieval (ICMR). Glasgow, United Kingdom. 2014.


---
