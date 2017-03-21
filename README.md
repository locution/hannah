***Overview of Social Network Methodology***

*Davie - March 18, 2017*

The overall goal of this portion of the analysis is to search for
significant differences in individual interaction and spatial network
metric values relative to individual horse characteristic, including
sex, reproductive status, group membership, and personality trait score.
Spatial (or distance) networks show the median inverse distance between
all present pairs of horses over the observation period. The interaction
networks present the number of occurrences for each interaction between
horses, such as kicks, mutual grooming, and play interactions.

<span id="anchor"></span>The metric values calculated include:

1.  a.  Weighted betweenness – Betweenness is a measure of how often a
        given node is part of the shortest paths between two
        other nodes. With weighted betweenness, an alpha value is set so
        that the shortest path between nodes in the network is one with
        the strongest ties (horses that are closer together or, for
        interaction networks, horses that most frequently engage
        in interactions) and the fewest intermediate nodes.
    b.  Strength – The strength of each node is the sum of the weights
        of all edges connecting that node to other nodes. For spatial
        networks, this would be the summed inverse distance to all
        other horses. For interactions, it would be the sum of all
        interactions/hour directed towards other horses.
    c.  In-strength – This was only calculated for interaction matrices,
        the instrength of the node is the total sum of the rate of
        interactions received by that horse.
    d.  Degree (alpha = 0.5) – when weighted degree is calculated with
        an alpha weighting of 0.5, the number of contacts over which the
        total node strength is distributed increases the value of
        the measure. Thus, a horse that interacts with many other horses
        will have a higher degree score than a horse that interacts with
        only one or two other horses at a higher rate.
    e.  In-degree (alpha=0.5) – This was only calculated for interaction
        matrices, and is the sum of the rate of interactions received by
        that horse, adjusted by alpha.
    f.  Clustering coefficient – In binary networks, the clustering
        coefficient for each node is the fraction of the number of ties
        present among a node’s immediate neighbours over the total
        number of ties possible. Because the network is weighted, the
        clustering coefficient in this case is calculated by summing the
        value of the closed triplets that were centred on the node and
        dividing by the total value of all triplets involving that node
        (Barrat et al. 2004, Opsahl and Panzarasa, 2009).

Below is a network of the median inverse distances between all horses
present during the Summer 2015 field season. The points, or nodes,
represent individual horses, the color represents each horses
reproductive class, and the size of the node represents the network
strength of each horse. The lines, or edges, between each horse
represent the median inverse distance between them, with thicker more
orange lines indicating horses that are overall spatially close to each
other, and light thin lines indicating horses that are usually farther
away from each other. The network program arranges the different nodes
so each node is closest to the greatest number of other nodes with which
it has strong ties. Thus, **kh** and **ukh** in the lower right hand
corner, who are a mare and foal pair, are positioned close together and
also have a very thick, dark edge tie.

All network metrics were calculated using the weighted network analysis
methodology developed by Dr. Opsahl in the R package tnet. I am testing
for a significant difference in the median betweenness, strength,
instrength, degree, indegree and clustering coefficient of horses
depending on sex, reproductive status, group membership, and sociability
and assertiveness personality scores in both spatial and interaction
networks. Test are run for the whole herd and individually for each
family group. I’m testing for a significant difference in metrics scores
using a Kruskall-Wallis test, and have been using Dunn’s test for follow
up multiple pairwise comparisons. For each test, the observed test
statistic was compared to 5000 test statistics calculated from
randomized versions of the network of interest.

I then calculated the proportion of random network test statistics
greater or less than my observed test statistic. If the proportion of
random test statistics greater than the observed value was less than
0.05 or greater than 0.95 I considered the difference in medians to be
greater than would be expected by chance.

Permutation methods – I used three different methods of permutation to
check for network trends that were significantly different from random
in my networks.

1.  a.  weight – the network weight reshuffling permutation randomly
        moves edge weights around the network. This creates a random
        network, but maintains the distribution of edge values and does
        not add any ties between horses where there were no ties in the
        original network. Also, the number of ties originating from any
        one node (horse) does not change.
    b.  link – the link reshuffling permutation repeatedly randomly
        selects two edges and swaps their destinations (ex: A was tied
        to B and C was tied to D, but now A is tied to D, and C is tied
        to B). This permutation method maintains the outstrength, but
        not the instrength of each node in the network.
    c.  nodeperm – The node permutation method randomly permutes whole
        rows and columns of the input matrix. Thus, this method does not
        preserve the topology of the original matrix in the way that the
        weight and (to a lesser extent) the link permutations do, but in
        the random networks does allow the formation of new edges
        connecting horses that were not part of the original network.
        Thus, even though the stallion Chronos was not involved in any
        mutual grooming during Spring, he is included in some of the
        random networks when using node permutations. For some of the
        more sparse distance and interaction networks, I think this
        might be a good thing.

The code is run for each network/characteristic. Each test requires the
network input file (usually in edgelist format) and an individual horse
attribute file, which includes all individual characteristics for each
horse in the network. The code outputs a Kruskal-Wallis test result, and
a separate file with the results of all pairwise tests.

The following files are included in the attached folder:

AttributeSummer2015Metrics.csv – This file has all information on the
individual horses included in the summer spatial network, including
their observed network metrics.

ComparingClassMetrics\_SummerAllRepStat – This is the R code file for
testing for a significant difference in summer spatial network metrics
among horses of differing reproductive status. A txt file version of the
same code is also present.

ComparingClassMetrics\_SummerAllRepStat – This is the R code file for
testing for a significant difference in summer spatial network metrics
among horses of differing rated sociability personality scores. A txt
file version of the same code is also present.

Summer2015\_All.csv – This in the network input file. It includes the
median inverse distance between all possible pairs of horses present in
the summer of 2015. Each row includes horse A’s id, horse B’s id, and
then the inverse median distance between them.
