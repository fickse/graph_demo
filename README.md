# Creating a network diagram of your workplace
---
For a presentation at work recently I wanted to create a visualization of a social network to highlight the kinds of insights that can be derived from social network analysis. Social networks are particularly useful for revealing indirect patterns in connected data (e.g. 'friends of friends' relationships).  

![alt text](Capture.png)

I decided to build a network based on the people at my [workplace](http://www.sei-international.org), showing how individuals could be linked to one another based on shared authorship in publications.  When plotted in a network diagram, one can quickly identify clusters of highly connected individuals, as well as guage 'importance' by the connectedness or centrality of any individual.  
* [Check out the the result](http://fickse.github.io/graph_demo/sei_network.html) (it takes a minute to load)!
* [Heres](sei_network.R) the code used to produce the diagram (fortunately SEI's website is easy to scrape!)  
* Much thanks to the excellent work of [Nicole White](https://nicolewhite.github.io/), author of the RNeo4J R package and [this](https://neo4j.com/blog/visualize-graph-with-rneo4j/) excellent tutorial.
