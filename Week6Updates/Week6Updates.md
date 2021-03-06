Week 6 Updates
================
2017-07-07 12:30:00 CDT

Steganography
-------------

This week, we learned how to perform the chi-square goodness-of-fit test and how the test can apply to steganalysis. The chi-square test is a statistical test applied to sets of categorical data to evaluate how likely it is that any observed difference between the sets arose by chance. The chi-square test is specifically for LSB replacement steganalysis because LSB replacement locks least significant bits into pairs. Chi-square only works with embedded images that are embedded lexicographically (meaning from left to right, top to bottom).

![Chi-square test](Chi-square.png)

This image describes our hypothesis and our test statistic. If the If p value is small, reject the null hypothesis. This implies that the image is possibly cover If the p value is large, fail to reject the null hypothesis. This implies that the image is possibly a stego.

Next week, we are using MatLab to calculate the p-values of our images (both cover and stego) and graphing the results for a visual representation of embedding.

Shoeprints
----------

Despite the short week, the shoeprint team made significant progress over the past three days. Continuing on from where we left off the previous week, we began creating histograms and plot to organize and visualize our data. Some of the packages we installed and worked with were: ggpairs, ggally, ggparcoord, ggplotyly, and kmeans. We accomplished this by guidance of our mentors Guillermo and Martín. Please see the prezi (<https://prezi.com/p/rijzzjfgybp_/>) for examples of plots and further code.

These are the packages we worked with.

    purrr
    tidyr
    dplyr

    GGally
    plotly

    EBImage

    solefinder

The code shown below creates a plot that compares the Hu moments against one another. The more linear the scatterplots the closer the correlation of those Hu moments is to one. The colors of the points as well as the graphs represent different pairs of shoes.

    ggpairs(shoeprints, columns = 4:11, ggplot2::aes(colour=id))

The code below generates a plot that compares the Hu moment values for every pair of shoes. The code chunk "mapping = ggplot2::aes(size = 1)" makes the lines thicker and easier to see. The code chunks "ggplot2::aes(size = 1)) + ggplot2::scale\_size\_identity())" put all the values on a relative scale with one representing the the maximum value and zero representing the minimum value. The code chunk "%&gt;% ggplotly()" pipes the plot into the ggplotly function, which makes the plot interactive and allows for more information to be shown.

    (ggparcoord(data = shoeprints, columns = c(3,4:11), groupColumn = 1,
                    title ="Parallel Coord. Plot of Shoeprints Data", mapping = ggplot2::aes(size = 1)) +
      ggplot2::scale_size_identity()) %>% ggplotly()

Guillermo and Martín compiled an excel document comprised of Pokemon statistics. This allowed us to practice clustering data. While this Pokemon data does not directly correlate with shoeprint analysis, the multiple variables present proved to be a great learning tool. The presence of both numeric and non-numeric variables paired with their range gave us new obstacles to overcome as we begin clustering.

Cataloguing Casings
-------------------

These past few weeks, I went through the large crates full of cartridge casings and cataloged them to see what we had and what we were missing. Then I created new codes for all of the casings we have, which will eventually be used for making subsamples for another round of analysis by examiners.
