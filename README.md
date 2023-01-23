# Forecasting Extended Lactations

<!-- badges: start -->

<!-- badges: end -->

This repository contains the code used to analyse the data published in \_\_\_\_\_, titled:

**Fitting mathematical functions to extended lactation curves and forecasting late-lactation milk yields of dairy cows**

David J. Innes, Linaya J. Pot, David J. Seymour, James France, Jan Dijkstra, John Doelman, and John P. Cant

DOI: \_\_\_\_\_\_

------------------------------------------------------------------------

The raw data used in the analysis published in the manuscript remains confidential, but simulated data is provided with characteristics that allows most aspects of this code to be utilised to replicate very similar outputs to those published. However, some of the provided data is not physiologically relevant and the distribution of error is comparatively very uniform, so use with caution and expect real-life data to behave a little differently.

The code in it's current form are provided to ensure that reproducibility of our research is possible, but it is not documented or formatted as a proper package and is perhaps not overly user-friendly. It does, however, require only a file with the following columns to work:

-   `Cow` - cow's number identification

-   `Parity` - number representing what parity the lactation represents

-   `ID` - lactation ID in the format: Cow_Parity (This can be added from the `Cow` and `Parity` columns by concatenating them using `df %>% dplyr::mutate(ID = stringr::str_c(Cow, Parity, sep = "_")`).

-   `DIM` - days in milk; d

-   `my` - milk yield; kg

Some of the initial cleaning steps were developed to fix issues specific to our dataset, partially because we only had access to the 5 columns of data above, and not any of the management data or dates that would otherwise be available on-farm.

The expected output from the provided data set can be viewed at: <https://cnm-university-of-guelph.github.io/forecasting-extended-lactation/> (these are the `.html` files located in the `docs/` folder, which are copies of the `.html` files from each associated `.Rmd` file in the main directory).

Please contact David Innes (innesd\@uoguelph.ca) or the other authors on our manuscript if you are interested in further explanations or assistance. Also, feel free to file an issue on github if you encounter a bug or have feature requests.
