# Mean-Reversion-Strategy-Analysis
Quantitative Analysis - Hedged Mean Reversion Strategy - Nasdaq &amp; Dow Jones Correlation - Historical Market Data Analysis (R).

[Google Drive Folder - documentation](https://docs.google.com/document/d/1PMq9pLrCYTFcRk1K3rm4ybAci_2FbkO_/edit?usp=sharing&ouid=110043109137149831311&rtpof=true&sd=true)

I've noticed that two major US indices - Dow Jones and Nasdaq tend to move in the opposite directions, exceeding some I'd say standard deviation of its correlation.
What's odd, despite the very high correlation these indices have, 'The Gap' in the price movement between them can rise quite significantly, but temporarily.
I observed some patterns, so called 'The Gap' expands and shrinks to some extend consecutively and repeatedly. It happens on many time-frames.

I decided to delve deeper, in order to build a Hedged Mean Reversion Strategy and back-test it on the historical data. The strategy aims to sell one of the mentioned indices which is performing better when deviation from the correlation mean or 'The Gap' is reaching its extremes, and buy worse performing index at the same time.


1. setup.R file installs and loads all the needed packages.
2. data_analysis.R file is fully focused on the EDA (Exploratory Data Analysis).
3. risk_on_off_sentiment.R file is also the part of the EDA process with a specific focus on the market sentiment.
4. InteractiveVisualisations folder includes the Shiny app which demonstrates a few interactive data visualisations.

More to come..
