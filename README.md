# Russian Housing Analysis
## Chad Evans

## Description
Housing costs demand a significant investment from both consumers and developers. And when it comes to planning a budget—whether personal or corporate—the last thing anyone needs is uncertainty about one of their biggets expenses. Sberbank, Russia’s oldest and largest bank, helps their customers by making predictions about realty prices so renters, developers, and lenders are more confident when they sign a lease or purchase a building.

Although the housing market is relatively stable in Russia, the country’s volatile economy makes forecasting prices as a function of apartment characteristics a unique challenge. Complex interactions between housing features such as number of bedrooms and location are enough to make pricing predictions complicated. Adding an unstable economy to the mix means Sberbank and their customers need more than simple regression models in their arsenal.

In this competition, Sberbank is challenging Kagglers to develop algorithms which use a broad spectrum of features to predict realty prices. Competitors will rely on a rich dataset that includes housing data and macroeconomic patterns. An accurate forecasting model will allow Sberbank to provide more certainty to their customers in an uncertain economy.

## Evaluation
Submissions are evaluated on the RMSLE between their predicted prices and the actual data. The target variable, called price_doc in the training set, is the sale price of each property.

Submission File

For each id in the test set, you must predict the price that the property sold for. The file should contain a header and have the following format:

id,price_doc
30474,7118500.44
30475,7118500.44
30476,7118500.44
etc.

## Timeline
June 22, 2017 - Entry deadline. You must accept the competition rules before this date in order to compete.
June 22, 2017 - Team Merger deadline. This is the last day participants may join or merge teams.
June 29, 2017 - Final submission deadline.
All deadlines are at 11:59 PM UTC on the corresponding day unless otherwise noted. The competition organizers reserve the right to update the contest timeline if they deem it necessary.
