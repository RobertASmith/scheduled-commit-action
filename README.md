# Scheduling R Scripts

This repository is an example of how to automate reporting using GitHub actions. It is based on the blog post [Running R Scripts on a Schedule](https://blog.simonpcouch.com/blog/r-github-actions-commit/). The [schedule-commit workflow file](.github/workflows/schedule-commit.yaml) sets up R, runs several R scripts related to Covid-19 and saves outputs to file, it does this on a daily schedule (after the dashboard updates). Read the linked blog post for more details and explanation!

I have used @VictimofMaths' code plotting covid cases by Sex & Age as an example - in part so that other scripts can be automated in the future.
Colin's code can be found here: https://github.com/VictimOfMaths/COVID-19
