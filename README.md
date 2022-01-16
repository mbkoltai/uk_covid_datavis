The script [datavis.R](https://github.com/mbkoltai/uk_covid_datavis/blob/master/datavis.R) pulls data from the UK Coronavirus dashboard for England and creates the plots below. The script can be run from the command line by `Rscript datavis.R`.
The script [london_datavis.R](https://github.com/mbkoltai/uk_covid_datavis/blob/master/london_datavis.R) does the same for London.
ONS population denominators are used instead of NIMS for vaccine coverage, the ONS data files are [here](https://github.com/mbkoltai/uk_covid_datavis/blob/master/ONS_2019_midpoint_population_estim_modified.csv) and [here](https://github.com/mbkoltai/uk_covid_datavis/blob/master/ons_all_age_groups_uk_england_2019.csv).

## England COVID-19: Vaccine rollout, cases, hospitalisations, deaths


| Data type           | Metric                                     | Facet/group by                   | Lin/log | Y-scale    | Link | comment         |
  |---------------------|--------------------------------------------|--------------------------------------|---------|------------|---------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------|-----------------|
  | Vaccines            | % 12y+ population per day and cumulatively | vaccine dose (1,2,3)                 | lin     | fixed      | [link](https://github.com/mbkoltai/uk_covid_datavis/raw/master/vaccine_data/vaccine_allage_phaseportrait_3rows.png)        |                 |
  | Vaccines            | **% vaccinated cumulatively**                  | 5-yr age bands                       | lin     | fixed      | [link](https://github.com/mbkoltai/uk_covid_datavis/raw/master/vaccine_data/vaccine_by_age_cumul.png) |                 |
  | Vaccines            | **% age group per day and cumulatively**       | 5-yr age bands                       | lin-log | fixed      | [link](https://github.com/mbkoltai/uk_covid_datavis/raw/master/vaccine_data/vaccine_by_age_phaseportrait_both_doses_line_log.png) |                 |
  | Vaccines            | abs num doses per day per age group        | 5-yr age bands                       | lin     | fixed      | [link](https://github.com/mbkoltai/uk_covid_datavis/raw/master/vaccine_data/vaccine_by_age_rate_absnum_lin.png)          |                 |
  | Vaccines            | abs num doses per day per age group        | 5-yr age bands                       | log     | fixed      | [link](https://github.com/mbkoltai/uk_covid_datavis/raw/master/vaccine_data/vaccine_by_age_rate_absnum_log.png) |                 |
  | Vaccines            | % age group per day                        | 5-yr age bands                       | lin     | fixed      | [link](https://github.com/mbkoltai/uk_covid_datavis/raw/master/vaccine_data/vaccine_by_age_rate_lin.png)    |                 |
  | Vaccines            | **% age group per day**                        | 5-yr age bands                       | log     | fixed      | [link](https://raw.githubusercontent.com/mbkoltai/uk_covid_datavis/master/vaccine_data/vaccine_by_age_rate_log.png)   |                 |
  | Cases               | % change of weekly sum                     | 25-year age bands, grouped by 5 year | lin     | fixed      | [link](https://raw.githubusercontent.com/mbkoltai/uk_covid_datavis/master/england_cases_age_4groups_rollingsum_change.png)  |                 |
  | Cases               | absolute number                | 10-year age bands                    | lin/log | fixed/free |
  [linear_nofacet](https://raw.githubusercontent.com/mbkoltai/uk_covid_datavis/master/cases_hosp_deaths_from_2021_10_01/england_cases_by_age_lineplot_linear_nofacet_absnum.png)
  [linear_free_facet](https://raw.githubusercontent.com/mbkoltai/uk_covid_datavis/master/cases_hosp_deaths_from_2021_10_01/england_cases_by_age_lineplot_linear_absnum.png)
  [log_nofacet](https://raw.githubusercontent.com/mbkoltai/uk_covid_datavis/master/cases_hosp_deaths_from_2021_10_01/england_cases_by_age_lineplot_log_nofacet_absnum.png)
  [log_free_facet](https://raw.githubusercontent.com/mbkoltai/uk_covid_datavis/master/cases_hosp_deaths_from_2021_10_01/england_cases_by_age_lineplot_log_absnum.png)
  [log_fixed_facet](https://raw.githubusercontent.com/mbkoltai/uk_covid_datavis/master/cases_hosp_deaths_from_2021_10_01/england_cases_by_age_lineplot_log_yfixed_absnum.png)                                                             | from 2021/07/01 |
  | Cases               | rate per million population                | 10-year age bands                    | lin/log | fixed/free |
  [linear_nofacet](https://raw.githubusercontent.com/mbkoltai/uk_covid_datavis/master/cases_hosp_deaths_from_2021_10_01/england_cases_by_age_lineplot_linear_nofacet_rate.png)
  [linear_free_facet](https://raw.githubusercontent.com/mbkoltai/uk_covid_datavis/master/cases_hosp_deaths_from_2021_10_01/england_cases_by_age_lineplot_linear_rate.png)
  [log_nofacet](https://raw.githubusercontent.com/mbkoltai/uk_covid_datavis/master/cases_hosp_deaths_from_2021_10_01/england_cases_by_age_lineplot_log_nofacet_rate.png)
  [log_free_facet](https://raw.githubusercontent.com/mbkoltai/uk_covid_datavis/master/cases_hosp_deaths_from_2021_10_01/england_cases_by_age_lineplot_log_rate.png)
  [log_fixed_facet](https://raw.githubusercontent.com/mbkoltai/uk_covid_datavis/master/cases_hosp_deaths_from_2021_10_01/england_cases_by_age_lineplot_log_yfixed_rate.png)                                                             | from 2021/07/01 |
  | Cases               | normalised to Jan/2021 peak                | 10-year age bands                    | lin/log | fixed/free |
  [linear_nofacet](https://raw.githubusercontent.com/mbkoltai/uk_covid_datavis/master/cases_hosp_deaths_from_2021_10_01/england_cases_by_age_lineplot_linear_nofacet_absnum_peak_norm.png)
  [linear_free_facet](https://raw.githubusercontent.com/mbkoltai/uk_covid_datavis/master/cases_hosp_deaths_from_2021_10_01/england_cases_by_age_lineplot_linear_absnum_peak_norm.png)
  [log_nofacet](https://raw.githubusercontent.com/mbkoltai/uk_covid_datavis/master/cases_hosp_deaths_from_2021_10_01/england_cases_by_age_lineplot_log_nofacet_absnum_peak_norm.png)
  [log_free_facet](https://raw.githubusercontent.com/mbkoltai/uk_covid_datavis/master/cases_hosp_deaths_from_2021_10_01/england_cases_by_age_lineplot_log_absnum_peak_norm.png)
  [log_fixed_facet](https://raw.githubusercontent.com/mbkoltai/uk_covid_datavis/master/cases_hosp_deaths_from_2021_10_01/england_cases_by_age_lineplot_log_yfixed_absnum_peak_norm.png) | from 2021/07/01, normalised to Jan/2021 peak |
  | Hospital admissions               | absolute number                | 10-year age bands                    | lin/log | fixed/free |
  [linear_nofacet](https://raw.githubusercontent.com/mbkoltai/uk_covid_datavis/master/cases_hosp_deaths_from_2021_10_01/england_admissions_by_age_lineplot_linear_nofacet_absnum.png)
  [linear_free_facet](https://raw.githubusercontent.com/mbkoltai/uk_covid_datavis/master/cases_hosp_deaths_from_2021_10_01/england_admissions_by_age_lineplot_linear_absnum.png)
  [log_nofacet](https://raw.githubusercontent.com/mbkoltai/uk_covid_datavis/master/cases_hosp_deaths_from_2021_10_01/england_admissions_by_age_lineplot_log_nofacet_absnum.png)
  [log_free_facet](https://raw.githubusercontent.com/mbkoltai/uk_covid_datavis/master/cases_hosp_deaths_from_2021_10_01/england_admissions_by_age_lineplot_log_absnum.png)
  [log_fixed_facet](https://raw.githubusercontent.com/mbkoltai/uk_covid_datavis/master/cases_hosp_deaths_from_2021_10_01/england_admissions_by_age_lineplot_log_yfixed_absnum.png)                                                             | from 2021/07/01 |
  | Hospital admissions               | rate per million population                | 10-year age bands                    | lin/log | fixed/free |
  [linear_nofacet](https://raw.githubusercontent.com/mbkoltai/uk_covid_datavis/master/cases_hosp_deaths_from_2021_10_01/england_admissions_by_age_lineplot_linear_nofacet_rate.png)
  [linear_free_facet](https://raw.githubusercontent.com/mbkoltai/uk_covid_datavis/master/cases_hosp_deaths_from_2021_10_01/england_admissions_by_age_lineplot_linear_rate.png)
  [log_nofacet](https://raw.githubusercontent.com/mbkoltai/uk_covid_datavis/master/cases_hosp_deaths_from_2021_10_01/england_admissions_by_age_lineplot_log_nofacet_rate.png)
  [log_free_facet](https://raw.githubusercontent.com/mbkoltai/uk_covid_datavis/master/cases_hosp_deaths_from_2021_10_01/england_admissions_by_age_lineplot_log_rate.png)
  [log_fixed_facet](https://raw.githubusercontent.com/mbkoltai/uk_covid_datavis/master/cases_hosp_deaths_from_2021_10_01/england_admissions_by_age_lineplot_log_yfixed_rate.png)                                                             | from 2021/07/01 |
  | Hospital admissions               | normalised to Jan/2021 peak                | 10-year age bands                    | lin/log | fixed/free |
  [linear_nofacet](https://raw.githubusercontent.com/mbkoltai/uk_covid_datavis/master/cases_hosp_deaths_from_2021_10_01/england_admissions_by_age_lineplot_linear_nofacet_absnum_peak_norm.png)
  [linear_free_facet](https://raw.githubusercontent.com/mbkoltai/uk_covid_datavis/master/cases_hosp_deaths_from_2021_10_01/england_admissions_by_age_lineplot_linear_absnum_peak_norm.png)
  [log_nofacet](https://raw.githubusercontent.com/mbkoltai/uk_covid_datavis/master/cases_hosp_deaths_from_2021_10_01/england_admissions_by_age_lineplot_log_nofacet_absnum_peak_norm.png)
  [log_free_facet](https://raw.githubusercontent.com/mbkoltai/uk_covid_datavis/master/cases_hosp_deaths_from_2021_10_01/england_admissions_by_age_lineplot_log_absnum_peak_norm.png)
  [log_fixed_facet](https://raw.githubusercontent.com/mbkoltai/uk_covid_datavis/master/cases_hosp_deaths_from_2021_10_01/england_admissions_by_age_lineplot_log_yfixed_absnum_peak_norm.png) | from 2021/07/01, normalised to Jan/2021 peak |
  | Deaths               | absolute number                | 10-year age bands                    | lin/log | fixed/free |
  [linear_nofacet](https://raw.githubusercontent.com/mbkoltai/uk_covid_datavis/master/cases_hosp_deaths_from_2021_10_01/england_deaths_by_age_lineplot_linear_nofacet_absnum.png)
  [linear_free_facet](https://raw.githubusercontent.com/mbkoltai/uk_covid_datavis/master/cases_hosp_deaths_from_2021_10_01/england_deaths_by_age_lineplot_linear_absnum.png)
  [log_nofacet](https://raw.githubusercontent.com/mbkoltai/uk_covid_datavis/master/cases_hosp_deaths_from_2021_10_01/england_deaths_by_age_lineplot_log_nofacet_absnum.png)
  [log_free_facet](https://raw.githubusercontent.com/mbkoltai/uk_covid_datavis/master/cases_hosp_deaths_from_2021_10_01/england_deaths_by_age_lineplot_log_absnum.png)
  [log_fixed_facet](https://raw.githubusercontent.com/mbkoltai/uk_covid_datavis/master/cases_hosp_deaths_from_2021_10_01/england_deaths_by_age_lineplot_log_yfixed_absnum.png)                                                             | from 2021/07/01 |
  | Deaths               | rate per million population                | 10-year age bands                    | lin/log | fixed/free |
  [linear_nofacet](https://raw.githubusercontent.com/mbkoltai/uk_covid_datavis/master/cases_hosp_deaths_from_2021_10_01/england_deaths_by_age_lineplot_linear_nofacet_rate.png)
  [linear_free_facet](https://raw.githubusercontent.com/mbkoltai/uk_covid_datavis/master/cases_hosp_deaths_from_2021_10_01/england_deaths_by_age_lineplot_linear_rate.png)
  [log_nofacet](https://raw.githubusercontent.com/mbkoltai/uk_covid_datavis/master/cases_hosp_deaths_from_2021_10_01/england_deaths_by_age_lineplot_log_nofacet_rate.png)
  [log_free_facet](https://raw.githubusercontent.com/mbkoltai/uk_covid_datavis/master/cases_hosp_deaths_from_2021_10_01/england_deaths_by_age_lineplot_log_rate.png)
  [log_fixed_facet](https://raw.githubusercontent.com/mbkoltai/uk_covid_datavis/master/cases_hosp_deaths_from_2021_10_01/england_deaths_by_age_lineplot_log_yfixed_rate.png)                                                             | from 2021/07/01 |
  | Deaths               | normalised to Jan/2021 peak                | 10-year age bands                    | lin/log | fixed/free |
  [linear_nofacet](https://raw.githubusercontent.com/mbkoltai/uk_covid_datavis/master/cases_hosp_deaths_from_2021_10_01/england_deaths_by_age_lineplot_linear_nofacet_absnum_peak_norm.png)
  [linear_free_facet](https://raw.githubusercontent.com/mbkoltai/uk_covid_datavis/master/cases_hosp_deaths_from_2021_10_01/england_deaths_by_age_lineplot_linear_absnum_peak_norm.png)
  [log_nofacet](https://raw.githubusercontent.com/mbkoltai/uk_covid_datavis/master/cases_hosp_deaths_from_2021_10_01/england_deaths_by_age_lineplot_log_nofacet_absnum_peak_norm.png)
  [log_free_facet](https://raw.githubusercontent.com/mbkoltai/uk_covid_datavis/master/cases_hosp_deaths_from_2021_10_01/england_deaths_by_age_lineplot_log_absnum_peak_norm.png)
  [log_fixed_facet](https://raw.githubusercontent.com/mbkoltai/uk_covid_datavis/master/cases_hosp_deaths_from_2021_10_01/england_deaths_by_age_lineplot_log_yfixed_absnum_peak_norm.png)  | from 2021/07/01, normalised to Jan/2021 peak |

****

## London COVID-19: Vaccine rollout, cases, deaths

| Data type           | Metric                                     | Facet/group by                   | Lin/log | Y-scale    | Link | comment         |
  |---------------------|--------------------------------------------|--------------------------------------|---------|------------|---------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------|-----------------|
  | Cases               | % change of weekly sum                     | 25-year age bands, grouped by 5 year | lin     | fixed      | [link](https://raw.githubusercontent.com/mbkoltai/uk_covid_datavis/master/london/london_cases_age_4groups_rollingsum_change.png)   |                 |
  | Vaccines            | % 12y+ population per day and cumulatively | vaccine dose (1,2,3)                 | lin     | fixed      | [link](https://github.com/mbkoltai/uk_covid_datavis/raw/master/vaccine_data/vaccine_allage_phaseportrait_3rows.png)                                          |                 |
  | Vaccines            | **% vaccinated cumulatively**                  | 5-yr age bands                       | lin     | fixed      | [link](https://github.com/mbkoltai/uk_covid_datavis/raw/master/vaccine_data/vaccine_by_age_cumul.png)  |                 |
  | Vaccines            | **% age group per day and cumulatively**       | 5-yr age bands                       | lin-log | fixed      | [link](https://github.com/mbkoltai/uk_covid_datavis/raw/master/vaccine_data/vaccine_by_age_phaseportrait_both_doses_line_log.png |                 |
  | Vaccines            | abs num doses per day per age group        | 5-yr age bands                       | lin     | fixed      | [link](https://github.com/mbkoltai/uk_covid_datavis/raw/master/vaccine_data/vaccine_by_age_rate_absnum_lin.png) |                 |
  | Vaccines            | abs num doses per day per age group        | 5-yr age bands                       | log     | fixed      | [link](https://github.com/mbkoltai/uk_covid_datavis/raw/master/vaccine_data/vaccine_by_age_rate_absnum_log.png) |                 |
  | Vaccines            | % age group per day                        | 5-yr age bands                       | lin     | fixed      | [link](https://github.com/mbkoltai/uk_covid_datavis/raw/master/vaccine_data/vaccine_by_age_rate_lin.png) |                 |
  | Vaccines            | **% age group per day**                        | 5-yr age bands                       | log     | fixed      | [link](https://raw.githubusercontent.com/mbkoltai/uk_covid_datavis/master/london/vaccine_data/vaccine_by_age_rate_log.png) |                 |

  | Cases               | % change of weekly sum                     | 25-year age bands, grouped by 5 year | lin     | fixed      | [link](https://raw.githubusercontent.com/mbkoltai/uk_covid_datavis/master/london/london_cases_age_4groups_rollingsum_change.png)  |                 |
  | Cases               | absolute number                | 10-year age bands                    | lin/log | fixed/free |
  [linear_nofacet](https://raw.githubusercontent.com/mbkoltai/uk_covid_datavis/master/london/cases_hosp_deaths_from_2021_10_01/london_cases_by_age_lineplot_linear_nofacet_absnum.png)
  [linear_free_facet](https://raw.githubusercontent.com/mbkoltai/uk_covid_datavis/master/london/cases_hosp_deaths_from_2021_10_01/london_cases_by_age_lineplot_linear_absnum.png)
  [log_nofacet](https://raw.githubusercontent.com/mbkoltai/uk_covid_datavis/master/london/cases_hosp_deaths_from_2021_10_01/london_cases_by_age_lineplot_log_nofacet_absnum.png)
  [log_free_facet](https://raw.githubusercontent.com/mbkoltai/uk_covid_datavis/master/london/cases_hosp_deaths_from_2021_10_01/london_cases_by_age_lineplot_log_absnum.png)
  [log_fixed_facet](https://raw.githubusercontent.com/mbkoltai/uk_covid_datavis/master/london/cases_hosp_deaths_from_2021_10_01/london_cases_by_age_lineplot_log_yfixed_absnum.png)                                                             | from 2021/07/01 |
  | Cases               | rate per million population                | 10-year age bands                    | lin/log | fixed/free |
  [linear_nofacet](https://raw.githubusercontent.com/mbkoltai/uk_covid_datavis/master/london/cases_hosp_deaths_from_2021_10_01/london_cases_by_age_lineplot_linear_nofacet_rate.png)
  [linear_free_facet](https://raw.githubusercontent.com/mbkoltai/uk_covid_datavis/master/london/cases_hosp_deaths_from_2021_10_01/london_cases_by_age_lineplot_linear_rate.png)
  [log_nofacet](https://raw.githubusercontent.com/mbkoltai/uk_covid_datavis/master/london/cases_hosp_deaths_from_2021_10_01/london_cases_by_age_lineplot_log_nofacet_rate.png)
  [log_free_facet](https://raw.githubusercontent.com/mbkoltai/uk_covid_datavis/master/london/cases_hosp_deaths_from_2021_10_01/london_cases_by_age_lineplot_log_rate.png)
  [log_fixed_facet](https://raw.githubusercontent.com/mbkoltai/uk_covid_datavis/master/london/cases_hosp_deaths_from_2021_10_01/london_cases_by_age_lineplot_log_yfixed_rate.png)                                                             | from 2021/07/01 |
  | Cases               | normalised to Jan/2021 peak                | 10-year age bands                    | lin/log | fixed/free |
  [linear_nofacet](https://raw.githubusercontent.com/mbkoltai/uk_covid_datavis/master/london/cases_hosp_deaths_from_2021_10_01/london_cases_by_age_lineplot_linear_nofacet_absnum_peak_norm.png)
  [linear_free_facet](https://raw.githubusercontent.com/mbkoltai/uk_covid_datavis/master/london/cases_hosp_deaths_from_2021_10_01/london_cases_by_age_lineplot_linear_absnum_peak_norm.png)
  [log_nofacet](https://raw.githubusercontent.com/mbkoltai/uk_covid_datavis/master/london/cases_hosp_deaths_from_2021_10_01/london_cases_by_age_lineplot_log_nofacet_absnum_peak_norm.png)
  [log_free_facet](https://raw.githubusercontent.com/mbkoltai/uk_covid_datavis/master/london/cases_hosp_deaths_from_2021_10_01/london_cases_by_age_lineplot_log_absnum_peak_norm.png)
  [log_fixed_facet](https://raw.githubusercontent.com/mbkoltai/uk_covid_datavis/master/london/cases_hosp_deaths_from_2021_10_01/london_cases_by_age_lineplot_log_yfixed_absnum_peak_norm.png)                                                             | from 2021/07/01, normalised to Jan/2021 peak |

  | Hospital admissions               | absolute number                | 10-year age bands                    | lin/log | fixed/free |
  [linear_nofacet](https://raw.githubusercontent.com/mbkoltai/uk_covid_datavis/master/london/cases_hosp_deaths_from_2021_10_01/london_admissions_by_age_lineplot_linear_nofacet_absnum.png)
  [linear_free_facet](https://raw.githubusercontent.com/mbkoltai/uk_covid_datavis/master/london/cases_hosp_deaths_from_2021_10_01/london_admissions_by_age_lineplot_linear_absnum.png)
  [log_nofacet](https://raw.githubusercontent.com/mbkoltai/uk_covid_datavis/master/london/cases_hosp_deaths_from_2021_10_01/london_admissions_by_age_lineplot_log_nofacet_absnum.png)
  [log_free_facet](https://raw.githubusercontent.com/mbkoltai/uk_covid_datavis/master/london/cases_hosp_deaths_from_2021_10_01/london_admissions_by_age_lineplot_log_absnum.png)
  [log_fixed_facet](https://raw.githubusercontent.com/mbkoltai/uk_covid_datavis/master/london/cases_hosp_deaths_from_2021_10_01/london_admissions_by_age_lineplot_log_yfixed_absnum.png)                                                             | from 2021/07/01 |
  | Hospital admissions               | rate per million population                | 10-year age bands                    | lin/log | fixed/free |
  [linear_nofacet](https://raw.githubusercontent.com/mbkoltai/uk_covid_datavis/master/london/cases_hosp_deaths_from_2021_10_01/london_admissions_by_age_lineplot_linear_nofacet_rate.png)
  [linear_free_facet](https://raw.githubusercontent.com/mbkoltai/uk_covid_datavis/master/london/cases_hosp_deaths_from_2021_10_01/london_admissions_by_age_lineplot_linear_rate.png)
  [log_nofacet](https://raw.githubusercontent.com/mbkoltai/uk_covid_datavis/master/london/cases_hosp_deaths_from_2021_10_01/london_admissions_by_age_lineplot_log_nofacet_rate.png)
  [log_free_facet](https://raw.githubusercontent.com/mbkoltai/uk_covid_datavis/master/london/cases_hosp_deaths_from_2021_10_01/london_admissions_by_age_lineplot_log_rate.png)
  [log_fixed_facet](https://raw.githubusercontent.com/mbkoltai/uk_covid_datavis/master/london/cases_hosp_deaths_from_2021_10_01/london_admissions_by_age_lineplot_log_yfixed_rate.png)                                                             | from 2021/07/01 |
  | Hospital admissions               | normalised to Jan/2021 peak                | 10-year age bands                    | lin/log | fixed/free |
  [linear_nofacet](https://raw.githubusercontent.com/mbkoltai/uk_covid_datavis/master/london/cases_hosp_deaths_from_2021_10_01/london_admissions_by_age_lineplot_linear_nofacet_absnum_peak_norm.png)
  [linear_free_facet](https://raw.githubusercontent.com/mbkoltai/uk_covid_datavis/master/london/cases_hosp_deaths_from_2021_10_01/london_admissions_by_age_lineplot_linear_absnum_peak_norm.png)
  [log_nofacet](https://raw.githubusercontent.com/mbkoltai/uk_covid_datavis/master/london/cases_hosp_deaths_from_2021_10_01/london_admissions_by_age_lineplot_log_nofacet_absnum_peak_norm.png)
  [log_free_facet](https://raw.githubusercontent.com/mbkoltai/uk_covid_datavis/master/london/cases_hosp_deaths_from_2021_10_01/london_admissions_by_age_lineplot_log_absnum_peak_norm.png)
  [log_fixed_facet](https://raw.githubusercontent.com/mbkoltai/uk_covid_datavis/master/london/cases_hosp_deaths_from_2021_10_01/london_admissions_by_age_lineplot_log_yfixed_absnum_peak_norm.png) | from 2021/07/01, normalised to Jan/2021 peak |
  | Deaths               | absolute number                | 10-year age bands                    | lin/log | fixed/free |
  [linear_nofacet](https://raw.githubusercontent.com/mbkoltai/uk_covid_datavis/master/london/cases_hosp_deaths_from_2021_10_01/london_deaths_by_age_lineplot_linear_nofacet_absnum.png)
  [linear_free_facet](https://raw.githubusercontent.com/mbkoltai/uk_covid_datavis/master/london/cases_hosp_deaths_from_2021_10_01/london_deaths_by_age_lineplot_linear_absnum.png)
  [log_nofacet](https://raw.githubusercontent.com/mbkoltai/uk_covid_datavis/master/london/cases_hosp_deaths_from_2021_10_01/london_deaths_by_age_lineplot_log_nofacet_absnum.png)
  [log_free_facet](https://raw.githubusercontent.com/mbkoltai/uk_covid_datavis/master/london/cases_hosp_deaths_from_2021_10_01/london_deaths_by_age_lineplot_log_absnum.png)
  [log_fixed_facet](https://raw.githubusercontent.com/mbkoltai/uk_covid_datavis/master/london/cases_hosp_deaths_from_2021_10_01/london_deaths_by_age_lineplot_log_yfixed_absnum.png)                                                             | from 2021/07/01 |
  | Deaths               | rate per million population                | 10-year age bands                    | lin/log | fixed/free |
  [linear_nofacet](https://raw.githubusercontent.com/mbkoltai/uk_covid_datavis/master/london/cases_hosp_deaths_from_2021_10_01/london_deaths_by_age_lineplot_linear_nofacet_rate.png)
  [linear_free_facet](https://raw.githubusercontent.com/mbkoltai/uk_covid_datavis/master/london/cases_hosp_deaths_from_2021_10_01/london_deaths_by_age_lineplot_linear_rate.png)
  [log_nofacet](https://raw.githubusercontent.com/mbkoltai/uk_covid_datavis/master/london/cases_hosp_deaths_from_2021_10_01/london_deaths_by_age_lineplot_log_nofacet_rate.png)
  [log_free_facet](https://raw.githubusercontent.com/mbkoltai/uk_covid_datavis/master/london/cases_hosp_deaths_from_2021_10_01/london_deaths_by_age_lineplot_log_rate.png)
  [log_fixed_facet](https://raw.githubusercontent.com/mbkoltai/uk_covid_datavis/master/london/cases_hosp_deaths_from_2021_10_01/london_deaths_by_age_lineplot_log_yfixed_rate.png)                                                             | from 2021/07/01 |
  | Deaths               | normalised to Jan/2021 peak                | 10-year age bands                    | lin/log | fixed/free |
  [linear_nofacet](https://raw.githubusercontent.com/mbkoltai/uk_covid_datavis/master/london/cases_hosp_deaths_from_2021_10_01/london_deaths_by_age_lineplot_linear_nofacet_absnum_peak_norm.png)
  [linear_free_facet](https://raw.githubusercontent.com/mbkoltai/uk_covid_datavis/master/london/cases_hosp_deaths_from_2021_10_01/london_deaths_by_age_lineplot_linear_absnum_peak_norm.png)
  [log_nofacet](https://raw.githubusercontent.com/mbkoltai/uk_covid_datavis/master/london/cases_hosp_deaths_from_2021_10_01/london_deaths_by_age_lineplot_log_nofacet_absnum_peak_norm.png)
  [log_free_facet](https://raw.githubusercontent.com/mbkoltai/uk_covid_datavis/master/london/cases_hosp_deaths_from_2021_10_01/london_deaths_by_age_lineplot_log_absnum_peak_norm.png)
  [log_fixed_facet](https://raw.githubusercontent.com/mbkoltai/uk_covid_datavis/master/london/cases_hosp_deaths_from_2021_10_01/london_deaths_by_age_lineplot_log_yfixed_absnum_peak_norm.png)  | from 2021/07/01, normalised to Jan/2021 peak |
******************
