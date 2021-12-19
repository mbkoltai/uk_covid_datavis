## England COVID-19: Vaccine rollout, cases, hospitalisations, deaths


The script [datavis.R](https://github.com/mbkoltai/uk_covid_datavis/blob/master/datavis.R) pulls data from the UK Coronavirus dashboard and creates all the plots below. For now all data are for England only.
ONS population denominators are used instead of NIMS for vaccine coverage and per population incidence rates. The ONS data used are [uploaded](https://github.com/mbkoltai/uk_covid_datavis/blob/master/ONS_2019_midpoint_population_estim_modified.csv) [to the repo](https://github.com/mbkoltai/uk_covid_datavis/blob/master/ons_all_age_groups_uk_england_2019.csv).

| Data type           | Metric                                     | Faceted/grouped by                   | Lin/log | Y-scale    | Link                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                  | comment         |
  |---------------------|--------------------------------------------|--------------------------------------|---------|------------|---------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------|-----------------|
  | Cases               | absolute number                            | 10-year age bands, grouped by 5 year | linear  | fixed      | [link](https://raw.githubusercontent.com/mbkoltai/uk_covid_datavis/master/england_cases_number_10_yr_agebands.png)                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                    |                 |
  | Cases               | absolute number                            | 10-year age bands, grouped by 5 year | log     | free       | [link](https://raw.githubusercontent.com/mbkoltai/uk_covid_datavis/master/england_cases_number_10_yr_agebands_log.png)                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                |                 |
  | Cases               | % change of weekly sum                     | 25-year age bands, grouped by 5 year | lin     | fixed      | [link](https://raw.githubusercontent.com/mbkoltai/uk_covid_datavis/master/england_cases_age_4groups_rollingsum_change.png)                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                            |                 |
  | Vaccines            | % 12y+ population per day and cumulatively | vaccine dose (1,2,3)                 | lin     | fixed      | [link](https://github.com/mbkoltai/uk_covid_datavis/raw/master/vaccine_data/vaccine_allage_phaseportrait_3rows.png)                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                   |                 |
  | Vaccines            | % vaccinated cumulatively                  | 5-yr age bands                       | lin     | fixed      | [link](https://github.com/mbkoltai/uk_covid_datavis/raw/master/vaccine_data/vaccine_by_age_cumul.png)                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                 |                 |
  | Vaccines            | % age group per day and cumulatively       | 5-yr age bands                       | lin-log | fixed      | [link](https://github.com/mbkoltai/uk_covid_datavis/raw/master/vaccine_data/vaccine_by_age_phaseportrait_both_doses_line_log.png)                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                     |                 |
  | Vaccines            | abs num doses per day per age group        | 5-yr age bands                       | lin     | fixed      | [link](https://github.com/mbkoltai/uk_covid_datavis/raw/master/vaccine_data/vaccine_by_age_rate_absnum_lin.png)                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                       |                 |
  | Vaccines            | abs num doses per day per age group        | 5-yr age bands                       | log     | fixed      | [link](https://github.com/mbkoltai/uk_covid_datavis/raw/master/vaccine_data/vaccine_by_age_rate_absnum_log.png)                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                       |                 |
  | Vaccines            | % age group per day                        | 5-yr age bands                       | lin     | fixed      | [link](https://github.com/mbkoltai/uk_covid_datavis/raw/master/vaccine_data/vaccine_by_age_rate_lin.png)                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                              |                 |
  | Vaccines            | % age group per day                        | 5-yr age bands                       | log     | fixed      | [link](https://raw.githubusercontent.com/mbkoltai/uk_covid_datavis/master/vaccine_data/vaccine_by_age_rate_log.png)                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                               |                 |
  | Cases               | rate per million population                | 10-year age bands                    | lin/log | fixed/free | [linear_nofacet](https://raw.githubusercontent.com/mbkoltai/uk_covid_datavis/master/cases_hosp_deaths_from_2021_07_01/england_cases_by_age_lineplot_linear_nofacet.png) [linear_free_facet](https://raw.githubusercontent.com/mbkoltai/uk_covid_datavis/master/cases_hosp_deaths_from_2021_07_01/england_cases_by_age_lineplot_linear.png) [linear_fixed_facet](https://raw.githubusercontent.com/mbkoltai/uk_covid_datavis/master/cases_hosp_deaths_from_2021_07_01/england_cases_by_age_lineplot_linear_yfixed.png) [log_nofacet](https://raw.githubusercontent.com/mbkoltai/uk_covid_datavis/master/cases_hosp_deaths_from_2021_07_01/england_cases_by_age_lineplot_log_nofacet.png) [log_free_facet](https://raw.githubusercontent.com/mbkoltai/uk_covid_datavis/master/cases_hosp_deaths_from_2021_07_01/england_cases_by_age_lineplot_log.png) [log_fixed_facet](https://raw.githubusercontent.com/mbkoltai/uk_covid_datavis/master/cases_hosp_deaths_from_2021_07_01/england_cases_by_age_lineplot_log_yfixed.png)                                                             | from 2021/07/01 |
  | Cases               | rate per million population                | 10-year age bands                    | lin/log | fixed/free | [linear_nofacet](https://raw.githubusercontent.com/mbkoltai/uk_covid_datavis/master/cases_hosp_deaths_from_2020_12_01/england_cases_by_age_lineplot_linear_nofacet.png) [linear_free_facet](https://raw.githubusercontent.com/mbkoltai/uk_covid_datavis/master/cases_hosp_deaths_from_2020_12_01/england_cases_by_age_lineplot_linear.png) [linear_fixed_facet](https://raw.githubusercontent.com/mbkoltai/uk_covid_datavis/master/cases_hosp_deaths_from_2020_12_01/england_cases_by_age_lineplot_linear_yfixed.png) [log_nofacet](https://raw.githubusercontent.com/mbkoltai/uk_covid_datavis/master/cases_hosp_deaths_from_2020_12_01/england_cases_by_age_lineplot_log_nofacet.png) [log_free_facet](https://raw.githubusercontent.com/mbkoltai/uk_covid_datavis/master/cases_hosp_deaths_from_2020_12_01/england_cases_by_age_lineplot_log.png) [log_fixed_facet](https://raw.githubusercontent.com/mbkoltai/uk_covid_datavis/master/cases_hosp_deaths_from_2020_12_01/england_cases_by_age_lineplot_log_yfixed.png)                                                             | from 2020/12/01 |
  | Hospital admissions | rate per million population                | 10-year age bands                    | lin/log | fixed/free | [linear_nofacet](https://raw.githubusercontent.com/mbkoltai/uk_covid_datavis/master/cases_hosp_deaths_from_2020_12_01/england_cases_by_age_lineplot_linear_nofacet.png) [linear_free_facet](https://raw.githubusercontent.com/mbkoltai/uk_covid_datavis/master/cases_hosp_deaths_from_2020_12_01/england_cases_by_age_lineplot_linear.png) [linear_fixed_facet](https://raw.githubusercontent.com/mbkoltai/uk_covid_datavis/master/cases_hosp_deaths_from_2020_12_01/england_cases_by_age_lineplot_linear_yfixed.png) [log_nofacet](https://raw.githubusercontent.com/mbkoltai/uk_covid_datavis/master/cases_hosp_deaths_from_2020_12_01/england_cases_by_age_lineplot_log_nofacet.png) [log_free_facet](https://raw.githubusercontent.com/mbkoltai/uk_covid_datavis/master/cases_hosp_deaths_from_2020_12_01/england_cases_by_age_lineplot_log.png) [log_fixed_facet](https://raw.githubusercontent.com/mbkoltai/uk_covid_datavis/master/cases_hosp_deaths_from_2020_12_01/england_cases_by_age_lineplot_log_yfixed.png)                                                             | from 2020/12/01 |
  | Hospital admissions | rate per million population                | 10-year age bands                    | lin/log | fixed/free | [linear_nofacet](https://raw.githubusercontent.com/mbkoltai/uk_covid_datavis/master/cases_hosp_deaths_from_2021_07_01/england_admissions_by_age_lineplot_linear_nofacet.png) [linear_free_facet](https://raw.githubusercontent.com/mbkoltai/uk_covid_datavis/master/cases_hosp_deaths_from_2021_07_01/england_admissions_by_age_lineplot_linear.png) [linear_fixed_facet](https://raw.githubusercontent.com/mbkoltai/uk_covid_datavis/master/cases_hosp_deaths_from_2021_07_01/england_admissions_by_age_lineplot_linear_yfixed.png) [log_nofacet](https://raw.githubusercontent.com/mbkoltai/uk_covid_datavis/master/cases_hosp_deaths_from_2021_07_01/england_admissions_by_age_lineplot_log_nofacet.png) [log_free_facet](https://raw.githubusercontent.com/mbkoltai/uk_covid_datavis/master/cases_hosp_deaths_from_2021_07_01/england_admissions_by_age_lineplot_log.png) [log_fixed_facet](https://raw.githubusercontent.com/mbkoltai/uk_covid_datavis/master/cases_hosp_deaths_from_2021_07_01/england_admissions_by_age_lineplot_log_yfixed.png) | from 2021/07/01 |
  | Deaths              | rate per million population                | 10-year age bands                    | lin/log | fixed/free | [linear_nofacet](https://raw.githubusercontent.com/mbkoltai/uk_covid_datavis/master/cases_hosp_deaths_from_2020_12_01/england_cases_by_age_lineplot_linear_nofacet.png) [linear_free_facet](https://raw.githubusercontent.com/mbkoltai/uk_covid_datavis/master/cases_hosp_deaths_from_2020_12_01/england_cases_by_age_lineplot_linear.png) [linear_fixed_facet](https://raw.githubusercontent.com/mbkoltai/uk_covid_datavis/master/cases_hosp_deaths_from_2020_12_01/england_cases_by_age_lineplot_linear_yfixed.png) [log_nofacet](https://raw.githubusercontent.com/mbkoltai/uk_covid_datavis/master/cases_hosp_deaths_from_2020_12_01/england_cases_by_age_lineplot_log_nofacet.png) [log_free_facet](https://raw.githubusercontent.com/mbkoltai/uk_covid_datavis/master/cases_hosp_deaths_from_2020_12_01/england_cases_by_age_lineplot_log.png) [log_fixed_facet](https://raw.githubusercontent.com/mbkoltai/uk_covid_datavis/master/cases_hosp_deaths_from_2020_12_01/england_cases_by_age_lineplot_log_yfixed.png)                                                             | from 2020/12/01 |
  | Deaths              | rate per million population                | 10-year age bands                    | lin/log | fixed/free | [linear_nofacet](https://raw.githubusercontent.com/mbkoltai/uk_covid_datavis/master/cases_hosp_deaths_from_2021_07_01/england_deaths_by_age_lineplot_linear_nofacet.png) [linear_free_facet](https://raw.githubusercontent.com/mbkoltai/uk_covid_datavis/master/cases_hosp_deaths_from_2021_07_01/england_deaths_by_age_lineplot_linear.png) [linear_fixed_facet](https://raw.githubusercontent.com/mbkoltai/uk_covid_datavis/master/cases_hosp_deaths_from_2021_07_01/england_deaths_by_age_lineplot_linear_yfixed.png) [log_nofacet](https://raw.githubusercontent.com/mbkoltai/uk_covid_datavis/master/cases_hosp_deaths_from_2021_07_01/england_deaths_by_age_lineplot_log_nofacet.png) [log_free_facet](https://raw.githubusercontent.com/mbkoltai/uk_covid_datavis/master/cases_hosp_deaths_from_2021_07_01/england_deaths_by_age_lineplot_log.png) [log_fixed_facet](https://raw.githubusercontent.com/mbkoltai/uk_covid_datavis/master/cases_hosp_deaths_from_2021_07_01/england_deaths_by_age_lineplot_log_yfixed.png)                                                 | from 2021/07/01 |
  | Deaths              | abs num and proportion                     | <50, 10-yr age bands above           | lin     | free       | [link](https://raw.githubusercontent.com/mbkoltai/uk_covid_datavis/master/cumul_deaths_by_age.png)                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                    |                 |

****