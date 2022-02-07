# deaths-at-home-covid19

This is the code and configuration for deaths-at-home-covid19.

This project is a **_work in progress_**, it intends to look at:
1. What services were provided to patients who died over the course of the pandemic, compared with pre-pandemic, and how did service use vary between sub-groups of patients?
2. What is the quality of data relevant to assessing the quality of end of life care within OpenSAFELY?

------------------------------------

* The paper will be available here.
* Raw model outputs, including charts, crosstabs, etc, are in `released_outputs/`
* If you are interested in how we defined our variables, take a look at the [study definition](analysis/study_definition.py); this is written in `python`, but non-programmers should be able to understand what is going on there
* If you are interested in how we defined our code lists, look in the [codelists folder](./codelists/).
* Developers and epidemiologists interested in the framework should review [the OpenSAFELY documentation](https://docs.opensafely.org)

# About the OpenSAFELY framework

The OpenSAFELY framework is a Trusted Research Environment (TRE) for electronic
health records research in the NHS, with a focus on public accountability and
research quality.

Read more at [OpenSAFELY.org](https://opensafely.org).

# Licences
As standard, research projects have a MIT license. 
