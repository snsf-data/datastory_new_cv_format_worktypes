# Datastory: *Our new CV format: How is it used by researchers?*

*The SNSF’s new standardised CV format allows grant applicants to highlight a diverse range of work types and achievements. How is the research community using the new format in practice?*

[English](https://data.snf.ch/stories/new-cv-format-worktypes-en.html)\
[German](https://data.snf.ch/stories/neuer-cv-forschungsarbeiten-de.html)\
[French](https://data.snf.ch/stories/nouveau-cv-types-de-travaux-fr.html)

**Author(s)**: Greta Kaufeld, Julia Jerke

**Publication date**: 28.11.2024

--

# Data description

The data used in this data story are available in the folder data. There is one dataset called `works.csv`.

The dataset consists of the work types/contributions submitted by SNSF applicants with the new CV format between October 2022 to July 2024 (for more details see the data story). Here is a description of the variables:

-   `CvId`: Unique identifier of a CV (the same CV can be reused for different applications).
-   `ApplicationId`: Unique identifier for applications.
-   `PersonId`: Unique identifier of the CV owner.
-   `ResearchArea`: Research area (SSH - "Social Sciences and Humanities", MINT - "Mathematics, Informatics, Natural sciences, and Technology", LS - "Life Sciences", and ID - "Interdisciplinary") to which the CV was submitted in the context of an application.
-   `CvAchievementId`: unique identifier of the achievement to which a work type is linked (a CV can have up to three achievements).
-   `CvWorkId`: unique identifier of a single referenced work by an applicant (each CV can have up to 10 works).
-   `WorkTypeName`: type of work (based on [work types supported by ORCID](https://info.orcid.org/ufaqs/what-work-types-does-orcid-support/))
-   `WorkCategory`: category of the work type ("Publications", "Conferences", "Intellectual Property", and "Other", as supported by ORCID).
-   `PublicationSubcategory`: subcategory of the work type ("Journal", "Book", "Dissertation", "Training and Supervision", "Other type of Article", "Textual Tool", and "Preprint"; the subcategories were created for the purpose of the data story and are not supported by ORCID).
