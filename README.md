# Winter sublimation at the Chhota Shigri Glacier, western Himalaya, India

This repository is the collection of the data and codes used in analysis and generating figures for the paper published in The Cryosphere Discussion (TCD):

>Mandal, A., Angchuk, T., Azam, M. F., Ramanathan, A., Wagnon, P., Soheb, M., and Singh, C.: 11-year record of wintertime snow surface energy balance and sublimation at 4863 m a.s.l. on Chhota Shigri Glacier moraine (western Himalaya, India), The Cryosphere Discuss. [preprint], https://doi.org/10.5194/tc-2021-386, in review, 2022.

Citation: [![DOI](https://zenodo.org/badge/493343517.svg)](https://zenodo.org/badge/latestdoi/493343517)


## Code
Surface energy balance (SEB) calculations were done in `R` and the codes are provided in `SEB_in_R_18 May 2022.R`. Most of the figure were generated in `R`, except few in `Python` using `Jupyter Notebook`. Several `R` and `Python` library/packages (mentioned in the respective scripts) were used to analyse the datasets and generate figures, **BIG THANKS** to those library/package authors. To smoothly reproduce the figures, the required libraries/packages should be installed in the user's `R` or `Python` environment. 

## Data
AWS-M (4863 m a.s.l.) data used in this study for 2009-2020 (Dec-Apr for each hydrological year) is provided in an excel file `/data/AWS-M_30-min_DJFMA_2009-2020.xlsx`. The weather station (AWS-M) is located in the Lahaul-Spiti District of Himachal Pradesh, India (GPS coordinates: 32.23° N, 77.51° E). Data for each figures in the paper are given in `/data` folder, with figure numbers.

## Acknowledgement / Funding
We thank Jawaharlal Nehru University, New Delhi, for providing all the facilities to carry out this work. The funding agencies and project collaborators who fully and partially supported this work are the Department of Science and Technology (Govt. of India), IFCPAR/CEFIPRA, INDICE, GLACINDIA and CHARIS, MoES, SAC-ISRO. The Pléiades image in Fig. 1 was provided under the Pléiades Glacier Observatory (PGO) initiative of the French Space Agency (CNES). AM is grateful to UGC-RGNF and DAAD Bi-nationally Supervised PhD Fellowship (Germany) for providing financial support for his PhD. MFA acknowledges the research grant from INSPIRE Faculty award (IFA-14-EAS-22) and Space Application Centre (ISRO).

## Contact
Please feel free to contact me if you have any questions or suggestions. <br/>
Arindan Mandal (arindan.141@gmail.com)
