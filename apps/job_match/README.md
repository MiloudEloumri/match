[![Contributors][contributors-shield]][contributors-url]
[![Forks][forks-shield]][forks-url]
[![Stargazers][stars-shield]][stars-url]
[![Issues][issues-shield]][issues-url]
[![MIT License][license-shield]][license-url]
[![DOCS][docs-shield]][docs-url]
[![LinkedIn][linkedin-shield]][linkedin-url]
***
**Job Match App**
=====
Job Match app uses:
1- Semantic Web ToolKit for Erlang applications to decode BTM jobs and applicants ontology in Erlang list of maps,
2- It then processes and builds jobs facts and applicants facts representing initial KB as a list of tuples accepted by SERESYE, 
1- Using Erlang SERESYE expert system, a rule engine is built operating on the jobs and applicants KB as an asserted KB, 
3- Based on the asserted KB, a rule matching is developed to match each job with its corresponding applicants,
4- Whenever there is a job-applicant match, a rule is fired and its actions are executed,
5- The actions of a rule are functions that evaluate a corresponding matching applicant qualification against job requirements based on several criteria,    
6- The evaluation results of matched facts are then added to the initial KB as inferred facts.
7- For convenience and examination, the process of data handing and their corresponding result are saved in txt files in this app */priv/data* directory.
The numbering naming in the txt files indicate the order of processing.  
***
Job Match App is built using [![Rebar3][rebar3-shield]][rebar3-url] on [![Erlang][erlang-shield]][erlang-url]
***

Build
-----

    $ rebar3 shell

***
Start Job Match app, then query the engine and test the results of matching and evaluation:

    $ job_match:start().
====================================================


    milou@HP122021 MINGW64 ~/match (main)
    $ rebar3 shell
    ===> Verifying dependencies...
    ===> Analyzing applications...
    ===> Compiling seresye
    ===> Compiling match
    ===> Compiling semantic_relatives
    ===> Compiling job_match
    ===> Analyzing applications...
    ===> Compiling extra_apps/seresye/examples
    Eshell V11.0  (abort with ^G)
    1> ===> Booted match
    1> ===> Booted semantic
    1> ===> Booted cf
    1> ===> Booted erlware_commons
    1> ===> Booted seresye
    1> ===> Booted job_match
    1> ===> Booted semantic_relatives
    1> ===> Booted sasl
    1>

    1> job_match:start().
    ok
    2>

Following the call to function *job_match:start/0*, the engine is created and populated; if no errors occurred, the
rules should have been processed and the new facts derived. To check this, we can use the function *seresye_srv:
get_kb/1*, which returns the list of all facts in the knowledge base of a given engine: *seresye_srv:get_kb(job_match).*

2> seresye_srv:get_kb(job_match).
[{match,<<"JPosting22">>,<<"JSeeker126">>,
        {<<"DataScientistJuniorDataScienceAnalytics">>,
         <<"MohamadLucero">>,
         {score,51.11},
         {stars,3.0},
         {education,8,60.0},
         {experience,8.0,13.33},
         {skills,1.6,80.0}}},
 {applicant,<<"JPosting22">>,
            {<<"JSeeker126">>,
             {<<"MohamadLucero">>,
              {education,<<"Bachelor">>,<<"Computing">>},
              {experience,2,
                          <<"MethodologiesAndTechniquesUsedAsAJuniorDataScientist">>,
                          <<"DataAnalysisExperience">>},
              {techskills,<<"KnowledgeAndExperienceOfLargeComplexDataAnalyticsOrIntelligenceP"...>>,
                          <<"StatisticalAndPatternRecognitionSkills">>},
              {softskills,<<"Numeracy">>,<<"ProblemSolving">>}}}},
 {match,<<"JPosting11">>,<<"JSeeker63">>,
        {<<"DigitalSecurityAuditor">>,<<"ImaanKendall">>,
         {score,69.57},
         {stars,3.5},
         {education,5,50.0},
         {experience,34.0,58.72},
         {skills,2.0,100.0}}},
 {...}|...]
3>

The presence of facts representing *match* tuples with their evaluation, proves that the rules seems to be working as
expected.

We can however query the knowledge base using specific fact templates. 

For example, if we want to know the match between *JPosting1* and *JSeeker1* if any and its corresponding evaluation, 
we can use the function *seresye_srv:query_kb/2* as follows:
*seresye_srv:query_kb(job_match, {match, <<"JPosting1">>, <<"JSeeker1">> , '_'}).*

3> seresye_srv:query_kb(job_match, {match, <<"JPosting1">>, <<"JSeeker1">> , '_'}).
[{match,<<"JPosting1">>,<<"JSeeker1">>,
        {<<"DigitalSecurityManagerOfficer">>,<<"AustinLeigh">>,
         {score,64.81},
         {stars,3.5},
         {education,7,35.71},
         {experience,34.0,58.72},
         {skills,2.0,100.0}}}]
4>

The facts returned conform to the actual BTM Jobs Ontology data match between *JPosting1* and *JSeeker1* and   
the result of evaluating *JSeeker1* based on the implemented evaluation criteria , thus proving that the rules written are
really working.

As the example shows, function *seresye_srv:query_kb/2* takes the engine name *job_match* as the first argument, while, for the
second parameter, a tuple has to be specified, representing the fact template to be matched; in such a tuple, the
atom *'_'* plays the role of a wildcard. However, to specify a more complex matching, a *fun* can be used as a tuple
element; this *fun* has to return a boolean value which indicates if the element matches the template. For example, to
select both *JSeeker1* and *JSeeker2* evaluation in *JPosting1*, we can use the following function call:
*seresye_srv:query_kb(job_match, {match, <<"JPosting1">>, fun (X) -> (X == <<"JSeeker1">>) or (X == <<"JSeeker3">>) end, '_'}).*


4> seresye_srv:query_kb(job_match, {match, <<"JPosting1">>, fun (X) -> (X == <<"JSeeker1">>) or (X == <<"JSeeker3">>) end, '_'}).
[{match,<<"JPosting1">>,<<"JSeeker3">>,
        {<<"DigitalSecurityManagerOfficer">>,<<"SteffanBeattie">>,
         {score,36.72},
         {stars,2.0},
         {education,10,57.14},
         {experience,20.0,33.03},
         {skills,0.4,20.0}}},
 {match,<<"JPosting1">>,<<"JSeeker1">>,
        {<<"DigitalSecurityManagerOfficer">>,<<"AustinLeigh">>,
         {score,64.81},
         {stars,3.5},
         {education,7,35.71},
         {experience,34.0,58.72},
         {skills,2.0,100.0}}}]
5>

Querying unmatched applicant, should give an empty list. For example:  

5> seresye_srv:query_kb(job_match, {match, <<"JPosting2">>, <<"JSeeker1">> , '_'}).
[]
6>

Since *JPosting1* has no associated applicant with ID *JSeeker1*, the result is empty list.

More complicated *fun* queries can be used. For example, the following checks for *JPosting1* applicants with a score greater than 50 resulting in one applicant *JSeeker1* meeting the query:       

6> seresye_srv:query_kb(job_match,
    {match, <<"JPosting1">>, '_',
        fun({_JobTitle, _ApplicantName, {score, Score}, _, _, _, _}) ->
            Score > 50
        end
    }
).
[{match,<<"JPosting1">>,<<"JSeeker1">>,
        {<<"DigitalSecurityManagerOfficer">>,<<"AustinLeigh">>,
         {score,64.81},
         {stars,3.5},
         {education,7,35.71},
         {experience,34.0,58.72},
         {skills,2.0,100.0}}}]
7>

Similarly,  we can use *fun* for querying any other type of data patters. For instance, the following checks for *JPosting1* applicants with star rating grater than 4 resulting in empty list, which indicates that no single applicant of *JPosting1* has star rating greater than 4:  

7> seresye_srv:query_kb(job_match,
    {match, <<"JPosting1">>, '_',
        fun({_JobTitle, _ApplicantName, _, {stars, Stars}, _, _, _}) ->
            Stars > 4
        end
    }
).
[]
8>

Furthermore,  we can query all jobs for applicants with star rating grater than 4: 


8> seresye_srv:query_kb(job_match,
    {match, '_', '_',
        fun({_JobTitle, _ApplicantName, _, {stars, Stars}, _, _, _}) ->
            Stars > 4
        end
    }
).
[{match,<<"JPosting19">>,<<"JSeeker107">>,
        {<<"FinancialServicesITGovernanceRiskAndComplianceManager">>,
         <<"LauraObrien">>,
         {score,86.67},
         {stars,4.5},
         {education,8,60.0},
         {experience,49.0,100.0},
         {skills,2.0,100.0}}},
 {match,<<"JPosting9">>,<<"JSeeker50">>,
        {<<"FinancialServicesEnterpriseArchitect">>,<<"GuyBenson">>,
         {score,86.67},
         {stars,4.5},
         {education,8,60.0},
         {experience,49.0,100.0},
         {skills,2.0,100.0}}},
 {match,<<"JPosting14">>,<<"JSeeker84">>,
        {<<"FinancialServicesCloudServicesManager">>,
         <<"JameelNielsen">>,
         {score,86.67},
         {stars,4.5},
         {education,8,60.0},
         {experience,49.0,100.0},
         {skills,2.0,100.0}}},
 {match,<<"JPosting9">>,<<"JSeeker46">>,
        {<<"FinancialServicesEnterpriseArchitect">>,
         <<"JoyceSheldon">>,
         {score,86.67},
         {stars,4.5},
         {education,8,60.0},
         {experience,49.0,100.0},
         {skills,2.0,100.0}}},
 {match,<<"JPosting14">>,<<"JSeeker81">>,
        {<<"FinancialServicesCloudServicesManager">>,
         <<"TabathaBarnett">>,
         {score,86.67},
         {stars,4.5},
         {education,8,60.0},
         {experience,49.0,100.0},
         {skills,2.0,100.0}}},
 {match,<<"JPosting19">>,<<"JSeeker105">>,
        {<<"FinancialServicesITGovernanceRiskAndComplianceManager">>,
         <<"SashaWooten">>,
         {score,86.67},
         {stars,4.5},
         {education,8,60.0},
         {experience,49.0,100.0},
         {skills,2.0,100.0}}},
 {match,<<"JPosting14">>,<<"JSeeker79">>,
        {<<"FinancialServicesCloudServicesManager">>,
         <<"ShauryaMack">>,
         {score,86.67},
         {stars,4.5},
         {education,8,60.0},
         {experience,49.0,100.0},
         {skills,2.0,100.0}}},
 {match,<<"JPosting14">>,<<"JSeeker82">>,
        {<<"FinancialServicesCloudServicesManager">>,
         <<"FergusOSullivan">>,
         {score,86.67},
         {stars,4.5},
         {education,8,60.0},
         {experience,49.0,100.0},
         {skills,2.0,100.0}}}]
9>

***
Conclusions
-----------

This shows the versatility of using and integrating Erlang technologies to process data source stored in semantic web formats and build a rule engine that derive new facts.  
The characteristics of functional and symbolic programming, together with the possibility of performing *introspection*
of function declaration, can be successfully exploited for application domains which are completely new for Erlang but
can surely be very interesting.

<!-- MARKDOWN LINKS & IMAGES -->
<!-- https://www.markdownguide.org/basic-syntax/#reference-style-links -->
[contributors-shield]: https://img.shields.io/github/contributors/MiloudEloumri/seresye.svg?style=for-the-badge
[contributors-url]: https://github.com/MiloudEloumri/seresye/graphs/contributors
[forks-shield]: https://img.shields.io/github/forks/MiloudEloumri/seresye.svg?style=for-the-badge
[forks-url]: https://github.com/MiloudEloumri/seresye/network/members
[stars-shield]: https://img.shields.io/github/stars/MiloudEloumri/seresye.svg?style=for-the-badge
[stars-url]: https://github.com/MiloudEloumri/seresye/stargazers
[issues-shield]: https://img.shields.io/github/issues/MiloudEloumri/seresye.svg?style=for-the-badge
[issues-url]: https://github.com/MiloudEloumri/seresye/issues
[license-shield]: https://img.shields.io/github/license/MiloudEloumri/seresye.svg?style=for-the-badge
[license-url]: https://github.com/MiloudEloumri/seresye/blob/master/LICENSE.md
[linkedin-shield]: https://img.shields.io/badge/-LinkedIn-black.svg?style=for-the-badge&logo=linkedin&colorB=555
[linkedin-url]: https://www.linkedin.com/in/miloudeloumri/
[rebar3-shield]: https://img.shields.io/badge/Rebar3-3.17.0-blue?labelColor=A90533&style=plastic
[rebar3-url]: https://github.com/erlang/rebar3
[erlang-shield]: https://img.shields.io/badge/Erlang|OTP_23-Erts_11.0-blue?labelColor=A90533&style=plastic&logo=Erlang
[erlang-url]: http://www.erlang.org
[docs-shield]: https://img.shields.io/static/v1?label=Docs&message=Read&&Color=8CA1AF&logo=Read-the-Docs&style=for-the-badge
[docs-url]: https://github.com/MiloudEloumri/seresye/tree/master/doc
